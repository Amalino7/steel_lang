use crate::parser::Parser;
use crate::parser::ast::{ImportSegment, ImportType, Stmt};
use crate::resolver::error::ResolverError;
use crate::resolver::{Exports, FileId, ModuleGraph, ModuleId, ModuleInfo};
use crate::scanner::Scanner;
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

pub struct ModuleResolver {
    pub visiting: HashSet<PathBuf>,
    pub visiting_stack: Vec<(String, PathBuf)>,
    pub next_file_id: u32,
}

impl Default for ModuleResolver {
    fn default() -> Self {
        Self::new()
    }
}
impl ModuleResolver {
    pub fn new() -> Self {
        Self {
            visiting: HashSet::new(),
            visiting_stack: Vec::new(),
            next_file_id: 1,
        }
    }

    pub fn resolve(&mut self, entry_file: &Path) -> Result<ModuleGraph, ResolverError> {
        let canonical_entry = entry_file.canonicalize().map_err(|err| ResolverError::Io {
            path: entry_file.to_path_buf(),
            error: err,
        })?;
        let root = canonical_entry.parent().unwrap_or_else(|| Path::new("."));

        let entry_module_name = canonical_entry
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("main")
            .to_string();

        let mut graph = ModuleGraph::new();
        self.visit_file(root, entry_module_name, &mut graph)?;
        Ok(graph)
    }

    fn visit_file(
        &mut self,
        root: &Path,
        name: String,
        graph: &mut ModuleGraph,
    ) -> Result<ModuleId, ResolverError> {
        if let Some(id) = graph.name_to_id_map.get(&name) {
            return Ok(*id);
        }

        let path = Self::resolve_module_to_path(root, &name)?;

        if self.visiting.contains(&path) {
            let mut cycle = Vec::new();
            if let Some(pos) = self.visiting_stack.iter().position(|(_, p)| p == &path) {
                for (mod_name, _) in &self.visiting_stack[pos..] {
                    cycle.push(mod_name.clone());
                }
            }
            cycle.push(name);
            return Err(ResolverError::CyclicDependency { cycle });
        }

        self.visiting.insert(path.clone());
        self.visiting_stack.push((name.clone(), path.clone()));

        let file_id = self.next_id();

        let res = self.handle_file(root, graph, &path, &name, file_id);

        self.visiting.remove(&path);
        self.visiting_stack.pop();

        let (dep_ids, source) = res?;

        let module_id = ModuleId(graph.modules.len() as u32);

        graph.name_to_id_map.insert(name.clone(), module_id);
        graph.file_to_module_id.insert(file_id, module_id);

        // Guarantees modules are already topologically sorted
        graph.modules.push(ModuleInfo {
            id: module_id,
            file_id,
            name,
            path,
            source,
            dependencies: dep_ids,
            exports: Exports::new(),
        });

        Ok(module_id)
    }

    fn next_id(&mut self) -> FileId {
        let id = FileId(self.next_file_id);
        self.next_file_id += 1;
        id
    }

    fn resolve_module_to_path(root: &Path, name: &str) -> Result<PathBuf, ResolverError> {
        let mut path = Self::name_to_path(root, name);
        path.set_extension("steel");

        if !path.exists() {
            let mut index_path = Self::name_to_path(root, name);
            index_path.push("index.steel");
            if !index_path.exists() {
                return Err(ResolverError::ModuleNotFound {
                    module_name: name.to_string(),
                    searched_paths: vec![path, index_path],
                });
            }
            path = index_path;
        }

        path.canonicalize().map_err(|err| ResolverError::Io {
            path: path.clone(),
            error: err,
        })
    }

    fn name_to_path(root: &Path, name: &str) -> PathBuf {
        name.split('.').fold(PathBuf::from(root), |mut acc, e| {
            acc.push(e);
            acc
        })
    }

    fn handle_file(
        &mut self,
        root: &Path,
        graph: &mut ModuleGraph,
        path: &Path,
        module_name: &str,
        file_id: FileId,
    ) -> Result<(Vec<ModuleId>, String), ResolverError> {
        let source = fs::read_to_string(path).map_err(|err| ResolverError::Io {
            path: path.to_path_buf(),
            error: err,
        })?;

        let scanner = Scanner::new(&source, file_id.0);
        let mut parser = Parser::new(scanner);
        let ast = parser.parse().map_err(|errors| ResolverError::ParseError {
            module_name: module_name.to_string(),
            path: path.to_path_buf(),
            errors: errors.into_iter().map(|e| e.to_string()).collect(),
        })?;

        let deps = self.get_dependencies(&ast);

        let dep_ids = deps
            .into_iter()
            .map(|dep| self.visit_file(root, dep, graph))
            .collect::<Result<Vec<_>, _>>()?;

        Ok((dep_ids, source))
    }

    fn get_dependencies(&mut self, ast: &[Stmt]) -> Vec<String> {
        let mut deps: Vec<String> = vec![];
        for stmt in ast {
            if let Stmt::Import(import) = stmt {
                Self::handle_import(None, &import.segment, &mut deps);
            }
        }
        deps
    }

    fn handle_import(prefix: Option<&str>, import: &ImportSegment, deps: &mut Vec<String>) {
        let mut path_segments = import.path.iter().map(|p| p.lexeme).collect::<Vec<_>>();

        if let Some(prefix) = prefix {
            path_segments.insert(0, prefix);
        }

        let path = path_segments.join(".");

        if let ImportType::Group { options } = &import.import_type {
            for option in options {
                Self::handle_import(Some(&path), option, deps);
            }
        } else if !path.is_empty() {
            deps.push(path);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::{self, File};
    use std::io::Write;
    use std::path::{Path, PathBuf};
    use tempfile::TempDir;

    struct TestProject {
        pub temp_dir: TempDir,
    }

    impl TestProject {
        fn new() -> Self {
            Self {
                temp_dir: TempDir::new().expect("failed to create temporary test directory"),
            }
        }

        fn root(&self) -> &Path {
            self.temp_dir.path()
        }

        fn write_file(&self, relative_path: &str, content: &str) -> PathBuf {
            let full_path = self.root().join(relative_path);
            if let Some(parent) = full_path.parent() {
                fs::create_dir_all(parent).expect("failed to create parent directories");
            }
            let mut file = File::create(&full_path).expect("failed to create file");
            file.write_all(content.as_bytes())
                .expect("failed to write content");
            full_path
        }
    }

    #[test]
    fn test_resolution_order_linear_chain() {
        // Dependency chain: a -> b -> c
        // Expected compilation/graph order: c, then b, then a
        let project = TestProject::new();
        project.write_file("c.steel", "// leaf");
        project.write_file("b.steel", "import c/Item;");
        project.write_file("a.steel", "import b/Item;");

        let mut resolver = ModuleResolver::new();
        let mut graph = ModuleGraph::new();
        resolver
            .visit_file(project.root(), "a".to_string(), &mut graph)
            .unwrap();

        let resolved_names: Vec<&str> = graph.modules.iter().map(|m| m.name.as_str()).collect();
        assert_eq!(resolved_names, vec!["c", "b", "a"]);
    }

    #[test]
    fn test_resolution_order_diamond_dependency() {
        //     app
        //    /   \
        //   b     c
        //    \   /
        //      d
        // Verifies:
        // 1. `d` is resolved before `b` and `c`
        // 2. `d` is processed only ONCE (no duplicate in graph.modules)
        // 3. No false-positive cycle error triggered when visiting `d` via `c`
        let project = TestProject::new();
        project.write_file("d.steel", "// base utility");
        project.write_file("b.steel", "import d/BaseItem;");
        project.write_file("c.steel", "import d/BaseItem;");
        project.write_file("app.steel", "import b/BItem;\nimport c/CItem;");

        let mut resolver = ModuleResolver::new();
        let mut graph = ModuleGraph::new();
        resolver
            .visit_file(project.root(), "app".to_string(), &mut graph)
            .unwrap();

        assert_eq!(
            graph.modules.len(),
            4,
            "Every module must only be added once"
        );

        let d_idx = graph.modules.iter().position(|m| m.name == "d").unwrap();
        let b_idx = graph.modules.iter().position(|m| m.name == "b").unwrap();
        let c_idx = graph.modules.iter().position(|m| m.name == "c").unwrap();
        let app_idx = graph.modules.iter().position(|m| m.name == "app").unwrap();

        assert!(d_idx < b_idx, "d must resolve before b");
        assert!(d_idx < c_idx, "d must resolve before c");
        assert!(b_idx < app_idx, "b must resolve before app");
        assert!(c_idx < app_idx, "c must resolve before app");
    }

    #[test]
    fn test_grouped_and_nested_resolution_order() {
        // Tests import foo/{bar/ItemA, baz/ItemB};
        let project = TestProject::new();
        project.write_file("core/math.steel", "");
        project.write_file("core/string.steel", "");
        project.write_file("main.steel", "import core/{math/Sin, string/Format};");

        let mut resolver = ModuleResolver::new();
        let mut graph = ModuleGraph::new();
        resolver
            .visit_file(project.root(), "main".to_string(), &mut graph)
            .unwrap();

        let math_idx = graph
            .modules
            .iter()
            .position(|m| m.name == "core.math")
            .unwrap();
        let string_idx = graph
            .modules
            .iter()
            .position(|m| m.name == "core.string")
            .unwrap();
        let main_idx = graph.modules.iter().position(|m| m.name == "main").unwrap();

        assert!(math_idx < main_idx);
        assert!(string_idx < main_idx);
    }

    #[test]
    fn test_glob_and_aliased_import_order() {
        // Tests `import x/y/*;` and `import x/y/Item as Name;`
        let project = TestProject::new();
        project.write_file("parser.steel", "");
        project.write_file("lexer.steel", "");
        project.write_file(
            "main.steel",
            "import parser/{*};\nimport lexer/Token as LexToken;",
        );

        let mut resolver = ModuleResolver::new();
        let mut graph = ModuleGraph::new();
        resolver
            .visit_file(project.root(), "main".to_string(), &mut graph)
            .unwrap();

        let parser_idx = graph
            .modules
            .iter()
            .position(|m| m.name == "parser")
            .unwrap();
        let lexer_idx = graph
            .modules
            .iter()
            .position(|m| m.name == "lexer")
            .unwrap();
        let main_idx = graph.modules.iter().position(|m| m.name == "main").unwrap();

        assert!(parser_idx < main_idx);
        assert!(lexer_idx < main_idx);
    }

    #[test]
    fn test_directory_module_with_index_steel() {
        // Tests module path "math" where there is NO "math.steel", but "math/index.steel" exists.
        let project = TestProject::new();
        project.write_file("math/index.steel", "// math module root");
        project.write_file("main.steel", "import math/Add;");

        let mut resolver = ModuleResolver::new();
        let mut graph = ModuleGraph::new();

        resolver
            .visit_file(project.root(), "main".to_string(), &mut graph)
            .unwrap();

        let math_mod = graph
            .modules
            .iter()
            .find(|m| m.name == "math")
            .expect("math module should be resolved");
        assert!(
            math_mod
                .path
                .ends_with(Path::new("math").join("index.steel")),
            "Expected path to be math/index.steel, but got: {:?}",
            math_mod.path
        );
    }

    #[test]
    fn test_direct_cycle_detection() {
        // a -> b -> a
        let project = TestProject::new();
        project.write_file("a.steel", "import b/Item;");
        project.write_file("b.steel", "import a/Item;");

        let mut resolver = ModuleResolver::new();
        let mut graph = ModuleGraph::new();
        let result = resolver.visit_file(project.root(), "a".to_string(), &mut graph);
        assert!(result.is_err());
        match result.unwrap_err() {
            ResolverError::CyclicDependency { cycle } => {
                assert_eq!(cycle, vec!["a", "b", "a"]);
            }
            err => panic!("Expected CyclicDependency error, got: {:?}", err),
        }
    }

    #[test]
    fn test_transitive_cycle_detection() {
        // a -> b -> c -> a
        let project = TestProject::new();
        project.write_file("a.steel", "import b/Item;");
        project.write_file("b.steel", "import c/Item;");
        project.write_file("c.steel", "import a/Item;");

        let mut resolver = ModuleResolver::new();
        let mut graph = ModuleGraph::new();
        let result = resolver.visit_file(project.root(), "a".to_string(), &mut graph);
        assert!(result.is_err());
        match result.unwrap_err() {
            ResolverError::CyclicDependency { cycle } => {
                assert_eq!(cycle, vec!["a", "b", "c", "a"]);
            }
            err => panic!("Expected CyclicDependency error, got: {:?}", err),
        }
    }

    #[test]
    fn test_self_cycle_detection() {
        // a -> a
        let project = TestProject::new();
        project.write_file("a.steel", "import a/Item;");

        let mut resolver = ModuleResolver::new();
        let mut graph = ModuleGraph::new();
        let result = resolver.visit_file(project.root(), "a".to_string(), &mut graph);
        assert!(result.is_err());
        match result.unwrap_err() {
            ResolverError::CyclicDependency { cycle } => {
                assert_eq!(cycle, vec!["a", "a"]);
            }
            err => panic!("Expected CyclicDependency error, got: {:?}", err),
        }
    }

    #[test]
    fn test_module_not_found() {
        let project = TestProject::new();
        project.write_file("main.steel", "import non_existent/Item;");

        let mut resolver = ModuleResolver::new();
        let mut graph = ModuleGraph::new();
        let result = resolver.visit_file(project.root(), "main".to_string(), &mut graph);
        assert!(result.is_err());
        match result.unwrap_err() {
            ResolverError::ModuleNotFound {
                module_name,
                searched_paths,
            } => {
                assert_eq!(module_name, "non_existent");
                assert_eq!(searched_paths.len(), 2);
            }
            err => panic!("Expected ModuleNotFound error, got: {:?}", err),
        }
    }

    #[test]
    fn test_entry_file_resolve() {
        let project = TestProject::new();
        let entry = project.write_file("main.steel", "import helper/Helper;");
        project.write_file("helper.steel", "// helper");

        let mut resolver = ModuleResolver::new();
        let graph = resolver.resolve(&entry).unwrap();
        assert_eq!(graph.modules.len(), 2);
        assert_eq!(graph.modules[0].name, "helper");
        assert_eq!(graph.modules[1].name, "main");
    }

    #[test]
    fn test_entry_file_not_found() {
        let mut resolver = ModuleResolver::new();
        let result = resolver.resolve(Path::new("definitely_non_existent_file_12345.steel"));
        assert!(result.is_err());
        match result.unwrap_err() {
            ResolverError::Io { path, .. } => {
                assert_eq!(
                    path,
                    PathBuf::from("definitely_non_existent_file_12345.steel")
                );
            }
            err => panic!("Expected Io error, got: {:?}", err),
        }
    }

    #[test]
    fn test_parse_error_in_imported_module() {
        let project = TestProject::new();
        project.write_file("main.steel", "import broken/Item;");
        project.write_file("broken.steel", "let = +;");

        let mut resolver = ModuleResolver::new();
        let mut graph = ModuleGraph::new();
        let result = resolver.visit_file(project.root(), "main".to_string(), &mut graph);
        assert!(result.is_err());
        match result.unwrap_err() {
            ResolverError::ParseError {
                module_name,
                errors,
                ..
            } => {
                assert_eq!(module_name, "broken");
                assert!(!errors.is_empty());
            }
            err => panic!("Expected ParseError, got: {:?}", err),
        }
    }

    #[test]
    fn test_error_display_formatting() {
        let io_err = ResolverError::io(
            PathBuf::from("some/path.steel"),
            std::io::Error::new(std::io::ErrorKind::NotFound, "file not found"),
        );
        assert!(io_err.to_string().contains("some/path.steel"));
        assert!(io_err.to_string().contains("file not found"));

        let not_found_err = ResolverError::module_not_found(
            "my_module",
            vec![
                PathBuf::from("my_module.steel"),
                PathBuf::from("my_module/index.steel"),
            ],
        );
        assert!(not_found_err.to_string().contains("my_module"));
        assert!(not_found_err.to_string().contains("my_module.steel"));

        let cycle_err = ResolverError::cyclic_dependency(vec![
            "a".to_string(),
            "b".to_string(),
            "a".to_string(),
        ]);
        assert_eq!(
            cycle_err.to_string(),
            "Cyclic dependency detected: a -> b -> a"
        );

        let parse_err = ResolverError::parse_error(
            "bad_mod",
            PathBuf::from("bad_mod.steel"),
            vec!["Unexpected token".to_string()],
        );
        assert!(parse_err.to_string().contains("bad_mod"));
        assert!(parse_err.to_string().contains("Unexpected token"));
    }
}
