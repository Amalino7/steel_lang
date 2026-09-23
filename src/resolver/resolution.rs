use crate::parser::Parser;
use crate::parser::ast::{ImportSegment, ImportType, Stmt};
use crate::resolver::error::ResolverError;
use crate::resolver::{Exports, FileId, ModuleGraph, ModuleId, ModuleInfo};
use crate::scanner::Scanner;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::mem::take;
use std::path::{Path, PathBuf};

pub struct ModuleResolver {
    pub visiting: HashSet<String>,
    pub visiting_stack: Vec<String>,
    pub errors: Vec<ResolverError>,
    pub next_file_id: u32,
}

pub enum Source<'a> {
    EntryFile(&'a Path),
    File {
        name: &'a str,
        source: &'a str,
    },
    SourceMap {
        entry: &'a str,
        map: HashMap<String, &'static str>,
    },
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
            errors: Vec::new(),
            next_file_id: 1,
        }
    }

    pub fn resolve_source(&mut self, src: &Source) -> Result<ModuleGraph, Vec<ResolverError>> {
        let graph = match src {
            Source::EntryFile(file) => self.resolve(file)?,
            Source::File { name, source } => self.resolve_file(name.to_string(), source),
            Source::SourceMap { entry, map } => self.resolve_mock(entry.to_string(), map),
        };
        if !self.errors.is_empty() {
            return Err(take(&mut self.errors));
        }
        Ok(graph)
    }

    pub fn resolve(&mut self, entry_file: &Path) -> Result<ModuleGraph, Vec<ResolverError>> {
        let canonical_entry = entry_file
            .canonicalize()
            .map_err(|err| ResolverError::Io {
                path: entry_file.to_path_buf(),
                error: err,
            })
            .map_err(|err| {
                self.errors.push(err);
                take(&mut self.errors)
            })?;

        let root = canonical_entry.parent().unwrap_or_else(|| Path::new("."));

        let entry_module_name = canonical_entry
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("main")
            .to_string();

        let mut graph = ModuleGraph::new();
        self.visit_file(entry_module_name, &mut graph, &|name| {
            let path = Self::resolve_module_to_path(root, name)?;
            let source = fs::read_to_string(&path).map_err(|err| ResolverError::Io {
                path: path.to_path_buf(),
                error: err,
            })?;
            Ok((source, path))
        })
        .map_err(|err| {
            self.errors.push(err);
            take(&mut self.errors)
        })?;

        if !self.errors.is_empty() {
            return Err(take(&mut self.errors));
        }

        Ok(graph)
    }

    pub fn resolve_mock<'src>(
        &mut self,
        entry: String,
        source_map: &HashMap<String, &'src str>,
    ) -> ModuleGraph {
        let mut graph = ModuleGraph::new();
        let res = self.visit_file(entry, &mut graph, &|name| {
            Ok((
                source_map
                    .get(name)
                    .ok_or(ResolverError::ModuleNotFound {
                        module_name: name.to_string(),
                        searched_paths: vec![],
                    })?
                    .to_string(),
                format!("{}.steel", name).into(),
            ))
        });
        if let Err(err) = res {
            self.errors.push(err);
        }
        graph
    }

    pub fn resolve_file(&mut self, name: String, source: &str) -> ModuleGraph {
        let mut graph = ModuleGraph::new();
        graph.name_to_id_map.insert(name.clone(), ModuleId(0));
        graph.file_to_module_id.insert(FileId(1), ModuleId(0));
        graph.modules.push(ModuleInfo {
            id: ModuleId(0),
            file_id: FileId(1),
            path: name.clone().into(),
            name,
            source: source.to_string(),
            dependencies: vec![],
            exports: Default::default(),
        });
        graph
    }

    fn visit_file(
        &mut self,
        name: String,
        graph: &mut ModuleGraph,
        module_to_source: &impl Fn(&str) -> Result<(String, PathBuf), ResolverError>,
    ) -> Result<ModuleId, ResolverError> {
        if let Some(id) = graph.name_to_id_map.get(&name) {
            return Ok(*id);
        }

        if self.visiting.contains(&name) {
            let mut cycle = Vec::new();
            if let Some(pos) = self.visiting_stack.iter().position(|n| n == &name) {
                for mod_name in &self.visiting_stack[pos..] {
                    cycle.push(mod_name.clone());
                }
            }
            cycle.push(name);
            return Err(ResolverError::CyclicDependency { cycle });
        }

        self.visiting.insert(name.clone());
        self.visiting_stack.push(name.clone());

        let file_id = self.next_id();

        let res = module_to_source(&name).and_then(|(source, path)| {
            self.handle_file(graph, source, &name, file_id, module_to_source)
                .map(|(deps, source)| (path, deps, source))
        });

        self.visiting.remove(&name);
        self.visiting_stack.pop();

        let (path, dep_ids, source) = match res {
            Ok(res) => res,
            Err(err) => {
                self.errors.push(err);
                (
                    Self::name_to_path(&PathBuf::new(), &name),
                    vec![],
                    String::new(),
                )
            }
        };

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
        graph: &mut ModuleGraph,
        source: String,
        module_name: &str,
        file_id: FileId,
        module_to_source: &impl Fn(&str) -> Result<(String, PathBuf), ResolverError>,
    ) -> Result<(Vec<ModuleId>, String), ResolverError> {
        let scanner = Scanner::new(&source, file_id.0);
        let mut parser = Parser::new(scanner);
        let ast = parser.parse().map_err(|errors| ResolverError::ParseError {
            module_name: module_name.to_string(),
            errors: errors.into_iter().map(|e| e.to_string()).collect(),
        })?;

        let deps = self.get_dependencies(&ast);

        let dep_ids = deps
            .into_iter()
            .filter_map(|dep| match self.visit_file(dep, graph, module_to_source) {
                Ok(ok) => Some(ok),
                Err(err) => {
                    self.errors.push(err);
                    None
                }
            })
            .collect::<Vec<_>>();

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
        let root = project.write_file("a.steel", "import b/Item;");

        let mut resolver = ModuleResolver::new();
        let graph = resolver.resolve(&root).unwrap();

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
        let root = project.write_file("app.steel", "import b/BItem;\nimport c/CItem;");

        let mut resolver = ModuleResolver::new();
        let graph = resolver.resolve(&root).unwrap();

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
        let root = project.write_file("main.steel", "import core/{math/Sin, string/Format};");

        let mut resolver = ModuleResolver::new();
        let graph = resolver.resolve(&root).unwrap();

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
        // Tests `import x/y/*; ` and `import x/y/Item as Name; `
        let project = TestProject::new();
        project.write_file("parser.steel", "");
        project.write_file("lexer.steel", "");
        let root = project.write_file(
            "main.steel",
            "import parser/{*};\nimport lexer/Token as LexToken;",
        );

        let mut resolver = ModuleResolver::new();
        let graph = resolver.resolve(&root).unwrap();

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
        let root = project.write_file("main.steel", "import math/Add;");

        let mut resolver = ModuleResolver::new();
        let graph = resolver.resolve(&root).unwrap();

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
        let root = project.write_file("a.steel", "import b/Item;");
        project.write_file("b.steel", "import a/Item;");

        let mut resolver = ModuleResolver::new();
        let result = resolver.resolve(&root);
        assert!(result.is_err());
        match result.unwrap_err().first().unwrap() {
            ResolverError::CyclicDependency { cycle } => {
                assert_eq!(cycle, &["a", "b", "a"]);
            }
            err => panic!("Expected CyclicDependency error, got: {:?}", err),
        }
    }

    #[test]
    fn test_transitive_cycle_detection() {
        // a -> b -> c -> a
        let project = TestProject::new();
        let root = project.write_file("a.steel", "import b/Item;");
        project.write_file("b.steel", "import c/Item;");
        project.write_file("c.steel", "import a/Item;");

        let mut resolver = ModuleResolver::new();

        let result = resolver.resolve(&root);
        assert!(result.is_err());
        match result.unwrap_err().first().unwrap() {
            ResolverError::CyclicDependency { cycle } => {
                assert_eq!(cycle, &["a", "b", "c", "a"]);
            }
            err => panic!("Expected CyclicDependency error, got: {:?}", err),
        }
    }

    #[test]
    fn test_self_cycle_detection() {
        // a -> a
        let project = TestProject::new();
        let root = project.write_file("a.steel", "import a/Item;");

        let mut resolver = ModuleResolver::new();
        let result = resolver.resolve(&root);
        assert!(result.is_err());
        match result.unwrap_err().first().unwrap() {
            ResolverError::CyclicDependency { cycle } => {
                assert_eq!(cycle, &["a", "a"]);
            }
            err => panic!("Expected CyclicDependency error, got: {:?}", err),
        }
    }

    #[test]
    fn test_module_not_found() {
        let project = TestProject::new();
        let root = project.write_file("main.steel", "import non_existent/Item;");

        let mut resolver = ModuleResolver::new();
        let result = resolver.resolve(&root);
        assert!(result.is_err());
        match result.unwrap_err().first().unwrap() {
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
        match result.unwrap_err().first().unwrap() {
            ResolverError::Io { path, .. } => {
                assert_eq!(
                    path,
                    &PathBuf::from("definitely_non_existent_file_12345.steel")
                );
            }
            err => panic!("Expected Io error, got: {:?}", err),
        }
    }

    #[test]
    fn test_parse_error_in_imported_module() {
        let project = TestProject::new();
        let root = project.write_file("main.steel", "import broken/Item;");
        project.write_file("broken.steel", "let = +;");

        let mut resolver = ModuleResolver::new();
        let result = resolver.resolve(&root);
        assert!(result.is_err());
        match result.unwrap_err().first().unwrap() {
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

        let parse_err = ResolverError::parse_error("bad_mod", vec!["Unexpected token".to_string()]);
        assert!(parse_err.to_string().contains("bad_mod"));
        assert!(parse_err.to_string().contains("Unexpected token"));
    }
}
