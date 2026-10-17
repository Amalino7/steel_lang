use crate::parser::Parser;
use crate::parser::ast::{ImportSegment, ImportStmt, ImportType};
use crate::resolver::error::{EntryError, LoadError, ResolverError};
use crate::resolver::{
    FileId, ModuleGraph, ModuleId, ModuleInfo, PRELUDE_FILE, STD_MODULE, collisions,
};
use crate::scanner::{Scanner, Span};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

pub struct ModuleResolver {
    /// Modules being loaded, (full_path, module_name)
    visiting: Vec<(PathBuf, String)>,
    next_file_id: u32,
    prelude: Option<String>,
    check_portable: bool,
    pub errors: Vec<ResolverError>,
}

impl Default for ModuleResolver {
    fn default() -> Self {
        Self::new()
    }
}

pub enum Source<'a> {
    EntryFile(&'a Path),
    File {
        name: &'a str,
        source: &'a str,
    },
    SourceMap {
        entry: &'a str,
        map: HashMap<String, &'a str>,
    },
}

/// Reads a module's source and path by its module path.
type Loader<'l> = dyn Fn(&str) -> Result<(String, PathBuf), LoadError> + 'l;

impl ModuleResolver {
    pub fn new() -> Self {
        Self {
            check_portable: false,
            visiting: Vec::new(),
            errors: Vec::new(),
            next_file_id: 1,
            prelude: None,
        }
    }

    pub fn new_with_prelude(prelude: &str) -> Self {
        Self {
            prelude: Some(prelude.to_string()),
            ..Self::new()
        }
    }

    pub fn portable(&mut self) -> &mut Self {
        self.check_portable = true;
        self
    }

    /// An empty graph, holding only `std` when this resolver has a prelude.
    fn new_graph(&self) -> ModuleGraph {
        let mut graph = ModuleGraph::new();
        if let Some(prelude) = &self.prelude {
            let id = ModuleId(0);
            graph.name_to_id_map.insert(STD_MODULE.to_string(), id);
            graph.file_to_module_id.insert(PRELUDE_FILE, id);
            graph.modules.push(ModuleInfo {
                id,
                file_id: PRELUDE_FILE,
                name: STD_MODULE.to_string(),
                path: PathBuf::from("<std>"),
                source: prelude.clone(),
                imports: vec![],
                is_std: true,
            });
        }
        graph
    }

    pub fn resolve_source(&mut self, src: &Source) -> Result<ModuleGraph, EntryError> {
        match src {
            Source::EntryFile(file) => self.resolve(file),
            Source::File { name, source } => self.resolve_file(name.to_string(), source),
            Source::SourceMap { entry, map } => self.resolve_mock(entry.to_string(), map),
        }
    }

    pub fn resolve(&mut self, entry_file: &Path) -> Result<ModuleGraph, EntryError> {
        let entry_error = |error| EntryError::Io {
            path: entry_file.to_path_buf(),
            error,
        };
        let canonical_entry = entry_file.canonicalize().map_err(entry_error)?;
        let source = fs::read_to_string(&canonical_entry).map_err(entry_error)?;

        let root = canonical_entry.parent().unwrap_or_else(|| Path::new("."));

        if self.check_portable {
            let collisions = collisions::walk_tree(root).map_err(entry_error)?;
            if !collisions.is_empty() {
                return Err(EntryError::PathCollisions { collisions });
            }
        }

        let entry_module_name = canonical_entry
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("main")
            .to_string();

        if entry_module_name == STD_MODULE {
            return Err(EntryError::ReservedName {
                name: entry_module_name,
            });
        }

        let mut graph = self.new_graph();
        self.load_module(
            entry_module_name,
            source,
            canonical_entry.clone(),
            &mut graph,
            &|name| {
                let path = Self::resolve_module_to_path(root, name)?;
                let source = fs::read_to_string(&path).map_err(|error| LoadError::Io {
                    path: path.clone(),
                    error,
                })?;
                Ok((source, path))
            },
        );

        Ok(graph)
    }

    pub fn resolve_mock(
        &mut self,
        entry: String,
        source_map: &HashMap<String, &str>,
    ) -> Result<ModuleGraph, EntryError> {
        let loader = |name: &str| {
            let source = source_map.get(name).ok_or(LoadError::NotFound {
                searched_paths: vec![],
            })?;
            let path = if name.ends_with(".steel") {
                name.into()
            } else {
                format!("{}.steel", name).into()
            };
            Ok((source.to_string(), path))
        };

        if entry == STD_MODULE {
            return Err(EntryError::ReservedName { name: entry });
        }

        let (source, path) = loader(&entry).map_err(|_| EntryError::MissingEntry {
            name: entry.clone(),
        })?;
        let mut graph = self.new_graph();
        self.load_module(entry, source, path, &mut graph, &loader);
        Ok(graph)
    }

    /// A single file is a source map with one module, so its imports are reported as not found.
    pub fn resolve_file(&mut self, name: String, source: &str) -> Result<ModuleGraph, EntryError> {
        let map = HashMap::from([(name.clone(), source)]);
        self.resolve_mock(name, &map)
    }

    /// Resolves one import. Returns `None` after reporting the error at `span`: the module is unavailable and
    /// stays out of the graph.
    fn visit_import(
        &mut self,
        name: &str,
        span: Span,
        graph: &mut ModuleGraph,
        loader: &Loader,
    ) -> Option<ModuleId> {
        if let Some(id) = graph.name_to_id_map.get(name) {
            return Some(*id);
        }

        match loader(name) {
            Ok((source, path)) => {
                if let Some(pos) = self.visiting.iter().position(|(p, _)| *p == path) {
                    let mut cycle: Vec<String> = self.visiting[pos..]
                        .iter()
                        .map(|(_, name)| name.clone())
                        .collect();
                    cycle.push(name.to_string());
                    self.errors
                        .push(ResolverError::cyclic_dependency(cycle, span));
                    return None;
                }

                Some(self.load_module(name.to_string(), source, path, graph, loader))
            }
            Err(load_err) => {
                self.errors
                    .push(load_err.into_resolver_err(name.to_string(), span));
                None
            }
        }
    }

    /// Adds a module that was read successfully to the graph, after the modules it imports.
    fn load_module(
        &mut self,
        name: String,
        source: String,
        path: PathBuf,
        graph: &mut ModuleGraph,
        loader: &Loader,
    ) -> ModuleId {
        let file_id = self.next_id();
        self.visiting.push((path.clone(), name.clone()));

        // Only the header matters here: errors are reported by the full parse in the pipeline.
        let header = Parser::new(Scanner::new(&source, file_id)).parse_imports();
        let imports = self
            .get_dependencies(&header)
            .into_iter()
            .filter_map(|dep| self.visit_import(&dep.module_name, dep.path_span, graph, loader))
            .collect();

        self.visiting.pop();

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
            imports,
            is_std: false,
        });

        module_id
    }

    fn next_id(&mut self) -> FileId {
        let id = FileId(self.next_file_id);
        self.next_file_id += 1;
        id
    }

    fn resolve_module_to_path(root: &Path, name: &str) -> Result<PathBuf, LoadError> {
        let mut base = Self::walk_to_file(root, name)?;

        let mut last_name = base
            .file_name()
            .map(|s| s.to_string_lossy().to_string())
            .expect("Base binding should be correctly constructed");

        last_name.push_str(".steel");

        let index = base.join("index.steel");

        base.set_extension("steel");
        let file = base;

        match (file.exists(), index.exists()) {
            (true, true) => Err(LoadError::Ambiguous {
                paths: vec![file, index],
            }),
            (true, false) => Self::names_match(&file, &last_name),
            (false, true) => Self::names_match(&index, "index.steel"),
            (false, false) => Err(LoadError::NotFound {
                searched_paths: vec![file, index],
            }),
        }
    }

    fn walk_to_file(root: &Path, name: &str) -> Result<PathBuf, LoadError> {
        let segments = name.split("/");

        let seg_count = segments.clone().count();
        let mut current_path = PathBuf::from(root);

        for (idx, seg) in segments.enumerate() {
            let is_last_seg = idx + 1 == seg_count;

            current_path.push(seg);

            let exists = fs::exists(current_path.as_path()).map_err(|io_err| LoadError::Io {
                path: current_path.clone(),
                error: io_err,
            })?;

            // If the last module is a directory, we also have to check it for casing.
            if !exists && !is_last_seg {
                return Err(LoadError::NotFound {
                    searched_paths: vec![current_path],
                });
            } else if !exists && is_last_seg {
                break;
            }

            let _ = Self::names_match(&current_path, seg)?;
        }

        Ok(current_path)
    }

    fn names_match(path: &Path, expected_name: &str) -> Result<PathBuf, LoadError> {
        let canonical = fs::canonicalize(path).map_err(|io_err| LoadError::Io {
            path: path.to_path_buf(),
            error: io_err,
        })?;

        let real_name = canonical
            .file_name()
            .expect("Canonical binding should have a valid file binding.");

        if real_name != expected_name {
            Err(LoadError::NameCase {
                on_disk: real_name.to_string_lossy().into(),
            })
        } else {
            Ok(canonical)
        }
    }

    fn get_dependencies(&mut self, imports: &[ImportStmt]) -> Vec<Dependency> {
        let mut deps = vec![];
        for import in imports {
            self.handle_import(None, &import.segment, &mut deps);
        }
        // `a/{x, y}` has one leaf per item but a single module path: one edge and one error.
        let mut seen = HashSet::new();
        deps.retain(|dep| seen.insert((dep.module_name.clone(), dep.path_span)));
        deps
    }

    fn handle_import(
        &mut self,
        prefix: Option<&Dependency>,
        import: &ImportSegment,
        deps: &mut Vec<Dependency>,
    ) {
        let mut names = prefix
            .map(|dep| dep.module_name.as_str())
            .into_iter()
            .collect::<Vec<_>>();
        names.extend(import.path.iter().map(|p| p.lexeme));
        let module_name = names.join("/");

        let span = prefix
            .map(|dep| dep.path_span)
            .into_iter()
            .chain(import.path.iter().map(|p| p.span))
            .reduce(|a, b| a.merge(b));

        // A segment without a path (`{item}` inside a group) refers to the module of its group.
        let Some(path_span) = span else { return };

        // Only the first segment of the whole import counts: `foo/std/x` is an ordinary module path.
        let module = Dependency {
            module_name,
            path_span,
        };

        if prefix.is_none() && matches!(names.first(), Some(&name) if name == STD_MODULE) {
            self.errors.push(ResolverError::reserved_module_name(
                &module.module_name,
                path_span,
                "Std is reserved for the standard library. ",
            ));
            return;
        }

        if names.contains(&"index") {
            self.errors.push(ResolverError::reserved_module_name(
                &module.module_name,
                path_span,
                "Index component names are ambiguous in file resolution.",
            ));
            return;
        }

        if let ImportType::Group { options } = &import.import_type {
            for option in options {
                self.handle_import(Some(&module), option, deps);
            }
        } else {
            deps.push(module);
        }
    }
}

struct Dependency {
    module_name: String,
    path_span: Span,
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
        let graph = resolver.resolve(&root).expect("entry loads");

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
        let graph = resolver.resolve(&root).expect("entry loads");

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
        let graph = resolver.resolve(&root).expect("entry loads");

        let math_idx = graph
            .modules
            .iter()
            .position(|m| m.name == "core/math")
            .unwrap();
        let string_idx = graph
            .modules
            .iter()
            .position(|m| m.name == "core/string")
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
        let graph = resolver.resolve(&root).expect("entry loads");

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
        let graph = resolver.resolve(&root).expect("entry loads");

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
        let _ = resolver.resolve(&root);
        let err = resolver.errors;
        assert!(!err.is_empty());
        match err.first().unwrap() {
            ResolverError::CyclicDependency { cycle, .. } => {
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

        let _ = resolver.resolve(&root);
        let err = resolver.errors;
        assert!(!err.is_empty());
        match err.first().unwrap() {
            ResolverError::CyclicDependency { cycle, .. } => {
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
        let _ = resolver.resolve(&root);
        let err = resolver.errors;
        assert!(!err.is_empty());
        match err.first().unwrap() {
            ResolverError::CyclicDependency { cycle, .. } => {
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
        let _ = resolver.resolve(&root);
        let err = resolver.errors;
        assert!(!err.is_empty());
        match err.first().unwrap() {
            ResolverError::ModuleNotFound {
                module_name,
                searched_paths,
                ..
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
        let graph = resolver.resolve(&entry).expect("entry loads");
        assert_eq!(graph.modules.len(), 2);
        assert_eq!(graph.modules[0].name, "helper");
        assert_eq!(graph.modules[1].name, "main");
    }

    #[test]
    fn test_entry_file_not_found() {
        let mut resolver = ModuleResolver::new();
        let err = resolver
            .resolve(Path::new("definitely_non_existent_file_12345.steel"))
            .expect_err("entry file does not exist");
        match err {
            EntryError::Io { path, .. } => {
                assert_eq!(
                    path,
                    PathBuf::from("definitely_non_existent_file_12345.steel")
                );
            }
            err => panic!("Expected Io error, got: {:?}", err),
        }
        assert!(resolver.errors.is_empty(), "no import was involved");
    }

    #[test]
    fn test_source_map_without_entry() {
        let mut resolver = ModuleResolver::new();
        let err = resolver
            .resolve_mock("main".to_string(), &HashMap::new())
            .expect_err("entry is not in the map");
        assert!(matches!(err, EntryError::MissingEntry { name } if name == "main"));
    }

    #[test]
    fn test_missing_module_is_absent_from_graph_and_edge_is_none() {
        let project = TestProject::new();
        let root = project.write_file("main.steel", "import non_existent/Item;");

        let mut resolver = ModuleResolver::new();
        let graph = resolver.resolve(&root).expect("entry loads");

        assert_eq!(graph.modules.len(), 1, "only the importer is loaded");
        assert!(graph.module_by_name("non_existent").is_none());
        assert_eq!(graph.modules[0].imports.len(), 0);

        assert_eq!(resolver.errors.len(), 1, "reported once, at the import");
    }

    #[test]
    fn test_file_and_index_for_one_module_is_ambiguous() {
        let project = TestProject::new();
        project.write_file("a.steel", "");
        project.write_file("a/index.steel", "");
        let root = project.write_file("main.steel", "import a/Item;");

        let mut resolver = ModuleResolver::new();
        let graph = resolver.resolve(&root).expect("entry loads");

        assert_eq!(graph.modules.len(), 1, "an ambiguous module is unavailable");
        assert_eq!(graph.modules[0].imports.len(), 0);
        assert_eq!(resolver.errors.len(), 1);
        match &resolver.errors[0] {
            ResolverError::AmbiguousModule {
                module_name, paths, ..
            } => {
                assert_eq!(module_name, "a");
                assert_eq!(paths.len(), 2);
            }
            err => panic!("Expected AmbiguousModule, got: {:?}", err),
        }
    }

    /// Whether `Math` finds `math.steel` (Windows, default macOS).
    fn is_case_insensitive(project: &TestProject) -> bool {
        project.root().join("MATH.steel").exists()
    }

    #[test]
    fn test_module_name_must_match_the_case_of_its_file() {
        let project = TestProject::new();
        project.write_file("math.steel", "");
        let root = project.write_file("main.steel", "import Math/y;");

        let mut resolver = ModuleResolver::new();
        let graph = resolver.resolve(&root).expect("entry loads");

        assert_eq!(resolver.errors.len(), 1);
        if is_case_insensitive(&project) {
            match &resolver.errors[0] {
                ResolverError::ModuleNameCase {
                    module_name,
                    on_disk,
                    ..
                } => assert_eq!(
                    (module_name.as_str(), on_disk.as_str()),
                    ("Math", "math.steel")
                ),
                err => panic!("Expected ModuleNameCase, got: {:?}", err),
            }
        } else {
            assert!(matches!(
                resolver.errors[0],
                ResolverError::ModuleNotFound { .. }
            ));
        }
        assert_eq!(graph.modules.len(), 1, "Math is unavailable");
        assert_eq!(graph.modules[0].imports.len(), 0);
    }

    #[test]
    fn test_module_name_case_checks_directories_too() {
        let project = TestProject::new();
        project.write_file("core/math.steel", "");
        let root = project.write_file("main.steel", "import Core/math/x;");

        let mut resolver = ModuleResolver::new();
        let graph = resolver.resolve(&root).expect("entry loads");

        assert_eq!(resolver.errors.len(), 1);
        if is_case_insensitive(&project) {
            match &resolver.errors[0] {
                ResolverError::ModuleNameCase { on_disk, .. } => {
                    assert_eq!(on_disk, "core")
                }
                err => panic!("Expected ModuleNameCase, got: {:?}", err),
            }
        }
        assert_eq!(graph.modules[0].imports.len(), 0);
    }

    #[test]
    fn test_index_module_must_match_the_case_of_its_directory() {
        let project = TestProject::new();
        project.write_file("math/index.steel", "");
        let root = project.write_file("main.steel", "import Math/x;");

        let mut resolver = ModuleResolver::new();
        let _ = resolver.resolve(&root).expect("entry loads");

        assert_eq!(resolver.errors.len(), 1);
        if is_case_insensitive(&project) {
            assert!(matches!(
                &resolver.errors[0],
                ResolverError::ModuleNameCase { on_disk, .. } if on_disk == "math"
            ));
        }
    }

    #[test]
    fn test_one_spelling_is_not_a_collision() {
        let mut resolver = ModuleResolver::new();
        let _ = resolver
            .resolve_mock(
                "main".to_string(),
                &mock(&[
                    ("main", "import core/a/x; import core/b/y; import Core/c/z;"),
                    ("core/a", ""),
                    ("core/b", ""),
                ]),
            )
            .expect("entry loads");
        // `Core/c` collides with the directory `core`; `core/b` next to `core/a` does not.
        assert_eq!(resolver.errors.len(), 1);
    }

    #[test]
    fn test_cycle_through_a_second_spelling_of_a_file() {
        // main -> foo (foo/index.steel) -> foo/index (the same file)
        let project = TestProject::new();
        project.write_file("foo/index.steel", "import foo/x;");
        let root = project.write_file("main.steel", "import foo/y;");

        let mut resolver = ModuleResolver::new();
        let graph = resolver.resolve(&root).expect("entry loads");

        assert_eq!(resolver.errors.len(), 1);
        match &resolver.errors[0] {
            ResolverError::CyclicDependency { cycle, .. } => {
                assert_eq!(cycle, &["foo", "foo"]);
            }
            err => panic!("Expected CyclicDependency, got: {:?}", err),
        }
        assert_eq!(graph.modules.len(), 2);
    }

    #[test]
    fn test_group_import_of_a_missing_module_is_no_edges_and_one_error() {
        let mut resolver = ModuleResolver::new();
        let graph = resolver
            .resolve_file("main".to_string(), "import other/{a, b, c};")
            .expect("entry exists");
        assert_eq!(graph.modules[0].imports.len(), 0);
        assert_eq!(resolver.errors.len(), 1);
    }

    #[test]
    fn test_each_importer_of_a_missing_module_gets_its_own_error() {
        let project = TestProject::new();
        project.write_file("b.steel", "import nope/Item;");
        let root = project.write_file(
            "main.steel",
            "import b/Item;
import nope/Item;",
        );

        let mut resolver = ModuleResolver::new();
        let graph = resolver.resolve(&root).expect("entry loads");

        assert_eq!(graph.modules.len(), 2);
        assert_eq!(resolver.errors.len(), 2);
    }

    #[test]
    fn test_imports_are_discovered_despite_body_syntax_error() {
        let project = TestProject::new();
        project.write_file("b.steel", "");
        let root = project.write_file(
            "main.steel",
            "import b/Item;
let = ;",
        );

        let mut resolver = ModuleResolver::new();
        let graph = resolver.resolve(&root).expect("entry loads");

        let names: Vec<&str> = graph.modules.iter().map(|m| m.name.as_str()).collect();
        assert_eq!(names, vec!["b", "main"]);
        assert!(resolver.errors.is_empty());
    }

    #[test]
    fn test_cycle_back_edge_is_none_and_both_modules_are_loaded() {
        let project = TestProject::new();
        let root = project.write_file("a.steel", "import b/Item;");
        project.write_file("b.steel", "import a/Item;");

        let mut resolver = ModuleResolver::new();
        let graph = resolver.resolve(&root).expect("entry loads");

        let a = graph.module_by_name("a").expect("a is loaded");
        let b = graph.module_by_name("b").expect("b is loaded");
        assert_eq!(a.imports[0], b.id);
        assert_eq!(b.imports.first(), None, "the back edge is dropped");
        assert_eq!(resolver.errors.len(), 1);
    }

    #[test]
    fn test_single_file_source_reports_its_imports_as_not_found() {
        let mut resolver = ModuleResolver::new();
        let graph = resolver
            .resolve_file("main".to_string(), "import other/Item;")
            .expect("entry exists");
        assert_eq!(graph.modules.len(), 1);
        assert_eq!(graph.modules[0].imports.len(), 0);
        assert_eq!(resolver.errors.len(), 1);
    }

    #[test]
    fn test_error_display_formatting() {
        let io_err = ResolverError::io(
            PathBuf::from("some/path.steel"),
            std::io::Error::new(std::io::ErrorKind::NotFound, "file not found"),
            Span::default(),
        );
        assert!(io_err.to_string().contains("some/path.steel"));
        assert!(io_err.to_string().contains("file not found"));

        let not_found_err = ResolverError::module_not_found(
            "my_module",
            vec![
                PathBuf::from("my_module.steel"),
                PathBuf::from("my_module/index.steel"),
            ],
            Span::default(),
        );
        assert!(not_found_err.to_string().contains("my_module"));
        assert!(not_found_err.to_string().contains("my_module.steel"));

        let cycle_err = ResolverError::cyclic_dependency(
            vec!["a".to_string(), "b".to_string(), "a".to_string()],
            Span::default(),
        );
        assert_eq!(
            cycle_err.to_string(),
            "Cyclic dependency detected: a -> b -> a"
        );
    }

    fn mock<'a>(modules: &[(&str, &'a str)]) -> HashMap<String, &'a str> {
        modules
            .iter()
            .map(|(name, src)| (name.to_string(), *src))
            .collect()
    }

    #[test]
    fn test_prelude_is_module_zero_and_precedes_the_entry() {
        let mut resolver = ModuleResolver::new_with_prelude("public func helper() {}");
        let graph = resolver
            .resolve_mock("main".to_string(), &mock(&[("main", "")]))
            .expect("entry loads");

        let names: Vec<&str> = graph.modules.iter().map(|m| m.name.as_str()).collect();
        assert_eq!(names, vec!["std", "main"]);
        assert!(graph.modules[0].is_std);
        assert_eq!(graph.modules[0].id, ModuleId(0));
        assert_eq!(graph.modules[0].file_id, PRELUDE_FILE);
        assert!(!graph.modules[1].is_std);
        assert_ne!(graph.modules[1].file_id, PRELUDE_FILE);
        assert_eq!(graph.std_module().map(|m| m.id), Some(ModuleId(0)));
        assert!(resolver.errors.is_empty());
    }

    #[test]
    fn test_no_prelude_means_no_std_module() {
        let mut resolver = ModuleResolver::new();
        let graph = resolver
            .resolve_mock("main".to_string(), &mock(&[("main", "")]))
            .expect("entry loads");
        assert!(graph.std_module().is_none());
        assert_eq!(graph.modules[0].id, ModuleId(0));
    }

    #[test]
    fn test_importing_std_is_a_resolver_error() {
        let mut resolver = ModuleResolver::new_with_prelude("");
        let graph = resolver
            .resolve_mock(
                "main".to_string(),
                &mock(&[("main", "import std/x;"), ("std.x", "")]),
            )
            .expect("entry loads");

        assert_eq!(resolver.errors.len(), 1);
        assert!(matches!(
            resolver.errors[0],
            ResolverError::ReservedModuleName { .. }
        ));
        let _ = graph.module_by_name("main").expect("main is loaded");
        assert_eq!(graph.modules.len(), 2, "std.x is not loaded");
    }

    #[test]
    fn test_std_group_import_reports_once() {
        let mut resolver = ModuleResolver::new_with_prelude("");
        let graph = resolver
            .resolve_mock(
                "main".to_string(),
                &mock(&[("main", "import std/{a/X, b/Y};")]),
            )
            .expect("entry loads");

        assert_eq!(resolver.errors.len(), 1);
        let _ = graph.module_by_name("main").expect("main is loaded");
    }

    #[test]
    fn test_std_only_reserved_as_the_first_segment() {
        let mut resolver = ModuleResolver::new_with_prelude("");
        let graph = resolver
            .resolve_mock(
                "main".to_string(),
                &mock(&[
                    ("main", "import lib/std/X; import stdlib/Y;"),
                    ("lib/std", ""),
                    ("stdlib", ""),
                ]),
            )
            .expect("entry loads");

        assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);
        assert_eq!(graph.modules.len(), 4);
    }

    #[test]
    fn test_std_is_reserved_without_a_prelude_too() {
        let mut resolver = ModuleResolver::new();
        let graph = resolver
            .resolve_mock(
                "main".to_string(),
                &mock(&[("main", "import std/x;"), ("std/x", "")]),
            )
            .expect("entry loads");
        assert_eq!(resolver.errors.len(), 1);
        assert_eq!(graph.modules.len(), 1);
    }

    #[test]
    fn test_entry_module_named_std_is_an_entry_error() {
        let mut resolver = ModuleResolver::new_with_prelude("");
        let err = resolver
            .resolve_mock("std".to_string(), &mock(&[("std", "")]))
            .expect_err("std is reserved");
        assert!(matches!(err, EntryError::ReservedName { .. }));

        let project = TestProject::new();
        let root = project.write_file("std.steel", "");
        let err = ModuleResolver::new()
            .resolve(&root)
            .expect_err("std is reserved");
        assert!(matches!(err, EntryError::ReservedName { .. }));
    }
}
