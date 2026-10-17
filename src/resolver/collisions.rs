use std::collections::HashMap;
use std::fmt::Display;
use std::path::{Path, PathBuf};

#[derive(Debug, PartialEq, Eq)]
pub struct Collision {
    pub dir: PathBuf,
    pub names: Vec<String>,
}

/// Walks a directory tree and returns a list of file name collisions on case-insensitive OSes.
pub fn walk_tree(root: &Path) -> std::io::Result<Vec<Collision>> {
    let mut stack = vec![root.to_path_buf()];
    let mut collisions = vec![];

    while let Some(dir) = stack.pop() {
        let mut seen: HashMap<String, Vec<String>> = HashMap::new();

        for entry in dir.read_dir()? {
            let entry = entry?;

            let file_name = entry.file_name().to_string_lossy().to_string();

            if file_name.starts_with('.') {
                continue;
            }

            if entry.file_type()?.is_symlink() {
                continue;
            }

            let lowercase_name = file_name.to_lowercase();
            seen.entry(lowercase_name)
                .and_modify(|vec| vec.push(file_name.clone()))
                .or_insert_with(|| vec![file_name]);

            if entry.file_type()?.is_dir() {
                stack.push(entry.path());
            }
        }

        for groups in seen.into_values() {
            if groups.len() > 1 {
                collisions.push(Collision {
                    dir: dir
                        .strip_prefix(root)
                        .expect("dir is not a subdirectory of root")
                        .to_path_buf(),
                    names: groups,
                });
            }
        }
    }

    Ok(collisions)
}

impl Display for Collision {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Collision in directory: {}", self.dir.display())?;
        write!(f, "\nNames: ")?;
        for name in &self.names {
            write!(f, "{} ", name)?;
        }
        Ok(())
    }
}
