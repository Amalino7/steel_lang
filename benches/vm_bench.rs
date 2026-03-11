use criterion::{criterion_group, criterion_main, Criterion};
use std::fs;
use std::hint::black_box;
use std::path::Path;
use steel_lang::SteelProgram;

/// Discovers every `.steel` file under `benches/programs/`, compiles each one
/// exactly once, then registers a Criterion benchmark that calls `run_once`
/// in a tight loop — measuring pure VM execution time.
fn bench_programs(c: &mut Criterion) {
    let programs_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("benches/programs");

    let mut entries: Vec<_> = fs::read_dir(&programs_dir)
        .expect("benches/programs/ directory not found")
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().and_then(|ext| ext.to_str()) == Some("steel"))
        .collect();

    // Stable ordering so benchmark IDs are deterministic across runs.
    entries.sort_by_key(|e| e.path());

    for entry in entries {
        let path = entry.path();
        let name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .expect("non-UTF-8 filename")
            .to_string();

        let source = fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("Failed to read {}: {e}", path.display()));

        // compile once, outside the measured loop
        let mut program = SteelProgram::compile(&source);

        c.bench_function(&name, |b| {
            b.iter(|| black_box(program.run_once()));
        });
    }
}

criterion_group!(benches, bench_programs);
criterion_main!(benches);
