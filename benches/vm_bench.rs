use criterion::{Criterion, criterion_group, criterion_main};
use std::fs;
use std::path::Path;
use steel_lang::resolver::new_pipeline::pipeline;
use steel_lang::resolver::resolution::Source;
use steel_lang::vm::VM;
use steel_lang::{CompiledProgram, Mode, RunConfig};

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
        let program = pipeline(&RunConfig::new(
            &name,
            Source::File {
                name: &name,
                source: &source,
            },
            Mode::Compile,
            false,
        ));
        let mut compiled = program.program.expect("Expected correct bench program");

        c.bench_function(&name, |b| {
            b.iter(|| run_once(&mut compiled));
        });
    }
}

fn run_once(compiled: &mut CompiledProgram) {
    let func = compiled.func;
    let mut vm = VM::new(compiled.global_count, &mut compiled.gc);
    vm.set_natives_by_name(&compiled.natives, &compiled.extern_fns);
    vm.run(func).expect("SteelProgram: runtime error");
    drop(vm);
    compiled.gc.collect_roots(compiled.func);
}

criterion_group!(benches, bench_programs);
criterion_main!(benches);
