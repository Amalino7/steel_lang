use std::process::{Command, ExitStatus};

fn steel() -> Command {
    Command::new(env!("CARGO_BIN_EXE_steel_lang"))
}

/// Runs the command and returns its exit status. Unlike `Command::status`, the child's stdout/stderr are
/// piped rather than inherited, so they cannot leak past the test harness's output capture.
fn exit_status(cmd: &mut Command) -> ExitStatus {
    cmd.output().unwrap().status
}

#[test]
fn run_mode_exits_zero() {
    let status = exit_status(steel().args(["tests/fixtures/hello.steel"]));
    assert!(status.success());
}

#[test]
fn check_mode_exits_zero() {
    let status = exit_status(steel().args(["tests/fixtures/hello.steel", "check"]));
    assert!(status.success());
}

#[test]
fn check_mode_debug_prints_ast() {
    let output = steel()
        .args(["tests/fixtures/hello.steel", "check", "-d"])
        .output()
        .unwrap();
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("=== AST ==="),
        "Expected AST header in output:\n{stdout}"
    );
}

#[test]
fn type_error_exits_one() {
    let status = exit_status(steel().args(["tests/fixtures/type_error.steel"]));
    assert_eq!(status.code(), Some(1));
}

#[test]
fn runtime_error_exits_two() {
    let status = exit_status(steel().args(["tests/fixtures/runtime_error.steel"]));
    assert_eq!(status.code(), Some(2));
}

#[test]
fn missing_file_exits_one() {
    let status = exit_status(steel().args(["nonexistent_file_that_does_not_exist.steel"]));
    assert_eq!(status.code(), Some(1));
}

#[test]
fn missing_file_reports_cannot_read() {
    let output = steel()
        .args([
            "nonexistent_file_that_does_not_exist.steel",
            "--color",
            "never",
        ])
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("cannot read"),
        "stderr:
{stderr}"
    );
}

#[test]
fn missing_import_is_rendered_with_the_importers_label() {
    let output = steel()
        .args([
            "tests/fixtures/missing_import.steel",
            "check",
            "--color",
            "never",
        ])
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Module not found"),
        "stderr:
{stderr}"
    );
    assert!(
        stderr.contains("missing_import"),
        "stderr:
{stderr}"
    );
}

#[test]
fn warnings_are_rendered_on_success() {
    let output = steel()
        .args([
            "tests/fixtures/unreachable_warning.steel",
            "check",
            "--color",
            "never",
        ])
        .output()
        .unwrap();
    assert!(output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Unreachable code"),
        "Expected a warning on stderr:\n{stderr}"
    );
}

#[test]
fn error_limit_renders_limited_errors_and_one_summary() {
    let output = steel()
        .args([
            "tests/fixtures/many_type_errors.steel",
            "check",
            "--error-limit",
            "1",
            "--color",
            "never",
        ])
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(
        stderr.matches("Error:").count(),
        1,
        "Expected exactly one rendered error:\n{stderr}"
    );
    assert_eq!(
        stderr.matches("... and 2 more error(s)").count(),
        1,
        "Expected exactly one summary line:\n{stderr}"
    );
}

#[test]
fn error_label_is_the_file_path() {
    let output = steel()
        .args(["tests/fixtures/type_error.steel", "--color", "never"])
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("type_error.steel:1:"),
        "Expected the file name in the label:\n{stderr}"
    );
    assert!(
        !stderr.contains(r"\?\"),
        "The verbatim path prefix must not be shown:\n{stderr}"
    );
}

#[test]
fn extern_func_in_a_user_file_is_rejected() {
    let output = steel()
        .args(["tests/fixtures/extern_user.steel", "check"])
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("E084"), "stderr:\n{stderr}");
}

/// Runs `check` on a fixture in `tests/fixtures/unavailable/` and returns (exit code, stderr).
fn check_unavailable(file: &str) -> (Option<i32>, String) {
    let output = steel()
        .args([
            &format!("tests/fixtures/unavailable/{file}"),
            "check",
            "--color",
            "never",
        ])
        .output()
        .unwrap();
    (
        output.status.code(),
        String::from_utf8_lossy(&output.stderr).into_owned(),
    )
}

#[test]
fn names_imported_from_an_unparsable_module_add_no_errors() {
    let (code, stderr) = check_unavailable("uses_unparsable.steel");
    assert_eq!(code, Some(1));
    assert_eq!(
        stderr.matches("Error:").count(),
        1,
        "Expected only the parser error:\n{stderr}"
    );
    assert!(stderr.contains("bad_syntax.steel"), "stderr:\n{stderr}");
}

#[test]
fn names_imported_from_a_missing_module_add_no_errors() {
    let (code, stderr) = check_unavailable("uses_missing.steel");
    assert_eq!(code, Some(1));
    assert_eq!(
        stderr.matches("Error:").count(),
        1,
        "Expected only the resolver error:\n{stderr}"
    );
    assert!(stderr.contains("Module not found"), "stderr:\n{stderr}");
}

#[test]
fn wildcard_import_of_a_missing_module_reports_the_resolver_error() {
    let (code, stderr) = check_unavailable("wildcard_missing.steel");
    assert_eq!(code, Some(1));
    assert!(stderr.contains("Module not found"), "stderr:\n{stderr}");
    assert!(!stderr.contains("panicked"), "stderr:\n{stderr}");
}

#[test]
fn private_and_missing_items_of_an_available_module_are_still_reported() {
    let (code, stderr) = check_unavailable("bad_items.steel");
    assert_eq!(code, Some(1));
    assert!(stderr.contains("E071"), "stderr:\n{stderr}");
    assert!(stderr.contains("E070"), "stderr:\n{stderr}");
}

#[test]
fn redeclaring_a_name_from_a_missing_module_points_at_the_import() {
    let (code, stderr) = check_unavailable("redeclare_missing.steel");
    assert_eq!(code, Some(1));
    let (_, duplicate) = stderr
        .split_once("[E033] Error: Duplicate definition of type Foo")
        .unwrap_or_else(|| {
            panic!(
                "stderr:
{stderr}"
            )
        });
    // The original is the import (line 1), labelled before the redeclaration on line 2.
    let import = duplicate.find("import nope/Foo").unwrap_or_else(|| {
        panic!(
            "stderr:
{stderr}"
        )
    });
    let label = duplicate.find("First defined here.").unwrap_or_else(|| {
        panic!(
            "stderr:
{stderr}"
        )
    });
    let redeclaration = duplicate.find("struct Foo").unwrap_or_else(|| {
        panic!(
            "stderr:
{stderr}"
        )
    });
    assert!(
        import < label && label < redeclaration,
        "stderr:
{stderr}"
    );
}
