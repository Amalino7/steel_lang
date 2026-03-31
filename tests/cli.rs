use std::process::Command;

fn steel() -> Command {
    Command::new(env!("CARGO_BIN_EXE_steel_lang"))
}

#[test]
fn run_mode_exits_zero() {
    let status = steel()
        .args(["tests/fixtures/hello.steel"])
        .status()
        .unwrap();
    assert!(status.success());
}

#[test]
fn check_mode_exits_zero() {
    let status = steel()
        .args(["tests/fixtures/hello.steel", "check"])
        .status()
        .unwrap();
    assert!(status.success());
}

#[test]
fn parse_mode_exits_zero() {
    let status = steel()
        .args(["tests/fixtures/hello.steel", "parse"])
        .status()
        .unwrap();
    assert!(status.success());
}

#[test]
fn parse_mode_debug_prints_ast() {
    let output = steel()
        .args(["tests/fixtures/hello.steel", "parse", "-d"])
        .output()
        .unwrap();
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("=== AST ==="), "Expected AST header in output:\n{stdout}");
}

#[test]
fn type_error_exits_one() {
    let status = steel()
        .args(["tests/fixtures/type_error.steel"])
        .status()
        .unwrap();
    assert_eq!(status.code(), Some(1));
}

#[test]
fn runtime_error_exits_two() {
    let status = steel()
        .args(["tests/fixtures/runtime_error.steel"])
        .status()
        .unwrap();
    assert_eq!(status.code(), Some(2));
}

#[test]
fn missing_file_exits_one() {
    let status = steel()
        .args(["nonexistent_file_that_does_not_exist.steel"])
        .status()
        .unwrap();
    assert_eq!(status.code(), Some(1));
}
