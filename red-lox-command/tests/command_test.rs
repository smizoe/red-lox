use std::path::Path;
use std::{io::Cursor, path::PathBuf};

use lazy_static::lazy_static;
use regex::Regex;
use rstest::rstest;
use std::fs::File;
use std::io::Read;

lazy_static! {
    static ref EXPECTATION_RE: Regex =
        Regex::new(r"^.*//[[:space:]]*([[:word:]]+):[[:space:]](.*)$").unwrap();
}

#[derive(Debug, Default)]
struct ExpectedOutput {
    stdout: Vec<String>,
    stderr: Vec<String>,
}

impl ExpectedOutput {
    fn new(path: &Path) -> Self {
        let mut content = String::new();
        {
            let mut lox_file = File::open(path).unwrap();
            lox_file
                .read_to_string(&mut content)
                .expect("failed to read file.");
        }
        let mut result = Self::default();
        for line in content.lines() {
            if let Some(captures) = EXPECTATION_RE.captures(line) {
                let to_push = match captures.get(1).unwrap().as_str() {
                    "out" => Some(&mut result.stdout),
                    "err" => Some(&mut result.stderr),
                    _ => None,
                };
                if let Some(v) = to_push {
                    v.extend(
                        captures
                            .get(2)
                            .unwrap()
                            .as_str()
                            .lines()
                            .map(str::to_string),
                    );
                }
            }
        }
        result
    }
}

#[rstest]
fn test_lox_interpreter(
    #[files("../tests/lox/**/*.lox")]
    #[exclude("compiler_only")]
    path: PathBuf,
) {
    use red_lox_command::interpreter::run_interpreter;

    let mut out = Cursor::new(Vec::new());
    let mut err = Cursor::new(Vec::new());
    let expected_output = ExpectedOutput::new(&path);

    let _ = run_interpreter(Some(path), &mut out, &mut err);

    let out_lines = String::from_utf8(out.into_inner())
        .unwrap()
        .lines()
        .map(str::to_string)
        .collect::<Vec<String>>();
    let err_lines = String::from_utf8(err.into_inner())
        .unwrap()
        .lines()
        .map(str::to_string)
        .collect::<Vec<String>>();
    assert_eq!(
        out_lines, expected_output.stdout,
        "out_lines: {:?}\nerr_lines: {:?}",
        out_lines, err_lines
    );
    assert_eq!(
        err_lines, expected_output.stderr,
        "out_lines: {:?}\nerr_lines: {:?}",
        out_lines, err_lines
    );
}

#[rstest]
fn test_lox_compiler(
    #[files("../tests/lox/**/*.lox")]
    #[exclude("(class|conditional|errors|functions|logical_op|ternary_op)")]
    path: PathBuf,
) {
    use red_lox_command::compiler::run_compiler;

    let mut out = Cursor::new(Vec::new());
    let mut err = Cursor::new(Vec::new());
    let expected_output = ExpectedOutput::new(&path);

    let _ = run_compiler(Some(path), &mut out, &mut err);

    let out_lines = String::from_utf8(out.into_inner())
        .unwrap()
        .lines()
        .map(str::to_string)
        .collect::<Vec<String>>();
    let err_lines = String::from_utf8(err.into_inner())
        .unwrap()
        .lines()
        .map(str::to_string)
        .collect::<Vec<String>>();
    assert_eq!(
        out_lines, expected_output.stdout,
        "out_lines: {:?}\nerr_lines: {:?}",
        out_lines, err_lines
    );
    assert_eq!(
        err_lines, expected_output.stderr,
        "out_lines: {:?}\nerr_lines: {:?}",
        out_lines, err_lines
    );
}
