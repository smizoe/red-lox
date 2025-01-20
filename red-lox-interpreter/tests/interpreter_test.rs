use std::{io::Cursor, path::PathBuf};

use lazy_static::lazy_static;
use red_lox_ast::parser::Parser;
use red_lox_ast::scanner::Scanner;
use red_lox_interpreter::Interpreter;
use regex::Regex;
use rstest::rstest;
use std::fs::File;
use std::io::Read;

#[rstest]
fn test_lox_interpreter(#[files("../tests/lox/**/*.lox")] path: PathBuf) {
    let mut content = String::new();
    {
        let mut lox_file = File::open(path).unwrap();
        lox_file
            .read_to_string(&mut content)
            .expect("failed to read file.");
    }
    let mut scanner = Scanner::new(&content);
    let scan_result = scanner.scan_tokens();
    assert_eq!(
        scan_result.errors.len(),
        0,
        "ScanResult contains some errors: {:?}",
        scan_result.errors
    );

    let mut parser = Parser::new(scan_result.tokens);
    let parse_result = parser.parse();
    assert_eq!(
        parse_result.errors.len(),
        0,
        "ParseResult contains some errors: {:?}",
        parse_result.errors
    );

    let mut out = Cursor::new(Vec::new());
    let mut err = Cursor::new(Vec::new());
    let mut interpreter = Interpreter::new(&mut out, &mut err);
    interpreter.interpret(&parse_result.stmts);

    let expected_output = ExpectedOutput::new(&content);
    let out_lines = String::from_utf8(out.into_inner())
        .unwrap()
        .lines()
        .map(str::to_string)
        .collect::<Vec<String>>();
    assert_eq!(out_lines, expected_output.stdout);
    let err_lines = String::from_utf8(err.into_inner())
        .unwrap()
        .lines()
        .map(str::to_string)
        .collect::<Vec<String>>();
    assert_eq!(err_lines, expected_output.stderr);
}

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
    fn new(lox_content: &str) -> Self {
        let mut result = Self::default();
        for line in lox_content.lines() {
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
