#![feature(custom_test_frameworks)]
#![feature(file_buffered)]
#![feature(iterator_try_collect)]
#![test_runner(datatest::runner)]

use std::borrow::Cow;
use std::fmt;
use std::fs::File;
use std::io::BufRead;
use std::iter::Peekable;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use regex::Regex;

#[derive(Parser, Debug)]
#[clap()]
#[clap(group(
        clap::ArgGroup::new("part")
            .required(true)
            .multiple(false)
    ))]
struct Args {
    #[clap(short = '1', long, action, group = "part")]
    part1: bool,
    #[clap(short = '2', long, action, group = "part")]
    part2: bool,

    input_path: PathBuf,
}

#[derive(Debug, Clone, Copy)]
enum InputToken {
    LParen,
    RParen,
    Plus,
    Star,
    Num(u64),
}

type InputExpr = Vec<InputToken>;
type Input = Vec<InputExpr>;

#[derive(Debug)]
struct AocError {
    msg: Cow<'static, str>,
}

impl From<std::io::Error> for AocError {
    fn from(e: std::io::Error) -> Self {
        return Self{msg: e.to_string().into()};
    }
}
impl From<std::num::ParseIntError> for AocError {
    fn from(e: std::num::ParseIntError) -> Self {
        return Self{msg: e.to_string().into()};
    }
}
impl From<std::num::TryFromIntError> for AocError {
    fn from(e: std::num::TryFromIntError) -> Self {
        return Self{msg: e.to_string().into()};
    }
}
impl fmt::Display for AocError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error: {}", self.msg)
    }
}

fn load_input(path: &Path) -> Result<Input, AocError> {
    let line_regex = Regex::new(r"[()+*]|\d+").unwrap();
    let lines = File::open_buffered(path)?.lines();

    let to_token = |m: regex::Match| -> Result<InputToken, AocError> {
        Ok::<InputToken, AocError>(match m.as_str() {
                "(" => InputToken::LParen,
                ")" => InputToken::RParen,
                "+" => InputToken::Plus,
                "*" => InputToken::Star,
                digits_str => InputToken::Num(digits_str.parse()?),
            })
    };

    let to_expr = |line: &str| -> Result<InputExpr, AocError> {
        line_regex.find_iter(line).map(to_token).try_collect()
    };

    lines.map(|l| {to_expr(&l?)}).try_collect()
}

fn calculate_subexpression<'a, I>(it: &mut Peekable<I>) -> Result<u64, AocError>
    where I: Iterator<Item = &'a InputToken> {

    fn get_following_value<'a, I>(it: &mut Peekable<I>) -> Result<u64, AocError>
        where I: Iterator<Item = &'a InputToken> {
        match it.next() {
            None => Err(AocError{msg: "Unexpected end of expression.".into()}),
            Some(InputToken::LParen) => {
                let sub_result = calculate_subexpression(it)?;
                if let Some(InputToken::RParen) = it.next() {
                    Ok(sub_result)
                } else {
                    Err(AocError{msg: "No ')' at end of subexpression.".into()})
                }
            },
            Some(InputToken::RParen) => Err(AocError{msg: "Unexpected ')'.".into()}),
            Some(InputToken::Plus) => Err(AocError{msg: "Unexpected '+'.".into()}),
            Some(InputToken::Star) => Err(AocError{msg: "Unexpected '*'.".into()}),
            Some(InputToken::Num(v)) => Ok(*v),
        }
    }

    let mut result = get_following_value(it)?;

    loop {
        if let Some(InputToken::RParen) = it.peek() {
            return Ok(result);
        }

        match it.next() {
            None => return Ok(result),
            Some(InputToken::LParen) => return Err(AocError{msg: "Unexpected '('.".into()}),
            Some(InputToken::RParen) => return Ok(result),
            Some(InputToken::Plus) => {
                result += get_following_value(it)?;
            },
            Some(InputToken::Star) => {
                result *= get_following_value(it)?;
            },
            Some(InputToken::Num(_)) => return Err(AocError{msg: "Unexpected number.".into()}),
        }
    }
}

fn part1(input: &Input) -> Result<u64, AocError> {
    return input.iter().map(|input_line| {
        let mut line_iter = input_line.iter().peekable();
        let line_result = calculate_subexpression(&mut line_iter)?;
        if let Some(_) = line_iter.next() {
            return Err(AocError{msg: "Elements left in expression after calculating main subexpression.".into()})
        }
        Ok(line_result)
    }).sum();
}

fn part2(input: &Input) -> Result<u64, AocError> {
    todo!()
}

fn main() -> ExitCode {
    let args = Args::parse();
    let input = match load_input(&args.input_path) {
        Ok(input) => input,
        Err(e) => {
            eprintln!("{}", e);
            return ExitCode::FAILURE;
        }
    };

    let result = match (args.part1, args.part2) {
        (true, false) => part1(&input),
        (false, true) => part2(&input),
        _ => unreachable!(),
    };

    return match result {
        Ok(result) => {
            println!("{result}");
            ExitCode::SUCCESS
        },
        Err(error) => {
            eprintln!("{}", error);
            ExitCode::FAILURE
        },
    };
}

#[datatest::files("data", {
    expected_result in r"^(data/sample[[:digit:]]+)\.answer(?:1|2)$",
    input_path = r"${1}",
    result_path = r"${0}",
})]
#[test]
fn test_case(input_path: &Path, expected_result: &str, result_path: &Path) -> Result<(), AocError> {
    let expected_result = expected_result.trim().parse::<u64>()?;
    let part_fn = if result_path.extension().unwrap().to_str().unwrap().ends_with("1") {part1} else {part2};
    let result = part_fn(&load_input(input_path)?)?;
    assert_eq!(result, expected_result);
    Ok(())
}
