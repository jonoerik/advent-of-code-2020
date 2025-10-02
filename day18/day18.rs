#![feature(custom_test_frameworks)]
#![feature(file_buffered)]
#![feature(iterator_try_collect)]
#![test_runner(datatest::runner)]

use std::boxed::Box;
use std::error::Error;
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

fn load_input(path: &Path) -> Result<Input, Box<dyn Error>> {
    let line_regex = Regex::new(r"[()+*]|\d+").unwrap();
    let lines = File::open_buffered(path)?.lines();

    let to_token = |m: regex::Match| -> Result<InputToken, Box<dyn Error>
> {
        Ok::<InputToken, Box<dyn Error>
    >(match m.as_str() {
                "(" => InputToken::LParen,
                ")" => InputToken::RParen,
                "+" => InputToken::Plus,
                "*" => InputToken::Star,
                digits_str => InputToken::Num(digits_str.parse()?),
            })
    };

    let to_expr = |line: &str| -> Result<InputExpr, Box<dyn Error>
> {
        line_regex.find_iter(line).map(to_token).try_collect()
    };

    lines.map(|l| {to_expr(&l?)}).try_collect()
}

fn part1(input: &Input) -> Result<u64, Box<dyn Error>> {
    fn calculate_subexpression<'a, I>(it: &mut Peekable<I>) -> Result<u64, Box<dyn Error>
>
        where I: Iterator<Item = &'a InputToken> {

        fn get_following_value<'a, I>(it: &mut Peekable<I>) -> Result<u64, Box<dyn Error>
    >
            where I: Iterator<Item = &'a InputToken> {
            match it.next() {
                None => Err("Unexpected end of expression.".into()),
                Some(InputToken::LParen) => {
                    let sub_result = calculate_subexpression(it)?;
                    if let Some(InputToken::RParen) = it.next() {
                        Ok(sub_result)
                    } else {
                        Err("No ')' at end of subexpression.".into())
                    }
                },
                Some(InputToken::RParen) => Err("Unexpected ')'.".into()),
                Some(InputToken::Plus) => Err("Unexpected '+'.".into()),
                Some(InputToken::Star) => Err("Unexpected '*'.".into()),
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
                Some(InputToken::LParen) => return Err("Unexpected '('.".into()),
                Some(InputToken::RParen) => return Ok(result),
                Some(InputToken::Plus) => {
                    result += get_following_value(it)?;
                },
                Some(InputToken::Star) => {
                    result *= get_following_value(it)?;
                },
                Some(InputToken::Num(_)) => return Err("Unexpected number.".into()),
            }
        }
    }
    return input.iter().map(|input_line| {
        let mut line_iter = input_line.iter().peekable();
        let line_result = calculate_subexpression(&mut line_iter)?;
        if let Some(_) = line_iter.next() {
            return Err("Elements left in expression after calculating main subexpression.".into())
        }
        Ok(line_result)
    }).sum();
}

fn part2(input: &Input) -> Result<u64, Box<dyn Error>> {
    /// Implementation of the Shunting Yard algorithm to evaluate the infix input expression.
    /// The output stack is evaluated as tokens are pushed into it, rather than at the end.
    /// https://en.wikipedia.org/w/index.php?title=Shunting_yard_algorithm&oldid=1296996216
    /// Accessed 2025-09-30.
    fn shunting_yard(input: impl Iterator<Item = InputToken>) -> Result<u64, Box<dyn Error>> {
        let mut output = Vec::new();
        let mut shunt = Vec::new();

        // Evaluate the operator token t on the top of the output stack.
        let eval_op = |t, output: &mut Vec<InputToken>| -> Result<(), Box<dyn Error>> {
            let get_output_num = |output: &mut Vec<InputToken>| -> Result<u64, Box<dyn Error>> {
                match output.pop().ok_or("Not enough elements on output stack for operator.")? {
                    InputToken::Num(n) => Ok(n),
                    _ => Err("Element in output stack is not a number.".into()),
                }
            };

            match t {
                InputToken::Plus => {
                    let a = get_output_num(output)?;
                    let b = get_output_num(output)?;
                    output.push(InputToken::Num(a + b));
                },
                InputToken::Star => {
                    let a = get_output_num(output)?;
                    let b = get_output_num(output)?;
                    output.push(InputToken::Num(a * b));
                },
                _ => assert!(false, "eval_op() called on non-operator token."),
            };
            Ok(())
        };

        for t in input {
            match t {
                    InputToken::LParen => shunt.push(t),
                    InputToken::RParen => {
                        loop {
                            match shunt.pop() {
                                None => return Err("Unmatched parentheses in input expression".into()),
                                Some(InputToken::LParen) => break,
                                Some(InputToken::RParen) => assert!(false, "Unexpected RParen in shunt stack."),
                                Some(shunt_t @ (InputToken::Plus | InputToken::Star)) => eval_op(shunt_t, &mut output)?,
                                Some(InputToken::Num(_)) => assert!(false, "Unexpected Num in shunt stack."),
                            }
                        }
                    },
                    InputToken::Plus | InputToken::Star => {
                        // All operators are left associative, and (uniquely to this puzzle) + has higher precedence than *.
                        while let Some(&shunt_t) = shunt.last() {
                            match shunt_t {
                                InputToken::LParen => break,
                                InputToken::RParen => assert!(false, "Unexpected RParen in shunt stack."),
                                InputToken::Plus | InputToken::Star => {
                                    match (t, shunt_t) {
                                        (InputToken::Plus, InputToken::Star) => break,
                                        _ => {
                                            shunt.pop();
                                            eval_op(shunt_t, &mut output)?;
                                        }
                                    }
                                },
                                InputToken::Num(_) => assert!(false, "Unexpected Num in shunt stack."),
                            }
                        }
                        shunt.push(t);
                    },
                    InputToken::Num(_) => output.push(t),
            }
        }

        for &t in shunt.iter().rev() {
            match t {
                InputToken::LParen => return Err("Unmatched parentheses in input expression.".into()),
                InputToken::RParen => assert!(false, "Unexpected RParen in shunt stack."),
                InputToken::Plus | InputToken::Star => eval_op(t, &mut output)?,
                InputToken::Num(_) => assert!(false, "Unexpected Num in shunt stack."),
            }
        }

        return if output.len() == 1 {
            if let InputToken::Num(n) = output[0] {
                Ok(n)
            } else {
                Err("Item on output stack is not a Num.".into())
            }
        } else if output.len() == 0 {
            Err("No values on output stack.".into())
        } else {
            Err("Too many values on output stack.".into())
        };
    }

    return input.iter().map(|input_line| {
        shunting_yard(input_line.iter().cloned())
    }).sum();
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
fn test_case(input_path: &Path, expected_result: &str, result_path: &Path) -> Result<(), Box<dyn Error>> {
    let expected_result = expected_result.trim().parse::<u64>()?;
    let part_fn = if result_path.extension().unwrap().to_str().unwrap().ends_with("1") {part1} else {part2};
    let result = part_fn(&load_input(input_path)?)?;
    assert_eq!(result, expected_result);
    Ok(())
}
