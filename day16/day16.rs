#![feature(assert_matches)]
#![feature(custom_test_frameworks)]
#![feature(file_buffered)]
#![feature(iterator_try_collect)]
#![test_runner(datatest::runner)]

use std::assert_matches::assert_matches;
use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::fs::File;
use std::io::BufRead;
use itertools::Itertools;
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

type ValueRange = (u32, u32);
type Ticket = Vec<u32>;

#[derive(Debug)]
struct Input {
    fields: HashMap<String, Vec<ValueRange>>,
    ticket: Ticket,
    nearby: Vec<Ticket>,
}

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
    let mut lines = File::open_buffered(path)?.lines();

    let field_re = Regex::new(r"^(?<field_name>[^:]+): ([[:digit:]]+)-([[:digit:]]+)(?: or ([[:digit:]]+)-([[:digit:]]+))*$").unwrap();
    let mut fields = HashMap::new();
    for line in lines.by_ref() {
        let line = line?;
        if line.len() == 0 {
            break;
        }
        let captures = field_re.captures(&line).ok_or(AocError{msg: "Input field line didn't match regex".into()})?;
        if captures.len() % 2 != 0 {
            panic!();
        }
        let name = &captures["field_name"];
        let field_range_values = captures.iter().skip(2).map(|c| { c.ok_or(AocError{msg: "Missing regex capture in input field line".into()})
            .and_then(|c| { Ok(c.as_str().parse::<u32>()?) }) });
        let field_ranges = field_range_values.tuples().map(|(a, b)| { Ok::<(u32, u32), AocError>((a?, b?)) }).try_collect()?;
        fields.insert(name.into(), field_ranges);
    }

    if lines.next().ok_or(AocError{msg: "Unexpected early end of input".into()})?? != "your ticket:" {
        return Err(AocError{msg: "Unexpected input when expecting \"your ticket:\"".into()});
    }
    let line = lines.next().ok_or(AocError{msg: "Unexpected early end of input".into()})??;
    let ticket = line.split(",").map(|s| -> Result<u32, AocError> {Ok(s.parse()?)}).try_collect()?;

    if lines.next().ok_or(AocError{msg: "Unexpected early end of input".into()})?? != "" {
        return Err(AocError{msg: "Unexpected input when expecting empty line".into()});
    }
    if lines.next().ok_or(AocError{msg: "Unexpected early end of input".into()})?? != "nearby tickets:" {
        return Err(AocError{msg: "Unexpected input when expecting \"nearby tickets:\"".into()});
    }
    let mut nearby = Vec::new();
    for line in lines.by_ref() {
        let line = line?;
        nearby.push(line.split(",").map(|s| -> Result<u32, AocError>{Ok(s.parse()?)}).try_collect()?);
    }

    assert_matches!(lines.next(), None);
    return Ok(Input{
        fields: fields,
        ticket: ticket,
        nearby: nearby,
    });
}

fn is_field_value_valid(value: &u32, input: &Input) -> bool {
    input.fields.values()
        .any(|field_ranges| {
            field_ranges.iter()
                .any(|(a, b)| {
                    a <= value && value <= b
                })
        })
}

fn part1(input: &Input) -> u64 {
    input.nearby
        .iter()
        .map(|ticket| { ticket.iter() })
        .flatten()
        .map(|x| {
            if !is_field_value_valid(x, input) {u64::from(*x)} else {0u64}
        })
        .sum()
}

fn part2(input: &Input) -> u64 {
    // Remove invalid tickets.
    let nearby_tickets: Vec<&Ticket> =
        input.nearby.iter()
        .filter(|ticket| {
            ticket.iter().all(|field_value| {
                is_field_value_valid(field_value, input)
            })
        }).collect();

    // Map each field to (index in each ticket of this field if known, vector of value ranges).
    let mut fields: HashMap<String, (Option<usize>, Vec<ValueRange>)> =
        HashMap::from_iter(input.fields.iter().map(|(k, v)| {
            (k.clone(), (None::<usize>, v.clone()))
        }));

    let mut remaining_indices = HashSet::<usize>::from_iter(0..input.ticket.len());
    let mut remaining_fields = HashSet::<String>::from_iter(fields.keys().map(|s| { s.clone() }));

    while !remaining_indices.is_empty() {
        let find_next_field = || -> Option<(String, usize)>{
            for (prospective_field_index, field_name) in itertools::iproduct!(remaining_indices.iter(), remaining_fields.iter()) {
                if fields[field_name].0.is_some() {
                    continue;
                }

                // Is the assignment of the current field to index i valid?
                let field_assignment_valid = |f: &String, i: &usize| -> bool {
                    nearby_tickets.iter().all(|nearby_ticket| {
                        fields[f].1.iter().any(|(a, b)| {
                            *a <= nearby_ticket[*i] && nearby_ticket[*i] <= *b
                        })
                    })
                };

                if field_assignment_valid(field_name, prospective_field_index) {
                    // This assignment is valid.
                    if remaining_indices.iter() // No other index fits this field.
                        .filter(|i| { *i != prospective_field_index })
                        .all(|i| {
                            !field_assignment_valid(field_name, i)
                        })
                    || remaining_fields.iter() // No other field works with this index.
                        .filter(|f| {*f != field_name})
                        .all(|f| {
                            !field_assignment_valid(f, prospective_field_index)
                        })
                    {
                        return Some((field_name.clone(), *prospective_field_index));
                    }
                }
            }

            return None;
        };

        match find_next_field() {
            Some((field_name, field_index)) => {
                match fields.get_mut(&field_name) {
                    Some((i, _)) => {*i = Some(field_index)},
                    None => unreachable!(),
                }
                remaining_fields.remove(&field_name);
                remaining_indices.remove(&field_index);
            },
            None => assert!(false),
        }
    }

    return fields
        .iter()
        .filter(|(k, _)| {
            k.starts_with("departure")
        })
        .map(|(_, (i, _))| {
            u64::from(input.ticket[i.unwrap()])
        })
        .product();
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

    println!("{result}");
    return ExitCode::SUCCESS;
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
    let result = part_fn(&load_input(input_path)?);
    assert_eq!(result, expected_result);
    Ok(())
}
