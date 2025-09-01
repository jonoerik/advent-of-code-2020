#![feature(custom_test_frameworks)]
#![test_runner(datatest::runner)]

use std::collections::HashMap;
use std::collections::hash_map::Entry as HashMapEntry;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;

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

type Num = usize;

fn load_input(path: &Path) -> Result<Vec<Num>, String> {
    let input_file = match fs::read_to_string(path) {
        Ok(f) => f,
        Err(err) => {
            return Err(format!("{}", err));
        },
    };
    match input_file.trim().split(",").map(|s| -> Result<Num, _> {s.parse::<Num>()}).collect() {
        Ok(r) => Ok(r),
        Err(e) => Err(e.to_string()),
    }
}

fn part1(input: &Vec<Num>) -> Num {
    assert!(input.len() > 0);
    // Map of number to turn on which that number was last spoken.
    let mut hm = HashMap::<Num, Num>::new();
    let mut next = 0;
    for (i, &x) in input.iter().enumerate() {
        let i = i + 1;
        hm.entry(x)
            .and_modify(|e| {
                next = i - *e;
                *e = i;
            }).or_insert_with(|| {
                next = 0;
                i
            });
    };

    for i in (input.len()+1) .. 2020 {
        match hm.entry(next) {
            HashMapEntry::Occupied(mut e) => {
                next = i - e.get();
                *e.get_mut() = i;
            },
            HashMapEntry::Vacant(e) => {
                next = 0;
                e.insert(i);
            },
        };
    };

    return next;
}

fn part2(input: &Vec<Num>) -> Num {
    todo!();
}

fn main() -> ExitCode {
    let args = Args::parse();
    let input = match load_input(&args.input_path) {
        Ok(input) => input,
        Err(msg) => {
            eprintln!("Error: {}.", msg);
            return ExitCode::FAILURE;
        }
    };

    let result = match (args.part1, args.part2) {
        (true, false) => part1(&input),
        (false, true) => part2(&input),
        _ => unreachable!(),
    };

    println!("{}", result);
    return ExitCode::SUCCESS;
}

#[datatest::files("data", {
    expected_result in r"^(data/sample[[:digit:]]+)\.answer(?:1|2)$",
    input_path = r"${1}",
    result_path = r"${0}",
})]
#[test]
fn test_case(input_path: &Path, expected_result: &str, result_path: &Path) -> Result<(), String> {
    let expected_result = match expected_result.trim().parse::<Num>() {
        Ok(v) => Ok(v),
        Err(e) => Err(e.to_string()),
    }?;
    let part_fn = if result_path.extension().unwrap().to_str().unwrap().ends_with("1") {part1} else {part2};
    assert_eq!(part_fn(&load_input(input_path)?), expected_result);
    Ok(())
}
