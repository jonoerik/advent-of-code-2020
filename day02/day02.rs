#![feature(custom_test_frameworks)]
#![test_runner(datatest::runner)]

use std::fs;
use std::process::ExitCode;
use std::path::{Path, PathBuf};

use clap::Parser;
use regex::Regex;

#[derive(Parser, Debug)]
#[clap()]
struct Args {
    input_path: PathBuf,
    #[clap(short = '1', long, action)]
    part1: bool,
    #[clap(short = '2', long, action)]
    part2: bool,
}

/// A single element of the puzzle input.
struct InputEntry {
    required_min: usize,
    required_max: usize,
    required_char: char,
    password: String,
}

/// From the provided filesystem path, load the puzzle input.
fn load_input(path: &Path) -> Result<Vec<InputEntry>, String> {
    let input_re = Regex::new(r"^(?P<min>[0-9]+)-(?P<max>[0-9]+) (?P<char>.): (?P<password>.+)$").unwrap();
    let input_file = match fs::read_to_string(path) {
        Ok(f) => f,
        Err(err) => {
            return Err(format!("{}", err));
        },
    };
    return input_file.lines().map(|line| {
        match input_re.captures(line) {
            Some(captures) => Ok(InputEntry {
                required_min: captures.name("min").unwrap().as_str().parse().unwrap(),
                required_max: captures.name("max").unwrap().as_str().parse().unwrap(),
                required_char: captures.name("char").unwrap().as_str().parse().unwrap(),
                password: captures.name("password").unwrap().as_str().to_string(),
            }),
            None => Err(format!("Input line \"{}\" is not of expected format", line))
        }
    }).collect();
}

/// Return a count of the number of provided entries that match their respective part1 password policies.
fn check_entries_p1(entries: &Vec<InputEntry>) -> usize {
    entries.into_iter().map(|entry| if check_entry_p1(entry) {1usize} else {0usize}).sum()
}

/// Return true if the single provided entry is valid according to its own part1 password policy, false otherwise.
fn check_entry_p1(entry: &InputEntry) -> bool {
    (entry.required_min..=entry.required_max).contains(&entry.password.chars().filter(|c| *c == entry.required_char).count())

}

/// Return a count of the number of provided entries that match their respective part2 password policies.
fn check_entries_p2(entries: &Vec<InputEntry>) -> usize {
    entries.into_iter().map(|entry| if check_entry_p2(entry) {1usize} else {0usize}).sum()
}

/// Return true if the single provided entry is valid according to its own part2 password policy, false otherwise.
fn check_entry_p2(entry: &InputEntry) -> bool {
    unimplemented!();
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

    let check_fn = match (args.part1, args.part2) {
        (true, false) => check_entries_p1,
        (false, true) => check_entries_p2,
        _ => {
            eprintln!("Error: must specify exactly one of --part1 or --part2.");
            return ExitCode::FAILURE;
        },
    };

    println!("{}", check_fn(&input));
    return ExitCode::SUCCESS;
}

#[datatest::files("data", {
    input in r"^data/sample([0-9]+)$",
    answer = r"data/sample${1}.answer1",
})]
fn tests_p1(input: &Path, answer: &str) {
    let input = load_input(input).unwrap();
    let answer = answer.parse::<usize>().unwrap();
    assert_eq!(check_entries_p1(&input), answer);
}

#[datatest::files("data", {
    input in r"^data/sample([0-9]+)$",
    answer = r"data/sample${1}.answer2",
})]
fn tests_p2(input: &Path, answer: &str) {
    let input = load_input(input).unwrap();
    let answer = answer.parse::<usize>().unwrap();
    assert_eq!(check_entries_p2(&input), answer);
}
