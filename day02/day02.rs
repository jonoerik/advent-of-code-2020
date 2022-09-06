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
    //#[clap(short = '1', long, action)]
    //part1: bool,
    //#[clap(short = '2', long, action)]
    //part2: bool,
}

struct InputEntry {
    required_min: usize,
    required_max: usize,
    required_char: char,
    password: String,
}

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

fn check_entries(entries: &Vec<InputEntry>) -> usize {
    entries.into_iter().map(|entry| if check_entry(entry) {1usize} else {0usize}).sum()
}

fn check_entry(entry: &InputEntry) -> bool {
    (entry.required_min..=entry.required_max).contains(&entry.password.chars().filter(|c| *c == entry.required_char).count())
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
    println!("{}", check_entries(&input));
    return ExitCode::SUCCESS;
}

#[datatest::files("data", {
    input in r"^data/sample([0-9]+)$",
    answer = r"data/sample${1}.answer",
})]
fn tests(input: &Path, answer: &str) {
    let input = load_input(input).unwrap();
    let answer = answer.parse::<usize>().unwrap();
    assert_eq!(check_entries(&input), answer);
}
