use std::fs;
use std::process::ExitCode;

use itertools::Itertools;
use clap::Parser;

#[derive(Parser, Debug)]
#[clap(version, about)]
struct Args {
    input_path: String,
}

/// From the file at path, read a list of integers (one per line), and return them
/// as a Vec<u32>.
fn load_input(path: String) -> Result<Vec<u32>, String> {
    let file_string = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(err) => {
            return Err(format!("{}.", err));
        },
    };
    match file_string.lines().map(|x| x.parse::<u32>()).collect() {
        Ok(xs) => return Ok(xs),
        Err(err) => {
            return Err(format!("{}.", err));
        },
    };
}

/// Value to which two elements of the input must sum.
const TARGET_TOTAL: u32 = 2020;

/// From the input list of values, find two that sum to TARGET_TOTAL, and return their product.
fn find_match(input: Vec<u32>) -> Result<u32, String> {
    match input.into_iter().combinations(2).find(|x| x.into_iter().sum::<u32>() == TARGET_TOTAL) {
        Some(x) => Ok(x.into_iter().product()),
        None => Err(format!("No pair summing to {} found", TARGET_TOTAL))
    }
}

fn main() -> ExitCode {
    let args = Args::parse();
    let input = match load_input(args.input_path){
        Ok(input) => input,
        Err(msg) => {
            eprintln!("Error: {}.", msg);
            return ExitCode::FAILURE;
        },
    };
    match find_match(input) {
        Ok(x) => {
            println!("{}", x);
            return ExitCode::SUCCESS
        },
        Err(msg) => {
            eprintln!("Error: {}.", msg);
            return ExitCode::FAILURE
        },
    };
}

//TODO add tests
