use std::fs;
use std::process::ExitCode;

use itertools::Itertools;
use clap::Parser;

#[derive(Parser, Debug)]
#[clap(version, about)]
struct Args {
    input_path: String,
}

const TARGET_TOTAL: u32 = 2020;

fn find_match(input: Vec<u32>) -> Result<u32, String> {
    match input.into_iter().combinations(2).find(|x| x.into_iter().sum::<u32>() == TARGET_TOTAL) {
        Some(x) => Ok(x.into_iter().product()),
        None => Err(format!("No pair summing to {} found", TARGET_TOTAL))
    }
}

fn main() -> ExitCode {
    let args = Args::parse();

    let file_string = match fs::read_to_string(args.input_path) {
        Ok(s) => s,
        Err(err) => {
            eprintln!("IO Error: {}.", err);
            return ExitCode::FAILURE
        },
    };
    let input = match file_string.lines().map(|x| x.parse::<u32>()).collect() {
        Ok(xs) => xs,
        Err(err) => {
            eprintln!("Error: {}.", err);
            return ExitCode::FAILURE;
        },
    };
    match find_match(input) {
        Ok(x) => {
            println!("{}", x);
            ExitCode::SUCCESS
        },
        Err(msg) => {
            eprintln!("Error: {}.", msg);
            ExitCode::FAILURE
        },
    }
}

//TODO add tests
