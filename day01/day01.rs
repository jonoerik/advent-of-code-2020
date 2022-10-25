use std::fs;
use std::process::ExitCode;
use std::path::{Path, PathBuf};

use itertools::Itertools;
use clap::Parser;

#[derive(Parser, Debug)]
#[clap()]
struct Args {
    input_path: PathBuf,
    #[clap(short = '1', long, action)]
    part1: bool,
    #[clap(short = '2', long, action)]
    part2: bool,
}

/// From the file at path, read a list of integers (one per line), and return them
/// as a Vec<u32>.
fn load_input(path: &Path) -> Result<Vec<u32>, String> {
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

/// From the input list of values, find a set of size subset_size that sums to TARGET_TOTAL, and return their product.
fn find_match(input: &Vec<u32>, subset_size: usize) -> Result<u32, String> {
    match input.into_iter().combinations(subset_size).find(|x| x.iter().copied().sum::<u32>() == TARGET_TOTAL) {
        Some(x) => Ok(x.into_iter().product()),
        None => Err(format!("No pair summing to {} found", TARGET_TOTAL))
    }
}

fn main() -> ExitCode {
    let args = Args::parse();

    // Number of elements from the input set that will be combined into the output set.
    let input_subset_size = match (args.part1, args.part2) {
        (true, false) => 2,
        (false, true) => 3,
        _ => {
            eprintln!("Error: must specify exactly one of --part1 or --part2.");
            return ExitCode::FAILURE;
        },
    };
    let input = match load_input(&args.input_path){
        Ok(input) => input,
        Err(msg) => {
            eprintln!("Error: {}.", msg);
            return ExitCode::FAILURE;
        },
    };
    match find_match(&input, input_subset_size) {
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

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::Path;

    // https://stackoverflow.com/questions/34662713/how-can-i-create-parameterized-tests-in-rust/34666891#34666891
    macro_rules! test_cases {
        ($($name:ident,)*) => {
            $(
                #[test]
                fn $name() {
                    let input = super::load_input(&Path::new("data").join(stringify!($name))).unwrap();
                    let answer = fs::read_to_string(&Path::new("data").join(concat!(stringify!($name), ".answer"))).unwrap().parse::<u32>().unwrap();
                    let input_subset_size = fs::read_to_string(&Path::new("data").join(concat!(stringify!($name), ".subset_size"))).unwrap().parse::<usize>().unwrap();
                    assert_eq!(super::find_match(&input, input_subset_size).unwrap(), answer);
                }
            )*
        }
    }

    test_cases! {
        sample1,
        sample2,
    }
}
