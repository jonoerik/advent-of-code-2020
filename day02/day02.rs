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
    requirement_a: usize,
    requirement_b: usize,
    required_char: char,
    password: String,
}

/// From the provided filesystem path, load the puzzle input.
fn load_input(path: &Path) -> Result<Vec<InputEntry>, String> {
    let input_re = Regex::new(r"^(?P<a>[0-9]+)-(?P<b>[0-9]+) (?P<char>.): (?P<password>.+)$").unwrap();
    let input_file = match fs::read_to_string(path) {
        Ok(f) => f,
        Err(err) => {
            return Err(format!("{}", err));
        },
    };
    return input_file.lines().map(|line| {
        match input_re.captures(line) {
            Some(captures) => Ok(InputEntry {
                requirement_a: captures.name("a").unwrap().as_str().parse().unwrap(),
                requirement_b: captures.name("b").unwrap().as_str().parse().unwrap(),
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
    (entry.requirement_a..=entry.requirement_b).contains(&entry.password.chars().filter(|c| *c == entry.required_char).count())

}

/// Return a count of the number of provided entries that match their respective part2 password policies.
fn check_entries_p2(entries: &Vec<InputEntry>) -> Result<usize, String> {
    entries.into_iter().fold(Ok(0usize),
        |a, b| Ok(a? + if check_entry_p2(b)? {1usize} else {0usize})
    )
}

/// Return true if the single provided entry is valid according to its own part2 password policy, false otherwise.
fn check_entry_p2(entry: &InputEntry) -> Result<bool, String> {
    let char_a = match entry.password.chars().nth(entry.requirement_a - 1) {
        Some(c) => c,
        None => {
            return Err(format!("Index {} is outside password string \"{}\"", entry.requirement_a, entry.password));
        },
    };
    let char_b = match entry.password.chars().nth(entry.requirement_b - 1) {
        Some(c) => c,
        None => {
            return Err(format!("Index {} is outside password string \"{}\"", entry.requirement_b, entry.password));
        },
    };
    return Ok((char_a == entry.required_char) != (char_b == entry.required_char));
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

    let result =
        if args.part1 && !args.part2 {
            check_entries_p1(&input)
        } else if !args.part1 && args.part2 {
            match check_entries_p2(&input) {
                Ok(result) => result,
                Err(msg) => {
                    eprintln!("Error: {}.", msg);
                    return ExitCode::FAILURE;
                },
            }
        } else {
            eprintln!("Error: must specify exactly one of --part1 or --part2.");
            return ExitCode::FAILURE;
        };

    println!("{}", result);
    return ExitCode::SUCCESS;
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::Path;

    #[test]
    fn sample1_p1() {
        let input = super::load_input(&Path::new("data").join("sample1")).unwrap();
        let answer = fs::read_to_string(&Path::new("data").join("sample1.answer1")).unwrap().parse::<usize>().unwrap();
        assert_eq!(super::check_entries_p1(&input), answer);
    }

    #[test]
    fn sample1_p2() {
        let input = super::load_input(&Path::new("data").join("sample1")).unwrap();
        let answer = fs::read_to_string(&Path::new("data").join("sample1.answer2")).unwrap().parse::<usize>().unwrap();
        assert_eq!(super::check_entries_p2(&input).unwrap(), answer);
    }
}
