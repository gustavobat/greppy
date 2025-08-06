use clap::Parser;
use matcher::Matcher;
use regex::Regex;
use std::io;
use std::path::PathBuf;
use std::process::ExitCode;

#[derive(Debug, Parser)]
struct Cli {
    #[clap(short = 'E', long = "expression")]
    regex: Regex,
    paths: Vec<PathBuf>,
    #[clap(short = 'r', default_value = "false")]
    recursive: bool,
}

fn solve_from_stdin(expression: &Regex) -> bool {
    let mut stdin_buffer = String::new();
    if io::stdin().read_line(&mut stdin_buffer).is_err() {
        eprintln!("No input data received. Please provide an input file or pipe data to stdin.");
        return false;
    }
    let solution = Matcher::new(expression, &stdin_buffer).solve();
    if !solution.is_empty() {
        println!("{stdin_buffer}");
        return true;
    }
    false
}

fn solve_file(expression: &Regex, file: &PathBuf, multiple: bool) -> bool {
    let Ok(content) = std::fs::read_to_string(file) else {
        eprintln!("Could not read file: {}", file.display());
        return false;
    };
    let file_name = if multiple { Some(file) } else { None };
    let mut match_found = false;
    for line in content.lines() {
        if !Matcher::new(expression, line).solve().is_empty() {
            if let Some(ancestors) = file_name {
                println!("{}:{}", ancestors.display(), line);
            } else {
                println!("{line}");
            }
            match_found = true;
        }
    }
    match_found
}

fn solve_dir(expression: &Regex, dir: &PathBuf) -> bool {
    let Ok(entries) = std::fs::read_dir(dir) else {
        eprintln!("Could not read directory: {}", dir.display());
        return false;
    };

    let mut match_found = false;
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_file() {
            let path = dir.join(path.file_name().unwrap());
            let solution = solve_file(expression, &path, true);
            if solution {
                match_found = true;
            }
        } else if path.is_dir() {
            let new_dir_path = dir.join(path.file_name().unwrap());
            if solve_dir(expression, &new_dir_path) {
                match_found = true;
            }
        }
    }
    match_found
}

fn solve_all_paths(expression: &Regex, paths: &[PathBuf], recursive: bool) -> bool {
    let mut match_found = false;
    for path in paths {
        if path.is_file() {
            let multiple = paths.len() > 1;
            if solve_file(expression, path, multiple) {
                match_found = true;
            }
        } else if path.is_dir() && recursive && solve_dir(expression, path) {
            match_found = true;
        }
    }
    match_found
}

fn main() -> ExitCode {
    let args = Cli::parse();
    let regex = args.regex;

    let found = if args.paths.is_empty() {
        solve_from_stdin(&regex)
    } else {
        solve_all_paths(&regex, &args.paths, args.recursive)
    };

    if found {
        ExitCode::SUCCESS
    } else {
        eprintln!("No matches found.");
        ExitCode::FAILURE
    }
}
