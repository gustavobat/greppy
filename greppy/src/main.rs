use clap::Parser;
use matcher::Matcher;
use regex::Regex;
use std::collections::HashMap;
use std::io;
use std::path::PathBuf;
use std::process;

#[derive(Debug, Parser)]
struct Cli {
    #[clap(short = 'E', long = "expression")]
    regex: Regex,
    file: Vec<PathBuf>,
    #[clap(short = 'r', default_value = "false")]
    recursive: bool,
}

fn solve_string(expression: &Regex, input: &str) -> bool {
    let matcher: Matcher = Matcher::new(expression.clone(), input.to_string());
    let results = matcher.solve();
    !results.is_empty()
}

fn solve_file(expression: &Regex, file: &PathBuf) -> Vec<String> {
    let content = std::fs::read_to_string(file).unwrap_or_else(|_| {
        eprintln!("Could not read file: {}", file.display());
        process::exit(1);
    });
    let mut matched_lines = vec![];
    for line in content.lines() {
        if solve_string(expression, line) {
            matched_lines.push(line.to_string());
        }
    }
    matched_lines
}

fn solve_dir(expression: &Regex, dir: &PathBuf) -> HashMap<PathBuf, Vec<String>> {
    let mut matched_lines = HashMap::new();
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file() {
                let file_matches = solve_file(expression, &path);
                if !file_matches.is_empty() {
                    matched_lines.insert(path.clone(), file_matches);
                }
            } else if path.is_dir() {
                matched_lines.extend(solve_dir(expression, &path));
            }
        }
    } else {
        eprintln!("Could not read directory: {}", dir.display());
    }
    matched_lines
}

fn main() {
    let args = Cli::parse();
    let expression = args.regex;

    let mut match_found = false;
    if args.file.is_empty() {
        let mut input = String::new();
        let Ok(_) = io::stdin().read_line(&mut input) else {
            eprintln!("No input data received. Please proved an input file or pipe data to stdin.");
            process::exit(1);
        };
        if solve_string(&expression, &input) {
            println!("{input}");
            match_found = true;
        }
    } else {
        for path in &args.file {
            if path.is_dir() && args.recursive {
                let dir_matches = solve_dir(&expression, path);
                if !dir_matches.is_empty() {
                    match_found = true;
                }
                for (path, matches) in dir_matches {
                    for line in matches {
                        println!("{}:{}", path.to_string_lossy(), line);
                    }
                }
            } else {
                let matched_lines = solve_file(&expression, path);
                if !matched_lines.is_empty() {
                    match_found = true;
                    for line in matched_lines {
                        let file_name = path.file_name().expect("Path should not be a directory");
                        if args.file.len() > 1 {
                            println!("{}:{}", file_name.to_string_lossy(), line);
                        } else {
                            println!("{line}");
                        }
                    }
                }
            }
        }
    }

    if match_found {
        process::exit(0);
    } else {
        eprintln!("No matches found.");
        process::exit(1)
    }
}
