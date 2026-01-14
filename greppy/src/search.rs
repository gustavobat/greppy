use crate::Cli;
use crate::write::process_line;
use std::io::BufRead;
use std::path::Path;

pub fn search_stdin(cli: &Cli) -> bool {
    let stdin = std::io::stdin();
    let mut lines = stdin.lock().lines().peekable();

    if lines.peek().is_none() {
        eprintln!("No input data received. Please provide an input file or pipe data to stdin.");
        return false;
    }

    let mut match_found = false;
    for line in lines.flatten() {
        if process_line(cli, &line, None) {
            match_found = true;
        }
    }

    match_found
}

pub fn search_paths(cli: &Cli) -> bool {
    let mut match_found = false;
    for path in &cli.paths {
        if path.is_file() {
            if search_file(cli, path) {
                match_found = true;
            }
        } else if path.is_dir() && cli.recursive && search_dir_recursive(cli, path) {
            match_found = true;
        }
    }
    match_found
}

pub fn search_file(cli: &Cli, file: &Path) -> bool {
    let Ok(content) = std::fs::read_to_string(file) else {
        eprintln!("Could not read file: {}", file.display());
        return false;
    };

    let file_prefix = if cli.show_file_prefix() {
        Some(file)
    } else {
        None
    };
    let mut match_found = false;

    for line in content.lines() {
        if process_line(cli, line, file_prefix) {
            match_found = true;
        }
    }

    match_found
}

pub fn search_dir_recursive(cli: &Cli, dir: &Path) -> bool {
    let Ok(entries) = std::fs::read_dir(dir) else {
        eprintln!("Could not read directory: {}", dir.display());
        return false;
    };

    let mut match_found = false;
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_file() {
            if search_file(cli, &path) {
                match_found = true;
            }
        } else if path.is_dir() && search_dir_recursive(cli, &path) {
            match_found = true;
        }
    }
    match_found
}
