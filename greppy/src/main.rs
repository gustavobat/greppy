mod search;
mod write;

use clap::Parser;
use clap::ValueEnum;
use regex::Regex;
use search::search_paths;
use search::search_stdin;
use std::io;
use std::io::IsTerminal;
use std::path::PathBuf;
use std::process::ExitCode;

#[derive(Debug, Parser)]
pub struct Cli {
    #[clap(short = 'E', long = "expression")]
    pub regex: Regex,
    pub paths: Vec<PathBuf>,
    #[clap(short = 'o', default_value = "false")]
    pub only_matching: bool,
    #[clap(short = 'r', default_value = "false")]
    pub recursive: bool,
    #[clap(long, value_enum, default_value = "never")]
    pub color: Color,
}

impl Cli {
    pub fn show_file_prefix(&self) -> bool {
        !self.paths.is_empty() && (self.paths.len() > 1 || self.recursive)
    }
}

#[derive(ValueEnum, Clone, Copy, Debug)]
pub enum Color {
    Always,
    Auto,
    Never,
}

impl Color {
    pub fn init(self) {
        match self {
            Color::Always => owo_colors::set_override(true),
            Color::Auto => owo_colors::set_override(io::stdout().is_terminal()),
            Color::Never => owo_colors::set_override(false),
        }
    }
}

fn main() -> ExitCode {
    let config = Cli::parse();
    config.color.init();

    let found = if config.paths.is_empty() {
        search_stdin(&config)
    } else {
        search_paths(&config)
    };

    if found {
        ExitCode::SUCCESS
    } else {
        eprintln!("No matches found.");
        ExitCode::FAILURE
    }
}
