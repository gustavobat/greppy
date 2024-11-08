use clap::Parser;
use greppy::matcher::Matcher;
use greppy::regex::Regex;
use std::io;
use std::process;

#[derive(Parser)]
struct Cli {
    #[clap(short = 'E', long = "expression")]
    regex: Regex,
}

fn main() {
    let args = Cli::parse();
    let expression = args.regex;

    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();

    let matcher: Matcher = Matcher::new(expression, input);
    let results = matcher.solve();
    if !results.matches.is_empty() {
        let mut stdout = io::stdout();
        results.print_result(&mut stdout).unwrap();
        process::exit(0);
    }
    println!("No matches found");
    process::exit(1)
}
