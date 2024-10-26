use std::io;
use std::process;

use greppy::expression::Expression;
use greppy::validation::Validation;

use clap::Parser;

#[derive(Parser)]
struct Cli {
    #[clap(short = 'E', long)]
    expression: Expression,
}

fn main() {
    let args = Cli::parse();
    let expression = args.expression;

    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();

    if expression.validate(&input_line).is_ok() {
        process::exit(0)
    };
    process::exit(1)
}
