use std::io;
use std::process;

use clap::Parser;
use greppy::expression::Expression;
use greppy::solver::Solver;

#[derive(Parser)]
struct Cli {
    #[clap(short = 'E', long)]
    expression: Expression,
}

fn main() {
    let args = Cli::parse();
    let expression = args.expression;

    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();

    let mut solver: Solver = Solver::new(expression, input);
    if solver.solve() {
        solver.print_result();
        process::exit(0);
    }
    println!("No matches found");
    process::exit(1)
}
