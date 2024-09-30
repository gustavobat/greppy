use std::env;
use std::io;
use std::process;
use std::str::FromStr;

use codecrafters_grep::expression::Expression;
use codecrafters_grep::validation::Validation;

// Usage: echo <input_text> | your_program.sh -E <pattern>
fn main() {
    if env::args().nth(1).unwrap() != "-E" {
        println!("Expected first argument to be '-E'");
        process::exit(1);
    }

    let expression_raw = env::args().nth(2).unwrap();
    let Ok(expression) = Expression::from_str(&expression_raw) else {
        println!("Invalid pattern: {}", expression_raw);
        process::exit(1);
    };

    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();

    if expression.validate(&input_line) {
        process::exit(0)
    } else {
        process::exit(1)
    }
}
