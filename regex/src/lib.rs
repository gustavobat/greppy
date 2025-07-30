pub fn match_pattern(input_line: &str, pattern: &str) -> bool {
    if pattern.chars().count() == 1 {
        input_line.contains(pattern)
    } else {
        panic!("Unhandled pattern: {pattern}")
    }
}
