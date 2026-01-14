use crate::Cli;
use matcher::Matcher;
use matcher::SourceSpan;
use owo_colors::OwoColorize;
use owo_colors::Stream::Stdout;
use owo_colors::Style;
use std::io;
use std::io::Write;
use std::path::Path;

fn write_highlighted(writer: &mut impl Write, text: &str) -> io::Result<()> {
    let highlight_style = Style::new().bold().red();
    write!(
        writer,
        "{}",
        text.if_supports_color(Stdout, |t| t.style(highlight_style))
    )
}

pub fn process_line(cli: &Cli, line: &str, file_prefix: Option<&Path>) -> bool {
    let matcher = Matcher::new(&cli.regex, line);
    let matches = matcher.solve();
    if matches.is_empty() {
        return false;
    }

    let source_spans = Matcher::merge_matches(&matches);

    let mut stdout = io::stdout().lock();
    let _ = if cli.only_matching {
        write_only_matching(
            &mut stdout,
            line,
            &source_spans,
            file_prefix,
            cli.show_file_prefix(),
        )
    } else {
        write_complete_line(
            &mut stdout,
            line,
            &source_spans,
            file_prefix,
            cli.show_file_prefix(),
        )
    };

    true
}

fn write_only_matching(
    writer: &mut impl Write,
    line: &str,
    source_spans: &[SourceSpan],
    file_prefix: Option<&Path>,
    show_prefix: bool,
) -> io::Result<()> {
    for span in source_spans {
        if let Some(path) = file_prefix
            && show_prefix
        {
            write!(writer, "{}:", path.display())?;
        }
        write_highlighted(writer, span.substr(line))?;
        writeln!(writer)?;
    }
    Ok(())
}

fn write_complete_line(
    writer: &mut impl Write,
    line: &str,
    source_spans: &[SourceSpan],
    file_prefix: Option<&Path>,
    show_prefix: bool,
) -> io::Result<()> {
    if source_spans.is_empty() {
        return Ok(());
    }

    if let Some(path) = file_prefix
        && show_prefix
    {
        write!(writer, "{}:", path.display())?;
    }

    let mut last_end = 0;
    for span in source_spans {
        if span.start > last_end {
            let before_span = SourceSpan::new(last_end, span.start);
            write!(writer, "{}", before_span.substr(line))?;
        }

        write_highlighted(writer, span.substr(line))?;
        last_end = span.end;
    }

    if last_end < line.chars().count() {
        let after_span = SourceSpan::new(last_end, line.chars().count());
        write!(writer, "{}", after_span.substr(line))?;
    }

    writeln!(writer)
}
