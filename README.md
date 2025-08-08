# greppy

A Rust implementation of a `grep`-like tool, built as an attempt to solve
[Codecrafters "_Build Your Own grep_" Challenge](https://app.codecrafters.io/courses/grep/overview).

# Usage

Piping from standard input:

```sh
echo "123 foo 456 bar 123 baz 456" | cargo run --release -- -E "(\d+).*(\d+).*\1.*\2"
```

`> 123 foo 456 bar 123 baz 456`

From a file:

```sh
echo "How many yaks could a yak-pack pack if a yak-pack could pack yaks?" > file.txt
cargo run --release -- -E "How many ((yak)s) could a (\2-(pack)) \4 if a \3 could \4 \1?" file.txt
```

`> file.txt:How many yaks could a yak-pack pack if a yak-pack could pack yaks?`

From multiple files/directories (with recursive `-r` option enabled):

```sh
mkdir -p dir/subdir
echo "🦦 🦦 🦦" > dir/subdir/file.txt
echo "🐼 🐼 🐼" > another_file.txt
cargo run --release -- -r -E "(.*) \1 \1" dir another_file.txt
```

```
> dir/subdir/file.txt:🦦 🦦 🦦
> another_file.txt:🐼 🐼 🐼
```
