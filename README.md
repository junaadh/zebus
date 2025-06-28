# Zebus

This is a personal learning project written in [Zig](https://ziglang.org/), focused on building a custom lexer and frontend primitives for a programming language. It includes support for:

- UTF-8 validated strings
- Character literals with full escape and Unicode handling
- Arena-backed string interning (zero heap allocations in the hot path)
- Diagnostics with spanned tokens
- Tests with emoji, escape codes, and raw Unicode literals

## Inspiration

This project is inspired by [Bitwise](https://github.com/pervognsen/bitwise) by **Per Vognsen**, which explores compiler construction and systems programming from scratch. Many structural ideas are based on his minimalistic and high-performance approach to language tooling.

## Project Status

This is a work-in-progress hobby project. Contributions are not expected, as it's primarily for learning and experimentation.

## License

Unlicensed — feel free to copy anything useful.
