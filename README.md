# FlagSamurai

A command-line tool for decoding SAM-style bit flags.

## Installation

From the project directory:

```bash
cargo install --path .
```

This installs two executables with identical behavior:

- **`flagsamurai`** — full name
- **`flagsam`** — short alias

## Usage

Invoke with either binary:

```bash
flagsamurai [OPTIONS] [FLAG] [SUBCOMMAND]...
flagsam [OPTIONS] [FLAG] [SUBCOMMAND]...
```

Examples below use `flagsamurai`; you can substitute `flagsam`.

- Explain a flag value: `flagsamurai 99` or `flagsamurai explain 99`
- Compare two values: `flagsamurai diff 99 29`
- Interactive selection: `flagsamurai select`
- Common combinations: `flagsamurai common`

Run `flagsamurai --help` (or `flagsam --help`) for options and subcommands.
