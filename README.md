# FlagSamurai

A command-line tool for decoding SAM-style bit flags.

## Rationale

I made this because I found errors when working with `samtools flags` (e.g. `samtools flags 0xFFF` should be the maximum allowable value (flag 4095 set, corresponding to all flags), but all values up to `samtools flags 0xFFFF` will produce a result. The [docs](https://www.htslib.org/doc/samtools-flags.html) also mention supporting octal notation but it does not work).

Additionally, I made it as comprehensive as possible to be able to analyze SAM flags and learn about them all inside the terminal.

| Bit     | Flag                                                    |
| ------- | ------------------------------------------------------- |
| `0x1`   | <font color="#008800">read paired</font>                |
| `0x2`   | <font color="#a67c00">read mapped in proper pair</font> |
| `0x4`   | <font color="#2244cc">read unmapped</font>              |
| `0x8`   | <font color="#aa00aa">mate unmapped</font>              |
| `0x10`  | <font color="#cc0000">read reverse strand</font>        |
| `0x20`  | <font color="#008888">mate reverse strand</font>        |
| `0x40`  | <font color="#00aa00">first in pair</font>              |
| `0x80`  | <font color="#c49212">second in pair</font>             |
| `0x100` | <font color="#5555ee">not primary alignment</font>      |
| `0x200` | <font color="#cc44cc">fails quality checks</font>       |
| `0x400` | <font color="#ee4444">PCR/optical duplicate</font>      |
| `0x800` | <font color="#00aaaa">supplementary alignment</font>    |

## Installation

### Quick Install

```bash
cargo install flagsamurai
```

### Compile locally

Clone the repo and then from the project directory:

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
- Plain output for scripts: `flagsamurai --suppress-warnings --color never 99`

Run `flagsamurai --help` (or `flagsam --help`) for options and subcommands.
