use clap::{Args, Parser, Subcommand};
use crossterm::{
    cursor,
    event::{self, Event, KeyCode},
    execute,
    style::{Attribute, Color, Print, SetAttribute, SetForegroundColor},
    terminal::{self, Clear, ClearType},
};
use std::io::{self, Write};
use termcolor::{Color as TermColor, ColorChoice, ColorSpec, StandardStream, WriteColor};

/// When to use terminal colours: always, auto (detect TTY), or never.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Default)]
enum ColorWhen {
    Always,
    #[default]
    Auto,
    Never,
}

impl std::str::FromStr for ColorWhen {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.eq_ignore_ascii_case("always") {
            true => Ok(ColorWhen::Always),
            false if s.eq_ignore_ascii_case("auto") => Ok(ColorWhen::Auto),
            false if s.eq_ignore_ascii_case("never") => Ok(ColorWhen::Never),
            _ => Err(format!(
                "invalid value '{s}' for '--color <WHEN>'\n  [possible values: always, auto, never]"
            )),
        }
    }
}

fn color_choice(when: ColorWhen) -> ColorChoice {
    match when {
        ColorWhen::Always => ColorChoice::AlwaysAnsi,
        ColorWhen::Auto => ColorChoice::Auto,
        ColorWhen::Never => ColorChoice::Never,
    }
}

/// Write a styled segment to a termcolor stream. Caller is responsible for newlines and flush.
fn write_style(w: &mut dyn WriteColor, spec: &ColorSpec, s: &str) -> io::Result<()> {
    w.set_color(spec)?;
    w.write_all(s.as_bytes())?;
    w.reset()?;
    Ok(())
}

fn write_bold(w: &mut dyn WriteColor, s: &str) -> io::Result<()> {
    let mut spec = ColorSpec::new();
    spec.set_bold(true);
    write_style(w, &spec, s)
}

fn write_dimmed(w: &mut dyn WriteColor, s: &str) -> io::Result<()> {
    let mut spec = ColorSpec::new();
    spec.set_dimmed(true);
    write_style(w, &spec, s)
}

fn write_red(w: &mut dyn WriteColor, s: &str) -> io::Result<()> {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(TermColor::Red));
    write_style(w, &spec, s)
}

fn write_yellow(w: &mut dyn WriteColor, s: &str) -> io::Result<()> {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(TermColor::Yellow));
    write_style(w, &spec, s)
}

fn write_yellow_bold(w: &mut dyn WriteColor, s: &str) -> io::Result<()> {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(TermColor::Yellow)).set_bold(true);
    write_style(w, &spec, s)
}

fn write_yellow_bold_underline(w: &mut dyn WriteColor, s: &str) -> io::Result<()> {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(TermColor::Yellow))
        .set_bold(true)
        .set_underline(true);
    write_style(w, &spec, s)
}

fn write_red_bold(w: &mut dyn WriteColor, s: &str) -> io::Result<()> {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(TermColor::Red)).set_bold(true);
    write_style(w, &spec, s)
}

fn write_red_bold_underline(w: &mut dyn WriteColor, s: &str) -> io::Result<()> {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(TermColor::Red))
        .set_bold(true)
        .set_underline(true);
    write_style(w, &spec, s)
}

/// Print an error message: "Error" in red bold underline, then ": " and the message in red bold.
/// Callers pass only the message body; the "Error: " prefix is added here.
fn write_error_message(w: &mut dyn WriteColor, msg: &str) -> io::Result<()> {
    write_red_bold_underline(w, "Error")?;
    write_red_bold(w, ": ")?;
    write_red_bold(w, msg)?;
    Ok(())
}

/// Print a warning message: "Warning" in yellow bold underline, then ": " and the message in yellow bold.
/// Callers pass only the message body; the "Warning: " prefix is added here.
fn write_warning_message(w: &mut dyn WriteColor, msg: &str) -> io::Result<()> {
    write_yellow_bold_underline(w, "Warning")?;
    write_yellow_bold(w, ": ")?;
    write_yellow_bold(w, msg)?;
    Ok(())
}

fn write_blue_bold(w: &mut dyn WriteColor, s: &str) -> io::Result<()> {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(TermColor::Blue)).set_bold(true);
    write_style(w, &spec, s)
}

fn write_green(w: &mut dyn WriteColor, s: &str) -> io::Result<()> {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(TermColor::Green));
    write_style(w, &spec, s)
}

fn write_magenta(w: &mut dyn WriteColor, s: &str) -> io::Result<()> {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(TermColor::Magenta));
    write_style(w, &spec, s)
}

fn write_magenta_bold_underline(w: &mut dyn WriteColor, s: &str) -> io::Result<()> {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(TermColor::Magenta))
        .set_bold(true)
        .set_underline(true);
    write_style(w, &spec, s)
}

fn write_bright_cyan(w: &mut dyn WriteColor, s: &str) -> io::Result<()> {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(TermColor::Cyan)).set_intense(true);
    write_style(w, &spec, s)
}

fn write_bright_black(w: &mut dyn WriteColor, s: &str) -> io::Result<()> {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(TermColor::Black)).set_intense(true);
    write_style(w, &spec, s)
}

/// Bit flag with a human-readable name and a short name for diff output.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct Flag {
    name: &'static str,
    /// Short snake_case name used in diff output (e.g. proper_pair, read_reverse).
    short_name: &'static str,
    bitmask: u16,
}

/// All flags supported by this tool.
///
/// The order here is significant: some operations (like mate switching)
/// treat this as an indexed list.
const FLAGS: [Flag; 12] = [
    Flag {
        name: "read paired",
        short_name: "read_paired",
        bitmask: 0x1,
    },
    Flag {
        name: "read mapped in proper pair",
        short_name: "proper_pair",
        bitmask: 0x2,
    },
    Flag {
        name: "read unmapped",
        short_name: "read_unmapped",
        bitmask: 0x4,
    },
    Flag {
        name: "mate unmapped",
        short_name: "mate_unmapped",
        bitmask: 0x8,
    },
    Flag {
        name: "read reverse strand",
        short_name: "read_reverse",
        bitmask: 0x10,
    },
    Flag {
        name: "mate reverse strand",
        short_name: "mate_reverse",
        bitmask: 0x20,
    },
    Flag {
        name: "first in pair",
        short_name: "first_in_pair",
        bitmask: 0x40,
    },
    Flag {
        name: "second in pair",
        short_name: "second_in_pair",
        bitmask: 0x80,
    },
    Flag {
        name: "not primary alignment",
        short_name: "not_primary",
        bitmask: 0x100,
    },
    Flag {
        name: "fails quality checks",
        short_name: "fails_qc",
        bitmask: 0x200,
    },
    Flag {
        name: "PCR/optical duplicate",
        short_name: "duplicate",
        bitmask: 0x400,
    },
    Flag {
        name: "supplementary alignment",
        short_name: "supplementary",
        bitmask: 0x800,
    },
];

/// Bitmask of flags that are only valid for paired reads.
///
/// This matches the pseudocode constant 235 and is the sum of:
/// - read paired (0x1)
/// - read mapped in proper pair (0x2)
/// - mate unmapped (0x8)
/// - mate reverse strand (0x20)
/// - first in pair (0x40)
/// - second in pair (0x80)
const INVALID_FOR_UNPAIRED_MASK: u16 = 0x1 | 0x2 | 0x8 | 0x20 | 0x40 | 0x80;

/// Explanation of a single set flag.
#[derive(Debug, Clone, Eq, PartialEq)]
struct FlagExplanation {
    name: &'static str,
    bitmask: u16,
}

/// Explain the meaning of a numeric flag value.
///
/// Returns all set flags as `summary`, and the subset that are
/// invalid when the read is not paired as `bad_flags`.
fn explain_flags(flag_value: u16) -> (Vec<FlagExplanation>, Vec<FlagExplanation>) {
    let paired_read = (flag_value & 0x1) != 0;
    let mut summary = Vec::new();
    let mut bad_flags = Vec::new();

    for flag in FLAGS {
        if (flag_value & flag.bitmask) != 0 {
            if !paired_read && (flag.bitmask & INVALID_FOR_UNPAIRED_MASK) != 0 {
                bad_flags.push(FlagExplanation {
                    name: flag.name,
                    bitmask: flag.bitmask,
                });
            }
            summary.push(FlagExplanation {
                name: flag.name,
                bitmask: flag.bitmask,
            });
        }
    }

    (summary, bad_flags)
}

/// Compute the numeric flag value from a list of booleans.
fn compute_flag_value(checked_flags: &[bool]) -> u16 {
    let mut flag_value: u16 = 0;
    for (i, checked) in checked_flags.iter().enumerate() {
        if *checked {
            if let Some(flag) = FLAGS.get(i) {
                flag_value |= flag.bitmask;
            }
        }
    }
    flag_value
}

/// Swap read/mate-related flags and recompute the resulting value.
///
/// The swapping behavior follows the pseudocode:
/// - read unmapped (index 2) <-> mate unmapped (index 3)
/// - read reverse strand (index 4) <-> mate reverse strand (index 5)
/// - first in pair (index 6) <-> second in pair (index 7)
fn switch_mate_flags(
    mut checked_flags: Vec<bool>,
) -> (Vec<bool>, u16, Vec<FlagExplanation>, Vec<FlagExplanation>) {
    let swaps = [(2usize, 3usize), (4usize, 5usize), (6usize, 7usize)];

    for (i, j) in swaps {
        if i < checked_flags.len() && j < checked_flags.len() {
            checked_flags.swap(i, j);
        }
    }

    let flag_value = compute_flag_value(&checked_flags);
    let (summary, bad_flags) = explain_flags(flag_value);

    (checked_flags, flag_value, summary, bad_flags)
}

/// Command-line interface definition.
#[derive(Parser, Debug)]
#[command(
    name = "flagsamurai",
    version,
    about = "Inspect and manipulate SAM-style bit flags",
    // Prefer subcommand names like `switch` over treating them as a bare flag
    // value. This lets `flagsamurai switch 99` be parsed as a subcommand call
    // instead of interpreting `switch` as the numeric argument.
    subcommand_precedence_over_arg = true
)]
struct Cli {
    /// When to use terminal colours (always, auto, never).
    #[arg(
        long = "color",
        alias = "colour",
        value_name = "WHEN",
        default_value = "auto",
        global = true
    )]
    color: ColorWhen,

    /// Show information for both reads (original and mate-swapped).
    #[arg(short, long, global = true)]
    full: bool,

    /// Treat the input as an integer and print the corresponding bitmask.
    ///
    /// If the input already looks like a bitmask (e.g. `0x63`), the same
    /// bitmask is printed but a warning is emitted on stderr suggesting
    /// `-i/--int` instead.
    #[arg(short = 'b', long = "bit", global = false)]
    bit: bool,

    /// Treat the input as a bitmask and print the corresponding integer.
    ///
    /// If the input already looks like an integer (e.g. `99`), the same
    /// integer is printed but a warning is emitted on stderr suggesting
    /// `-b/--bit` instead.
    #[arg(short = 'i', long = "int", global = false)]
    int_mode: bool,

    /// Numeric flag value(s) to explain (decimal or hex). Multiple values allowed when no subcommand is used.
    #[arg(num_args = 1.., value_name = "FLAG")]
    flag: Option<Vec<String>>,

    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Explain the meaning of one or more numeric flag values.
    Explain {
        /// Flag value(s) to explain (decimal or hex, e.g. 99 0x63 29).
        #[arg(num_args = 1.., value_name = "FLAG")]
        flags: Vec<String>,
    },
    /// Compute a numeric flag value from individual boolean flags.
    Compute(FlagSelection),
    /// Swap mate-related flags and show the resulting value and explanation.
    ///
    /// This takes an existing numeric flag value, flips the read/mate-related
    /// bits (unmapped, reverse, first/second in pair), and reports the new
    /// value and its explanation.
    Switch {
        /// Numeric flag value (decimal, hex like 0x93 is also accepted).
        flag: String,
    },
    /// Interactive terminal UI for selecting flags via a checklist.
    Select {
        /// Show information for both reads (original and mate-swapped).
        ///
        /// This is equivalent to using the top-level `--full` flag.
        /// Using `--full` either before or after
        /// `select` (or both) has the same effect.
        #[arg(short, long)]
        full: bool,
    },
    /// Show common SAM flag combinations and their meanings.
    Common {
        /// Also show the hex bitmask alongside each decimal value.
        ///
        /// This is equivalent to using the top-level `--full` flag. Using
        /// `--full` either before or after `common` (or both) has the same
        /// effect.
        #[arg(short, long)]
        full: bool,
    },
    /// Explain multiple flag values in one run (decimal and hex can be mixed).
    Evaluate {
        /// Flag values to explain, separated by spaces (e.g. 99 0x63 147).
        #[arg(num_args = 1.., value_name = "FLAG")]
        flags: Vec<String>,
    },
    /// Show which flags differ between two values (+ in second only, - in first only).
    Diff {
        /// Exactly two flag values (decimal or hex), e.g. `99 0x93`.
        #[arg(num_args = 1.., value_name = "FLAG")]
        flags: Vec<String>,
    },
}

/// Selection of individual flags via booleans.
///
/// This mirrors the `checkedFlags` list in the pseudocode, but with
/// descriptive long options for usability.
#[derive(Args, Debug, Default)]
struct FlagSelection {
    /// 0x1 - read paired
    #[arg(long)]
    read_paired: bool,
    /// 0x2 - read mapped in proper pair
    #[arg(long)]
    read_proper_pair: bool,
    /// 0x4 - read unmapped
    #[arg(long)]
    read_unmapped: bool,
    /// 0x8 - mate unmapped
    #[arg(long)]
    mate_unmapped: bool,
    /// 0x10 - read reverse strand
    #[arg(long)]
    read_reverse: bool,
    /// 0x20 - mate reverse strand
    #[arg(long)]
    mate_reverse: bool,
    /// 0x40 - first in pair
    #[arg(long)]
    first_in_pair: bool,
    /// 0x80 - second in pair
    #[arg(long)]
    second_in_pair: bool,
    /// 0x100 - not primary alignment
    #[arg(long)]
    not_primary: bool,
    /// 0x200 - fails quality checks
    #[arg(long)]
    fails_qc: bool,
    /// 0x400 - PCR/optical duplicate
    #[arg(long)]
    duplicate: bool,
    /// 0x800 - supplementary alignment
    #[arg(long)]
    supplementary: bool,
}

impl FlagSelection {
    /// Convert the struct into a boolean vector aligned with `FLAGS`.
    fn to_vec(&self) -> Vec<bool> {
        vec![
            self.read_paired,
            self.read_proper_pair,
            self.read_unmapped,
            self.mate_unmapped,
            self.read_reverse,
            self.mate_reverse,
            self.first_in_pair,
            self.second_in_pair,
            self.not_primary,
            self.fails_qc,
            self.duplicate,
            self.supplementary,
        ]
    }
}

fn parse_flag_value(input: &str) -> Result<u16, String> {
    let trimmed = input.trim();
    let value = if let Some(stripped) = trimmed
        .strip_prefix("0x")
        .or_else(|| trimmed.strip_prefix("0X"))
    {
        u16::from_str_radix(stripped, 16)
            .map_err(|e| format!("Invalid hex flag value '{input}': {e}"))?
    } else {
        trimmed
            .parse::<u16>()
            .map_err(|e| format!("Invalid decimal flag value '{input}': {e}"))?
    };

    if value > 0x0fff {
        Err(format!(
            "Flag value '{input}' is out of range; must be between 0 and 4095 (0x000 – 0xfff)"
        ))
    } else {
        Ok(value)
    }
}

/// Heuristic to decide whether the original string "looks like" a bitmask.
///
/// We treat hex values with a `0x`/`0X` prefix as bitmask-style input and
/// plain decimal numbers as integer input.
fn input_looks_like_bitmask(input: &str) -> bool {
    let trimmed = input.trim();
    trimmed.starts_with("0x") || trimmed.starts_with("0X")
}

/// Convert a numeric flag value into the boolean vector used by
/// `switch_mate_flags`, aligned with the `FLAGS` table.
fn value_to_checked_flags(flag_value: u16) -> Vec<bool> {
    FLAGS
        .iter()
        .map(|flag| (flag_value & flag.bitmask) != 0)
        .collect()
}

/// Print explanation for a single read (no mate-swapping), with color.
fn print_single_read_explanation(flag_value: u16, out: &mut StandardStream) {
    let (summary, bad_flags) = explain_flags(flag_value);

    write_bold(out, "Flag value:").unwrap();
    write!(out, " ").unwrap();
    write_bright_cyan(out, &flag_value.to_string()).unwrap();
    write!(out, " ").unwrap();
    write_bright_black(out, &format!("(0x{:03x})", flag_value)).unwrap();
    writeln!(out).unwrap();
    if summary.is_empty() {
        write_dimmed(out, "No flags are set.").unwrap();
        writeln!(out).unwrap();
    } else {
        write_bold(out, "Set flags:").unwrap();
        writeln!(out).unwrap();
        for f in &summary {
            write!(out, "  - ").unwrap();
            write_bright_cyan(out, &format!("{:>3}", f.bitmask)).unwrap();
            write!(out, " ").unwrap();
            write_bright_black(out, &format!("(0x{:03x})", f.bitmask)).unwrap();
            writeln!(out, ": {}", f.name).unwrap();
        }
    }

    if !bad_flags.is_empty() {
        writeln!(out).unwrap();
        write_warning_message(
            out,
            "The following flags are invalid when the read is not paired:",
        )
        .unwrap();
        writeln!(out).unwrap();
        for f in &bad_flags {
            write!(out, "  - ").unwrap();
            write_red(out, &format!("{:>3}", f.bitmask)).unwrap();
            write!(out, " ").unwrap();
            write_bright_black(out, &format!("(0x{:03x})", f.bitmask)).unwrap();
            write!(out, ": ").unwrap();
            write_red(out, f.name).unwrap();
            writeln!(out).unwrap();
        }
    }
    let _ = out.flush();
}

/// Print explanation, optionally for both reads when `full` is true.
fn print_explanation(flag_value: u16, full: bool, out: &mut StandardStream) {
    if !full {
        print_single_read_explanation(flag_value, out);
        return;
    }

    write_blue_bold(out, "Description of").unwrap();
    write!(out, " ").unwrap();
    write_green(out, "first").unwrap();
    write!(out, " ").unwrap();
    write_blue_bold(out, "read").unwrap();
    writeln!(out).unwrap();
    print_single_read_explanation(flag_value, out);
    writeln!(out).unwrap();

    // Second read: flip read/mate-related bits and recompute.
    let checked = value_to_checked_flags(flag_value);
    let (_swapped, mate_value, _summary, _bad_flags) = switch_mate_flags(checked);

    write_blue_bold(out, "Description of").unwrap();
    write!(out, " ").unwrap();
    write_green(out, "second").unwrap();
    write!(out, " ").unwrap();
    write_blue_bold(out, "read").unwrap();
    writeln!(out).unwrap();
    print_single_read_explanation(mate_value, out);
    let _ = out.flush();
}

/// Print a diff of two flag values: + flags only in second, - flags only in first.
/// Print a human-friendly diff of two flag values: header, common flags,
/// then only-in-second and only-in-first sections.
fn print_diff(first_value: u16, second_value: u16, out: &mut StandardStream) {
    write_bold(out, "Comparing flags:").unwrap();
    write!(out, " ").unwrap();
    write_bright_cyan(out, &first_value.to_string()).unwrap();
    write!(out, " ").unwrap();
    write_bright_black(out, &format!("(0x{:03x})", first_value)).unwrap();
    write!(out, " ").unwrap();
    write_bold(out, "→").unwrap();
    write!(out, " ").unwrap();
    write_bright_cyan(out, &second_value.to_string()).unwrap();
    write!(out, " ").unwrap();
    write_bright_black(out, &format!("(0x{:03x})", second_value)).unwrap();
    writeln!(out).unwrap();
    writeln!(out).unwrap();

    let mut common = Vec::new();
    let mut only_second = Vec::new();
    let mut only_first = Vec::new();

    for flag in FLAGS {
        let in_first = (first_value & flag.bitmask) != 0;
        let in_second = (second_value & flag.bitmask) != 0;
        if in_first && in_second {
            common.push(flag);
        } else if in_second && !in_first {
            only_second.push(flag);
        } else if in_first && !in_second {
            only_first.push(flag);
        }
    }

    write_blue_bold(out, "Common flags:").unwrap();
    writeln!(out).unwrap();
    if common.is_empty() {
        write!(out, "  ").unwrap();
        write_dimmed(out, "(none)").unwrap();
        writeln!(out).unwrap();
    } else {
        for flag in &common {
            write!(out, "  - ").unwrap();
            write_bright_cyan(out, &format!("{:>3}", flag.bitmask)).unwrap();
            write!(out, " (").unwrap();
            write_bright_black(out, &format!("0x{:03x}", flag.bitmask)).unwrap();
            writeln!(out, "): {}", flag.name).unwrap();
        }
    }
    writeln!(out).unwrap();

    // Section: only in first. Decimal in cyan, hex in gray, punctuation in blue.
    write_blue_bold(out, "Only in ").unwrap();
    write_green(out, "first").unwrap();
    write_blue_bold(out, " (").unwrap();
    write_bright_cyan(out, &first_value.to_string()).unwrap();
    write_blue_bold(out, ", ").unwrap();
    write_bright_black(out, &format!("0x{:03x}", first_value)).unwrap();
    write_blue_bold(out, "):").unwrap();
    writeln!(out).unwrap();
    if only_first.is_empty() {
        write!(out, "  ").unwrap();
        write_dimmed(out, "(none)").unwrap();
        writeln!(out).unwrap();
    } else {
        for flag in &only_first {
            write!(out, "  - ").unwrap();
            write_green(out, &format!("{:>3}", flag.bitmask)).unwrap();
            write!(out, " (").unwrap();
            write_bright_black(out, &format!("0x{:03x}", flag.bitmask)).unwrap();
            write!(out, "): ").unwrap();
            write_green(out, flag.name).unwrap();
            writeln!(out).unwrap();
        }
    }
    writeln!(out).unwrap();

    // Section: only in second. Same styling as above.
    write_blue_bold(out, "Only in ").unwrap();
    write_magenta(out, "second").unwrap();
    write_blue_bold(out, " (").unwrap();
    write_bright_cyan(out, &second_value.to_string()).unwrap();
    write_blue_bold(out, ", ").unwrap();
    write_bright_black(out, &format!("0x{:03x}", second_value)).unwrap();
    write_blue_bold(out, "):").unwrap();
    writeln!(out).unwrap();
    if only_second.is_empty() {
        write!(out, "  ").unwrap();
        write_dimmed(out, "(none)").unwrap();
        writeln!(out).unwrap();
    } else {
        for flag in &only_second {
            write!(out, "  - ").unwrap();
            write_magenta(out, &format!("{:>3}", flag.bitmask)).unwrap();
            write!(out, " (").unwrap();
            write_bright_black(out, &format!("0x{:03x}", flag.bitmask)).unwrap();
            write!(out, "): ").unwrap();
            write_magenta(out, flag.name).unwrap();
            writeln!(out).unwrap();
        }
    }

    // Mirror the semantic warnings from `print_single_read_explanation` so
    // that diff also reports invalid flag combinations (e.g. paired-only
    // flags used on unpaired reads).
    let (_summary_first, bad_first) = explain_flags(first_value);
    let (_summary_second, bad_second) = explain_flags(second_value);

    if !bad_first.is_empty() {
        writeln!(out).unwrap();
        write_warning_message(
            out,
            "The following flags are invalid for the first value when the read is not paired:",
        )
        .unwrap();
        writeln!(out).unwrap();
        for f in &bad_first {
            write!(out, "  - ").unwrap();
            write_red(out, &format!("{:>3}", f.bitmask)).unwrap();
            write!(out, " ").unwrap();
            write_bright_black(out, &format!("(0x{:03x})", f.bitmask)).unwrap();
            write!(out, ": ").unwrap();
            write_red(out, f.name).unwrap();
            writeln!(out).unwrap();
        }
    }

    if !bad_second.is_empty() {
        writeln!(out).unwrap();
        write_warning_message(
            out,
            "The following flags are invalid for the second value when the read is not paired:",
        )
        .unwrap();
        writeln!(out).unwrap();
        for f in &bad_second {
            write!(out, "  - ").unwrap();
            write_red(out, &format!("{:>3}", f.bitmask)).unwrap();
            write!(out, " ").unwrap();
            write_bright_black(out, &format!("(0x{:03x})", f.bitmask)).unwrap();
            write!(out, ": ").unwrap();
            write_red(out, f.name).unwrap();
            writeln!(out).unwrap();
        }
    }
    let _ = out.flush();
}

/// Print a nicely formatted, colorized summary of common SAM flag combinations.
///
/// When `with_bitmasks` is true, also show the hex bitmask alongside each
/// decimal value.
fn print_common_flags(with_bitmasks: bool, out: &mut StandardStream) {
    // Helper to print a section header and a list of codes.
    fn print_section(title: &str, codes: &[u16], with_bitmasks: bool, out: &mut StandardStream) {
        write_blue_bold(out, title).unwrap();
        writeln!(out).unwrap();
        write!(out, "  ").unwrap();
        for (i, c) in codes.iter().enumerate() {
            if i > 0 {
                write!(out, ", ").unwrap();
            }
            if with_bitmasks {
                write_bright_cyan(out, &c.to_string()).unwrap();
                write!(out, " ").unwrap();
                write_bright_black(out, &format!("(0x{:03x})", c)).unwrap();
            } else {
                write_bright_cyan(out, &c.to_string()).unwrap();
            }
        }
        writeln!(out).unwrap();
        writeln!(out).unwrap();
    }

    write_magenta_bold_underline(out, "Common flags*").unwrap();
    writeln!(out).unwrap();
    writeln!(out).unwrap();

    print_section(
        "One of the reads is unmapped:",
        &[73, 133, 89, 121, 165, 181, 101, 117, 153, 185, 69, 137],
        with_bitmasks,
        out,
    );

    print_section("Both reads are unmapped:", &[77, 141], with_bitmasks, out);

    print_section(
        "Mapped within the insert size and in correct orientation:",
        &[99, 147, 83, 163],
        with_bitmasks,
        out,
    );

    print_section(
        "Mapped within the insert size but in wrong orientation:",
        &[67, 131, 115, 179],
        with_bitmasks,
        out,
    );

    print_section(
        "Mapped uniquely, but with wrong insert size:",
        &[81, 161, 97, 145, 65, 129, 113, 177],
        with_bitmasks,
        out,
    );
    let _ = out.flush();
}

/// Simple interactive checklist UI for selecting flags.
///
/// - Arrow Up/Down: move selection
/// - Space: toggle current flag
/// - Enter: accept and return the computed flag value
/// - q: cancel without making changes
fn run_interactive(full: bool, out: &mut StandardStream) -> io::Result<Option<u16>> {
    terminal::enable_raw_mode()?;

    execute!(out, Clear(ClearType::All), cursor::MoveTo(0, 0))?;

    let mut selected: Vec<bool> = vec![false; FLAGS.len()];
    let mut index: usize = 0;

    loop {
        // Redraw screen
        execute!(out, Clear(ClearType::All), cursor::MoveTo(0, 0))?;
        write_bold(
            out,
            "Interactive flag selection (↑/↓ to move, space to toggle, Enter to accept, q to quit)",
        )?;
        writeln!(out)?;
        writeln!(out)?;

        // Each flag rendered on its own fixed row starting at row 2 so that
        // the list stays left-aligned regardless of wrapping.
        for (i, flag) in FLAGS.iter().enumerate() {
            let row = 2 + i as u16;
            execute!(out, cursor::MoveTo(0, row), Clear(ClearType::CurrentLine))?;

            let marker = if selected[i] { "[x]" } else { "[ ]" };
            let cursor_char = if i == index { ">" } else { " " };
            let line = format!(
                "{cursor_char} {} {:>3}  {}",
                marker, flag.bitmask, flag.name
            );

            if i == index {
                execute!(
                    out,
                    SetAttribute(Attribute::Reverse),
                    SetForegroundColor(Color::Cyan),
                    Print(&line),
                    SetAttribute(Attribute::Reset),
                    SetForegroundColor(Color::Reset)
                )?;
            } else {
                write!(out, "{line}")?;
            }
        }

        out.flush()?;

        // Handle input
        if let Event::Key(key) = event::read()? {
            match key.code {
                KeyCode::Char('q') | KeyCode::Esc => {
                    terminal::disable_raw_mode()?;
                    // User cancelled
                    return Ok(None);
                }
                KeyCode::Up => {
                    if index == 0 {
                        index = FLAGS.len() - 1;
                    } else {
                        index -= 1;
                    }
                }
                KeyCode::Down => {
                    index = (index + 1) % FLAGS.len();
                }
                KeyCode::Char(' ') => {
                    selected[index] = !selected[index];
                }
                KeyCode::Enter => {
                    let value = compute_flag_value(&selected);
                    // Clear the interactive UI, then leave raw mode and print on a clean screen.
                    execute!(out, Clear(ClearType::All), cursor::MoveTo(0, 0))?;
                    terminal::disable_raw_mode()?;
                    print_explanation(value, full, out);
                    return Ok(Some(value));
                }
                _ => {}
            }
        }
    }
}

fn main() {
    let cli = Cli::parse();
    let choice = color_choice(cli.color);
    let mut stdout = StandardStream::stdout(choice);
    let mut stderr = StandardStream::stderr(choice);

    // These modes are mutually exclusive; mixing them would be confusing.
    if cli.bit && cli.int_mode {
        let _ = write_error_message(
            &mut stderr,
            "Options -b/--bit and -i/--int cannot be used together.",
        );
        let _ = writeln!(stderr);
        let _ = stderr.flush();
        std::process::exit(1);
    }
    // It also doesn't make sense to request bit/int conversion together with
    // `--full` explanations, since -b/-i only transform numeric values.
    if (cli.bit || cli.int_mode) && cli.full {
        let _ = write_error_message(
            &mut stderr,
            "Options -b/--bit or -i/--int cannot be combined with -f/--full.",
        );
        let _ = writeln!(stderr);
        let _ = stderr.flush();
        std::process::exit(1);
    }

    match (cli.flag, cli.command) {
        // Default behavior: one or more flag values ⇒ explain each (or -b/-i on all).
        (Some(flags), None) => {
            // -b/--bit: convert each value to bitmask (hex), one per line.
            if cli.bit {
                for raw in &flags {
                    let looks_like_bitmask = input_looks_like_bitmask(raw);
                    match parse_flag_value(raw) {
                        Ok(value) => {
                            writeln!(stdout, "0x{:03x}", value).unwrap();
                            if looks_like_bitmask {
                                let _ = write_warning_message(
                                    &mut stderr,
                                    "Input already looks like a bitmask; -b/--bit is intended for \
                                     integer input. Did you mean -i/--int?",
                                );
                                let _ = writeln!(stderr);
                                let _ = stderr.flush();
                            }
                        }
                        Err(err) => {
                            let _ = write_error_message(&mut stderr, &err);
                            let _ = writeln!(stderr);
                            let _ = stderr.flush();
                            std::process::exit(1);
                        }
                    }
                }
                return;
            }

            // -i/--int: convert each value to integer (decimal), one per line.
            if cli.int_mode {
                for raw in &flags {
                    let looks_like_bitmask = input_looks_like_bitmask(raw);
                    match parse_flag_value(raw) {
                        Ok(value) => {
                            writeln!(stdout, "{value}").unwrap();
                            if !looks_like_bitmask {
                                let _ = write_warning_message(
                                    &mut stderr,
                                    "Input already looks like an integer; -i/--int is intended for \
                                     bitmask input. Did you mean -b/--bit?",
                                );
                                let _ = writeln!(stderr);
                                let _ = stderr.flush();
                            }
                        }
                        Err(err) => {
                            let _ = write_error_message(&mut stderr, &err);
                            let _ = writeln!(stderr);
                            let _ = stderr.flush();
                            std::process::exit(1);
                        }
                    }
                }
                return;
            }

            // Default: explain each flag value (same as evaluate).
            for (i, raw) in flags.iter().enumerate() {
                if i > 0 {
                    writeln!(stdout).unwrap();
                }
                match parse_flag_value(raw) {
                    Ok(value) => print_explanation(value, cli.full, &mut stdout),
                    Err(err) => {
                        let _ = write_error_message(&mut stderr, &err);
                        let _ = writeln!(stderr);
                        let _ = stderr.flush();
                        std::process::exit(1);
                    }
                }
            }
        }

        // Explicit `explain` subcommand
        (None, Some(Command::Explain { flags })) => {
            for (i, raw) in flags.iter().enumerate() {
                if i > 0 {
                    writeln!(stdout).unwrap();
                }
                match parse_flag_value(raw) {
                    Ok(value) => print_explanation(value, cli.full, &mut stdout),
                    Err(err) => {
                        let _ = write_error_message(&mut stderr, &err);
                        let _ = writeln!(stderr);
                        let _ = stderr.flush();
                        std::process::exit(1);
                    }
                }
            }
        }

        // Other subcommands
        (None, Some(Command::Compute(selection))) => {
            let checked = selection.to_vec();
            let value = compute_flag_value(&checked);
            print_explanation(value, cli.full, &mut stdout);
        }
        (None, Some(Command::Switch { flag })) => match parse_flag_value(&flag) {
            Ok(flag_value) => {
                let checked = value_to_checked_flags(flag_value);
                let (_swapped, value, _summary, _bad_flags) = switch_mate_flags(checked);
                let _ = write_blue_bold(&mut stdout, "After switching mate-related flags:");
                let _ = writeln!(stdout);
                print_single_read_explanation(value, &mut stdout);
            }
            Err(err) => {
                let _ = write_error_message(&mut stderr, &err);
                let _ = writeln!(stderr);
                let _ = stderr.flush();
                std::process::exit(1);
            }
        },
        (None, Some(Command::Select { full: select_full })) => {
            // Allow `flagsamurai --full select`, `flagsamurai select --full`,
            // or even both; in all cases, treat `--full` as enabled if it
            // appears at least once.
            let effective_full = cli.full || select_full;
            if let Err(err) = run_interactive(effective_full, &mut stdout) {
                let _ =
                    write_error_message(&mut stderr, &format!("Interactive mode failed: {err}"));
                let _ = writeln!(stderr);
                let _ = stderr.flush();
                std::process::exit(1);
            }
        }
        (None, Some(Command::Common { full: common_full })) => {
            // Honor both top-level and subcommand-level `--full` for this
            // listing: when set in either place, also show the bitmask
            // alongside the decimal value.
            let effective_full = cli.full || common_full;
            print_common_flags(effective_full, &mut stdout);
        }
        (None, Some(Command::Evaluate { flags })) => {
            for (i, raw) in flags.iter().enumerate() {
                if i > 0 {
                    writeln!(stdout).unwrap();
                }
                match parse_flag_value(raw) {
                    Ok(value) => print_explanation(value, cli.full, &mut stdout),
                    Err(err) => {
                        let _ = write_error_message(&mut stderr, &err);
                        let _ = writeln!(stderr);
                        let _ = stderr.flush();
                        std::process::exit(1);
                    }
                }
            }
        }
        (None, Some(Command::Diff { flags })) => {
            if flags.len() != 2 {
                let _ = write_error_message(
                    &mut stderr,
                    &format!(
                        "The 'diff' subcommand expects exactly 2 flag values, but got {}.",
                        flags.len()
                    ),
                );
                let _ = writeln!(stderr);
                let _ = write_yellow(&mut stderr, "Usage: flagsamurai diff <FIRST> <SECOND>");
                let _ = writeln!(stderr);
                let _ = stderr.flush();
                std::process::exit(1);
            }

            let first_value = match parse_flag_value(&flags[0]) {
                Ok(v) => v,
                Err(err) => {
                    let _ = write_error_message(&mut stderr, &err);
                    let _ = writeln!(stderr);
                    let _ = stderr.flush();
                    std::process::exit(1);
                }
            };
            let second_value = match parse_flag_value(&flags[1]) {
                Ok(v) => v,
                Err(err) => {
                    let _ = write_error_message(&mut stderr, &err);
                    let _ = writeln!(stderr);
                    let _ = stderr.flush();
                    std::process::exit(1);
                }
            };
            print_diff(first_value, second_value, &mut stdout);
        }

        // No args at all: print a short usage hint.
        (None, None) => {
            let _ = write_yellow(
                &mut stderr,
                "Usage: flagsamurai <FLAG> | flagsamurai <SUBCOMMAND> [FLAGS...]",
            );
            let _ = writeln!(stderr);
            let _ = stderr.flush();
            std::process::exit(1);
        }

        // Both top-level flag and subcommand present: treat as misuse.
        (Some(_), Some(_)) => {
            let _ = write_error_message(
                &mut stderr,
                "Provide either a FLAG value or a subcommand, not both.",
            );
            let _ = writeln!(stderr);
            let _ = stderr.flush();
            std::process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compute_and_explain_round_trip() {
        let selection = FlagSelection {
            read_paired: true,
            read_proper_pair: true,
            read_unmapped: false,
            mate_unmapped: true,
            read_reverse: false,
            mate_reverse: true,
            first_in_pair: true,
            second_in_pair: false,
            not_primary: false,
            fails_qc: false,
            duplicate: false,
            supplementary: false,
        };

        let checked = selection.to_vec();
        let value = compute_flag_value(&checked);
        let (summary, bad_flags) = explain_flags(value);

        // We set 7 paired-related flags, all valid because the read is paired.
        assert_eq!(summary.len(), 6);
        assert!(bad_flags.is_empty());
    }

    #[test]
    fn invalid_flags_for_unpaired_reads_are_detected() {
        // Value contains "mate unmapped" and "second in pair" but is not marked as paired.
        let value: u16 = 0x8 | 0x80;
        let (_summary, bad_flags) = explain_flags(value);
        assert_eq!(bad_flags.len(), 2);
    }

    #[test]
    fn switch_mate_flags_swaps_expected_bits() {
        let mut selection = FlagSelection::default();
        selection.read_unmapped = true;
        selection.read_reverse = true;
        selection.first_in_pair = true;

        let (swapped, value, _summary, _bad_flags) = switch_mate_flags(selection.to_vec());

        // After swapping, the original read flags should move to mate/second.
        assert!(!swapped[2]); // read unmapped
        assert!(swapped[3]); // mate unmapped
        assert!(!swapped[4]); // read reverse
        assert!(swapped[5]); // mate reverse
        assert!(!swapped[6]); // first in pair
        assert!(swapped[7]); // second in pair

        // The resulting value should have the expected bits set.
        assert_eq!(value & 0x8, 0x8);
        assert_eq!(value & 0x20, 0x20);
        assert_eq!(value & 0x80, 0x80);
    }
}
