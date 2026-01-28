use clap::{Args, Parser, Subcommand};
use colored::Colorize;

/// Bit flag with a human-readable name.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct Flag {
    name: &'static str,
    bitmask: u16,
}

/// All flags supported by this tool.
///
/// The order here is significant: some operations (like mate switching)
/// treat this as an indexed list.
const FLAGS: [Flag; 12] = [
    Flag {
        name: "read paired",
        bitmask: 0x1,
    },
    Flag {
        name: "read mapped in proper pair",
        bitmask: 0x2,
    },
    Flag {
        name: "read unmapped",
        bitmask: 0x4,
    },
    Flag {
        name: "mate unmapped",
        bitmask: 0x8,
    },
    Flag {
        name: "read reverse strand",
        bitmask: 0x10,
    },
    Flag {
        name: "mate reverse strand",
        bitmask: 0x20,
    },
    Flag {
        name: "first in pair",
        bitmask: 0x40,
    },
    Flag {
        name: "second in pair",
        bitmask: 0x80,
    },
    Flag {
        name: "not primary alignment",
        bitmask: 0x100,
    },
    Flag {
        name: "fails quality checks",
        bitmask: 0x200,
    },
    Flag {
        name: "PCR/optical duplicate",
        bitmask: 0x400,
    },
    Flag {
        name: "supplementary alignment",
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
    /// Show information for both reads (original and mate-swapped).
    #[arg(short, long)]
    full: bool,

    /// Treat the input as an integer and print the corresponding bitmask.
    ///
    /// If the input already looks like a bitmask (e.g. `0x63`), the same
    /// bitmask is printed but a warning is emitted on stderr suggesting
    /// `-i/--int` instead.
    #[arg(short = 'b', long = "bit")]
    bit: bool,

    /// Treat the input as a bitmask and print the corresponding integer.
    ///
    /// If the input already looks like an integer (e.g. `99`), the same
    /// integer is printed but a warning is emitted on stderr suggesting
    /// `-b/--bit` instead.
    #[arg(short = 'i', long = "int")]
    int_mode: bool,

    /// Numeric flag value to explain (decimal or hex like 0x93).
    ///
    /// If provided without a subcommand, this is handled like the `explain` command.
    flag: Option<String>,

    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Explain the meaning of a numeric flag value.
    Explain {
        /// Numeric flag value (decimal, hex like 0x93 is also accepted).
        flag: String,
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
    if let Some(stripped) = trimmed
        .strip_prefix("0x")
        .or_else(|| trimmed.strip_prefix("0X"))
    {
        u16::from_str_radix(stripped, 16)
            .map_err(|e| format!("invalid hex flag value '{input}': {e}"))
    } else {
        trimmed
            .parse::<u16>()
            .map_err(|e| format!("invalid decimal flag value '{input}': {e}"))
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
fn print_single_read_explanation(flag_value: u16) {
    let (summary, bad_flags) = explain_flags(flag_value);

    println!(
        "{} {} {}",
        "Flag value:".bold(),
        flag_value.to_string().bright_cyan(),
        format!("(0x{:03x})", flag_value).bright_black()
    );
    if summary.is_empty() {
        println!("{}", "No flags are set.".dimmed());
    } else {
        println!("{}", "Set flags:".bold());
        for f in &summary {
            println!(
                "  - {} {}: {}",
                format!("{:>3}", f.bitmask).bright_cyan(),
                format!("(0x{:03x})", f.bitmask).bright_black(),
                f.name
            );
        }
    }

    if !bad_flags.is_empty() {
        println!();
        println!(
            "{}",
            "Warning: the following flags are invalid when the read is not paired:"
                .bright_yellow()
                .bold()
        );
        for f in &bad_flags {
            println!(
                "  - {} {}: {}",
                format!("{:>3}", f.bitmask).bright_red(),
                format!("(0x{:03x})", f.bitmask).bright_black(),
                f.name.bright_red()
            );
        }
    }
}

/// Print explanation, optionally for both reads when `full` is true.
fn print_explanation(flag_value: u16, full: bool) {
    if !full {
        print_single_read_explanation(flag_value);
        return;
    }

    println!(
        "{} {} {}",
        "Description of".bold().bright_blue(),
        "first".bold().bright_green(),
        "read".bold().bright_blue()
    );
    print_single_read_explanation(flag_value);
    println!();

    // Second read: flip read/mate-related bits and recompute.
    let checked = value_to_checked_flags(flag_value);
    let (_swapped, mate_value, _summary, _bad_flags) = switch_mate_flags(checked);

    println!(
        "{} {} {}",
        "Description of".bold().bright_blue(),
        "second".bold().bright_green(),
        "read".bold().bright_blue()
    );
    print_single_read_explanation(mate_value);
}

fn main() {
    let cli = Cli::parse();

    // These modes are mutually exclusive; mixing them would be confusing.
    if cli.bit && cli.int_mode {
        eprintln!(
            "{}",
            "Options -b/--bit and -i/--int cannot be used together.".bright_red()
        );
        std::process::exit(1);
    }

    match (cli.flag, cli.command) {
        // Default behavior: just a number ⇒ explain
        (Some(flag), None) => {
            // -b/--bit: convert integer-style input to bitmask (hex).
            if cli.bit {
                let looks_like_bitmask = input_looks_like_bitmask(&flag);
                match parse_flag_value(&flag) {
                    Ok(value) => {
                        println!("{}", format!("0x{:03x}", value));
                        if looks_like_bitmask {
                            eprintln!(
                                "{}",
                                "Input already looks like a bitmask; -b/--bit is intended for \
                                 integer input. Did you mean -i/--int?"
                                    .bright_yellow()
                            );
                        }
                    }
                    Err(err) => {
                        eprintln!("{}", err.bright_red());
                        std::process::exit(1);
                    }
                }
                return;
            }

            // -i/--int: convert bitmask-style input to integer (decimal).
            if cli.int_mode {
                let looks_like_bitmask = input_looks_like_bitmask(&flag);
                match parse_flag_value(&flag) {
                    Ok(value) => {
                        println!("{value}");
                        if !looks_like_bitmask {
                            eprintln!(
                                "{}",
                                "Input already looks like an integer; -i/--int is intended for \
                                 bitmask input. Did you mean -b/--bit?"
                                    .bright_yellow()
                            );
                        }
                    }
                    Err(err) => {
                        eprintln!("{}", err.bright_red());
                        std::process::exit(1);
                    }
                }
                return;
            }

            // Default: explain flags.
            match parse_flag_value(&flag) {
                Ok(value) => print_explanation(value, cli.full),
                Err(err) => {
                    eprintln!("{}", err.bright_red());
                    std::process::exit(1);
                }
            }
        }

        // Explicit `explain` subcommand
        (None, Some(Command::Explain { flag })) => match parse_flag_value(&flag) {
            Ok(value) => print_explanation(value, cli.full),
            Err(err) => {
                eprintln!("{}", err.bright_red());
                std::process::exit(1);
            }
        },

        // Other subcommands
        (None, Some(Command::Compute(selection))) => {
            let checked = selection.to_vec();
            let value = compute_flag_value(&checked);
            print_explanation(value, cli.full);
        }
        (None, Some(Command::Switch { flag })) => match parse_flag_value(&flag) {
            Ok(flag_value) => {
                let checked = value_to_checked_flags(flag_value);
                let (_swapped, value, summary, bad_flags) = switch_mate_flags(checked);

                println!(
                    "{}",
                    "After switching mate-related flags:".bold().bright_blue()
                );
                println!(
                    "{} {} {}",
                    "Flag value:".bold(),
                    value.to_string().bright_cyan(),
                    format!("(0x{:03x})", value).bright_black()
                );

                if summary.is_empty() {
                    println!("{}", "No flags are set.".dimmed());
                } else {
                    println!("{}", "Set flags:".bold());
                    for f in &summary {
                        println!(
                            "  - {} {}: {}",
                            format!("{:>3}", f.bitmask).bright_cyan(),
                            format!("(0x{:03x})", f.bitmask).bright_black(),
                            f.name
                        );
                    }
                }

                if !bad_flags.is_empty() {
                    println!();
                    println!(
                        "{}",
                        "Warning: the following flags are invalid when the read is not paired:"
                            .bright_yellow()
                            .bold()
                    );
                    for f in &bad_flags {
                        println!(
                            "  - {} {}: {}",
                            format!("{:>3}", f.bitmask).bright_red(),
                            format!("(0x{:03x})", f.bitmask).bright_black(),
                            f.name.bright_red()
                        );
                    }
                }
            }
            Err(err) => {
                eprintln!("{}", err.bright_red());
                std::process::exit(1);
            }
        },

        // No args at all: print a short usage hint.
        (None, None) => {
            eprintln!(
                "{}",
                "Usage: flagsamurai <FLAG> | flagsamurai <SUBCOMMAND> [FLAGS...]".bright_yellow()
            );
            std::process::exit(1);
        }

        // Both top-level flag and subcommand present: treat as misuse.
        (Some(_), Some(_)) => {
            eprintln!(
                "{}",
                "Provide either a FLAG value or a subcommand, not both.".bright_red()
            );
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
