use clap::{Arg, ArgAction, Command};

fn main() {
    let matches = Command::new("vts")
        .about("A framework for generating and synthesizing SPGA hardware")
        .version("0.1.0")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .author("Rikus Honey <22550119 at sun.ac.za>")
        .subcommand(
            Command::new("blif")
                .about("Process BLIF files")
                .long_flag("blif")
                .arg_required_else_help(true)
                .arg(
                    Arg::new("check")
                        .long("check")
                        .help("Check BLIF files for syntax errors")
                        .action(ArgAction::Set)
                        .num_args(1..),
                ),
        )
        .get_matches();

    match matches.subcommand() {
        Some(("blif", blif_matches)) => {
            if blif_matches.contains_id("check") {
                let files_to_check: Vec<_> = blif_matches
                    .get_many::<String>("check")
                    .expect("contains_filenames")
                    .map(|s| s.as_str())
                    .collect();
                let filenames = files_to_check.join(", ");
                eprintln!("Checking {filenames}...");
            }
        }
        _ => unreachable!(),
    }
}
