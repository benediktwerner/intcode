fn main() {
    let matches = clap::App::new("intcode")
        .version(clap::crate_version!())
        .author(clap::crate_authors!())
        .about("Compiler, assembler and VM for intcode from Advent of Code 2019")
        .setting(clap::AppSettings::ArgRequiredElseHelp)
        .subcommand(
            clap::SubCommand::with_name("run")
                .about("Run an intcode program")
                .arg(
                    clap::Arg::with_name("file")
                        .help("The file to run")
                        .required(true),
                )
                .arg(
                    clap::Arg::with_name("trace")
                        .long("trace")
                        .help("Trace all instructions during execution"),
                )
                .arg(
                    clap::Arg::with_name("trace-load-store")
                        .long("trace-load-store")
                        .help("Trace all memory loads and stores during execution")
                        .requires("run"),
                )
                .arg(
                    clap::Arg::with_name("no-prompt")
                        .long("no-prompt")
                        .help("Don't prompt for  instructions during execution"),
                ),
        )
        .subcommand(
            clap::SubCommand::with_name("asm")
                .about("Assemble an intcode program")
                .arg(
                    clap::Arg::with_name("file")
                        .help("The file to assemble")
                        .required(true),
                )
                .arg(
                    clap::Arg::with_name("run")
                        .long("run")
                        .help("Run the program after assembling"),
                )
                .arg(
                    clap::Arg::with_name("trace")
                        .long("trace")
                        .help("Trace all instructions during execution")
                        .requires("run"),
                )
                .arg(
                    clap::Arg::with_name("trace-load-store")
                        .long("trace-load-store")
                        .help("Trace all memory loads and stores during execution")
                        .requires("run"),
                )
                .arg(
                    clap::Arg::with_name("no-prompt")
                        .long("no-prompt")
                        .help("Don't prompt for  instructions during execution")
                        .requires("run"),
                ),
        )
        .subcommand(
            clap::SubCommand::with_name("compile")
                .about("Compile an intcode program")
                .arg(
                    clap::Arg::with_name("file")
                        .help("The file to compile")
                        .required(true),
                )
                .arg(
                    clap::Arg::with_name("asm")
                        .long("asm")
                        .help("Print assembly after after compiling"),
                )
                .arg(
                    clap::Arg::with_name("run")
                        .long("run")
                        .help("Run the program after compiling"),
                )
                .arg(
                    clap::Arg::with_name("trace")
                        .long("trace")
                        .help("Trace all instructions during execution")
                        .requires("run"),
                )
                .arg(
                    clap::Arg::with_name("trace-load-store")
                        .long("trace-load-store")
                        .help("Trace all memory loads and stores during execution")
                        .requires("run"),
                )
                .arg(
                    clap::Arg::with_name("no-prompt")
                        .long("no-prompt")
                        .help("Don't prompt for  instructions during execution")
                        .requires("run"),
                ),
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("run") {
        let file = matches.value_of("file").unwrap();
        let trace = matches.is_present("trace");
        let trace_load_store = matches.is_present("trace-load-store");
        let prompt = !matches.is_present("no-prompt");
        intcode_vm::run_file(file, trace, trace_load_store, prompt);
    } else if let Some(matches) = matches.subcommand_matches("asm") {
        let file = matches.value_of("file").unwrap();
        let run = matches.is_present("run");
        let trace = matches.is_present("trace");
        let trace_load_store = matches.is_present("trace-load-store");
        let prompt = !matches.is_present("no-prompt");

        let stmts = match intcode_asm::parse_file(file) {
            Ok(code) => code,
            Err(error) => {
                eprintln!("Error during parsing:");
                eprintln!("{}", error);
                return;
            }
        };
        let code = intcode_asm::assemble(&stmts);

        if run {
            intcode_vm::run(code, trace, trace_load_store, prompt);
        } else {
            println!(
                "{}",
                code.iter()
                    .map(i64::to_string)
                    .collect::<Vec<_>>()
                    .join(",")
            );
        }
    } else if let Some(matches) = matches.subcommand_matches("compile") {
        let file = matches.value_of("file").unwrap();
        let print_asm = matches.is_present("asm");
        let run = matches.is_present("run");
        let trace = matches.is_present("trace");
        let trace_load_store = matches.is_present("trace-load-store");
        let prompt = !matches.is_present("no-prompt");

        let asm = match intcode_compiler::compile(file) {
            Ok(code) => code,
            Err(error) => {
                eprintln!("{}", error);
                return;
            }
        };

        if print_asm {
            intcode_asm::print_program(&asm);
        }

        let code = intcode_asm::assemble(&asm);

        if run {
            intcode_vm::run(code, trace, trace_load_store, prompt);
        } else if !print_asm {
            println!(
                "{}",
                code.iter()
                    .map(i64::to_string)
                    .collect::<Vec<_>>()
                    .join(",")
            );
        }
    }
}
