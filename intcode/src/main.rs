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
                        .help("Trace all instructions during execution")
                        .requires("run"),
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
                ),
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("run") {
        let file = matches.value_of("file").unwrap();
        let trace = matches.is_present("trace");
        intcode_vm::run_file(file, trace);
    } else if let Some(matches) = matches.subcommand_matches("asm") {
        let file = matches.value_of("file").unwrap();
        let run = matches.is_present("run");
        let trace = matches.is_present("trace");

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
            intcode_vm::run(code, trace);
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

        let asm = match intcode_compiler::compile(file) {
            Ok(code) => code,
            Err(error) => {
                eprintln!("{}", error);
                return;
            }
        };

        if print_asm {
            for stmt in &asm {
                println!("{}", stmt);
            }
        }

        let code = intcode_asm::assemble(&asm);

        if run {
            intcode_vm::run(code, trace);
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
