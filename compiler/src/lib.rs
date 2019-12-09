#[macro_use]
extern crate lalrpop_util;

mod ast;
mod codegen;
mod parser;

use codegen::CompilerError;

pub fn compile(file: &str) -> Result<Vec<intcode_asm::Stmt>, String> {
    let content = std::fs::read_to_string(file).expect("cannot read file");
    let content = parser::strip_comments(content);

    let (program, env) = match parser::parse(&content) {
        Ok(result) => result,
        Err(error) => return Err(format!("Error during file parsing:\n{}", error)),
    };

    codegen::gen(program, &env).map_err(|error| {
        format!(
            "Error during compilation:\n{}",
            match error {
                CompilerError::DuplicateDeclaration(ident) => {
                    format!("Duplicate declaration of variable: {}", env.to_str(ident))
                }
                CompilerError::UndefinedVar(ident) => {
                    format!("Undefined variable: {}", env.to_str(ident))
                }
            }
        )
    })
}
