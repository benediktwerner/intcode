#[macro_use]
extern crate pest_derive;

mod codegen;
mod parser;

pub use codegen::*;
pub use parser::*;

pub fn print_program(stmts: &[Stmt]) {
    let mut pos = 0;
    for stmt in stmts {
        if let Stmt::Label(_) = stmt {
            println!("{}", stmt);
        }
        else {
            println!("  {:3}: {}", pos, stmt);
            pos += stmt.length();
        }
    }
}
