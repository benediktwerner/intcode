use crate::ast::{BinOp, Expr, IdentEnv, Program, UnOp};

lalrpop_mod!(pub grammar);

pub enum Line {
    Func(crate::ast::Function),
    Stmt(crate::ast::Stmt),
}

pub struct SourcePos(usize, usize);

impl std::fmt::Display for SourcePos {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

fn unop(op: UnOp, a: Expr) -> Expr {
    Expr::UnOp(op, Box::new(a))
}

fn binop(a: Expr, op: BinOp, b: Expr) -> Expr {
    Expr::BinOp(Box::new(a), op, Box::new(b))
}

fn map_location(input: &str, index: usize) -> SourcePos {
    let mut line = 1;
    let mut column = 1;

    for (i, c) in input.char_indices() {
        if i == index {
            break;
        }
        if c == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }

    SourcePos(line, column)
}

pub fn parse<'a>(input: &'a str) -> Result<(Program, IdentEnv), impl std::fmt::Display + 'a> {
    let mut env = IdentEnv::new();
    match grammar::ProgramParser::new().parse(&mut env, input) {
        Ok(program) => Ok((program, env)),
        Err(error) => Err(error.map_location(|p| map_location(input, p))),
    }
}

pub fn strip_comments(s: String) -> String {
    let mut result = Vec::new();
    let regex = regex::Regex::new(r"\s*//.*").unwrap();
    for line in s.lines() {
        result.push(regex.replace(line, ""));
    }
    result.join("\n")
}
