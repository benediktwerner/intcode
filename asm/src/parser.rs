use std::{
    collections::hash_map::{Entry, HashMap},
    fmt::Display,
};

use pest::{iterators::Pair, Parser};

use crate::codegen::{Ident, Param, ParamMode, Stmt};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct AsmParser;

#[derive(Default)]
pub struct IdentEnv<'a> {
    map: HashMap<&'a str, u32>,
    next: u32,
}

impl<'a> IdentEnv<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_str(&mut self, s: &'a str) -> u32 {
        match self.map.entry(s) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                entry.insert(self.next);
                self.next += 1;
                self.next
            }
        }
    }
}

fn parse_radix(pair: Pair<Rule>, radix: u32) -> i64 {
    let mut pairs = pair.into_inner();
    if pairs.next().unwrap().as_str() == "-" {
        pairs.next().unwrap();
        -i64::from_str_radix(pairs.next().unwrap().as_str(), radix).unwrap()
    } else {
        i64::from_str_radix(pairs.next().unwrap().as_str(), radix).unwrap()
    }
}

fn parse_num(pair: Pair<Rule>) -> i64 {
    match pair.as_rule() {
        Rule::num => parse_num(pair.into_inner().next().unwrap()),
        Rule::decimal => pair.as_str().parse().unwrap(),
        Rule::hex => parse_radix(pair, 16),
        Rule::oct => parse_radix(pair, 16),
        Rule::bin => parse_radix(pair, 16),
        _ => unreachable!(),
    }
}

fn create_ident<'a>(name: &'a str, env: &mut IdentEnv<'a>) -> Ident {
    if name == "__end" {
        Ident("__end", 0)
    } else {
        Ident("__user_label", env.from_str(name))
    }
}

fn parse_param<'a>(pair: Pair<'a, Rule>, env: &mut IdentEnv<'a>) -> Param {
    match pair.as_rule() {
        Rule::param => parse_param(pair.into_inner().next().unwrap(), env),
        Rule::ident => Param::Ident(ParamMode::Positional, create_ident(pair.as_str(), env)),
        Rule::ident_immediate => {
            let s = pair.into_inner().next().unwrap().as_str();
            Param::Ident(ParamMode::Immediate, create_ident(s, env))
        }
        Rule::ident_relative => {
            let s = pair.into_inner().next().unwrap().as_str();
            Param::Ident(ParamMode::Relative, create_ident(s, env))
        }
        Rule::num => Param::Value(ParamMode::Immediate, parse_num(pair)),
        Rule::num_positional => Param::Value(
            ParamMode::Positional,
            parse_num(pair.into_inner().next().unwrap()),
        ),
        Rule::num_relative => Param::Value(
            ParamMode::Relative,
            parse_num(pair.into_inner().next().unwrap()),
        ),
        _ => unreachable!("unknown param rule: {}", pair.as_str()),
    }
}

fn parse_stmt<'a>(pair: Pair<'a, Rule>, env: &mut IdentEnv<'a>) -> Stmt {
    match pair.as_rule() {
        Rule::stmt => parse_stmt(pair.into_inner().next().unwrap(), env),
        Rule::halt => Stmt::Halt,
        Rule::ret => Stmt::Ret,
        Rule::unary_stmt => {
            let mut pairs = pair.into_inner();
            let cons = match pairs.next().unwrap().as_str() {
                "in" => Stmt::In,
                "out" => Stmt::Out,
                "jmp" => Stmt::Jmp,
                "push" => Stmt::Push,
                "pop" => Stmt::Pop,
                "call" => Stmt::Call,
                "add_rel_base" => Stmt::AddRelBase,
                other => unreachable!("unknown unary instruction: {}", other),
            };
            let a = parse_param(pairs.next().unwrap(), env);
            cons(a)
        }
        Rule::binary_stmt => {
            let mut pairs = pair.into_inner();
            let cons = match pairs.next().unwrap().as_str() {
                "jnz" | "jmp_true" => Stmt::JumpTrue,
                "jz" | "jmp_false" => Stmt::JumpFalse,
                "not" => Stmt::Not,
                "mov" => Stmt::Mov,
                "load" => Stmt::Load,
                "store" => Stmt::Store,
                other => unreachable!("unknown binary instruction: {}", other),
            };
            let a = parse_param(pairs.next().unwrap(), env);
            let b = parse_param(pairs.next().unwrap(), env);
            cons(a, b)
        }
        Rule::ternary_stmt => {
            let mut pairs = pair.into_inner();
            let cons = match pairs.next().unwrap().as_str() {
                "add" => Stmt::Add,
                "mul" => Stmt::Mul,
                "sub" => Stmt::Sub,
                "div" => Stmt::Div,
                "mod" => Stmt::Mod,
                "eq" => Stmt::Equal,
                "lt" => Stmt::LessThan,
                "leq" => Stmt::LessEqual,
                "gt" => Stmt::GreaterThan,
                "geq" => Stmt::GreaterEqual,
                "and" => Stmt::And,
                "or" => Stmt::Or,
                other => unreachable!("unknown ternary instruction: {}", other),
            };
            let a = parse_param(pairs.next().unwrap(), env);
            let b = parse_param(pairs.next().unwrap(), env);
            let c = parse_param(pairs.next().unwrap(), env);
            cons(a, b, c)
        }
        Rule::quaternary_stmt => {
            let mut pairs = pair.into_inner();
            let cons = match pairs.next().unwrap().as_str() {
                "divmod" => Stmt::DivMod,
                other => unreachable!("unknown quaternary instruction: {}", other),
            };
            let a = parse_param(pairs.next().unwrap(), env);
            let b = parse_param(pairs.next().unwrap(), env);
            let c = parse_param(pairs.next().unwrap(), env);
            let d = parse_param(pairs.next().unwrap(), env);
            cons(a, b, c, d)
        }
        Rule::data => Stmt::Data(pair.into_inner().map(|d| parse_param(d, env)).collect()),
        Rule::data_array => {
            let mut pairs = pair.into_inner();
            let val = parse_num(pairs.next().unwrap());
            let len = parse_num(pairs.next().unwrap());
            Stmt::DataArray(val, len as usize)
        }
        _ => unreachable!(),
    }
}

pub fn parse_file(file: &str) -> Result<Vec<Stmt>, impl Display> {
    let content = std::fs::read_to_string(file).expect("error when reading file");
    let pairs = match AsmParser::parse(Rule::program, &content) {
        Ok(pairs) => pairs,
        Err(error) => return Err(error.with_path(file)),
    };

    let mut stmts = Vec::new();
    let mut env = IdentEnv::new();

    for pair in pairs {
        match pair.as_rule() {
            Rule::stmt => stmts.push(parse_stmt(pair, &mut env)),
            Rule::label => stmts.push({
                let ident = pair.into_inner().next().unwrap().as_str();
                Stmt::Label(Ident("__user_label", env.from_str(ident)))
            }),
            _ => (),
        }
    }

    Ok(stmts)
}
