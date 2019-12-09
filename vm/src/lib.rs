use std::convert::{TryFrom, TryInto};
use std::io::{self, BufRead, Write};

type Error = String;
type Result<T = ()> = std::result::Result<T, Error>;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum ParamMode {
    Positional,
    Immediate,
    Relative,
}

impl TryFrom<i64> for ParamMode {
    type Error = Error;

    fn try_from(val: i64) -> Result<Self> {
        match val {
            0 => Ok(Self::Positional),
            1 => Ok(Self::Immediate),
            2 => Ok(Self::Relative),
            _ => Err(format!("Invalid param mode: {}", val)),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct Param {
    val: i64,
    mode: ParamMode,
}

impl std::fmt::Display for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.mode {
            ParamMode::Positional => write!(f, "[{}]", self.val),
            ParamMode::Immediate => write!(f, "{}", self.val),
            ParamMode::Relative => write!(f, "%{}", self.val),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Instr {
    Add(Param, Param, Param),
    Mul(Param, Param, Param),
    In(Param),
    Out(Param),
    JumpTrue(Param, Param),
    JumpFalse(Param, Param),
    LessThan(Param, Param, Param),
    Equal(Param, Param, Param),
    AddRelBase(Param),
    Halt,
}

impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Instr::*;
        match self {
            Add(a, b, c) => write!(f, "add {} {} {}", a, b, c),
            Mul(a, b, c) => write!(f, "mul {} {} {}", a, b, c),
            In(a) => write!(f, "in {}", a),
            Out(a) => write!(f, "out {}", a),
            JumpTrue(a, b) => write!(f, "jmp_true {} {}", a, b),
            JumpFalse(a, b) => write!(f, "jmp_false {} {}", a, b),
            LessThan(a, b, c) => write!(f, "lt {} {} {}", a, b, c),
            Equal(a, b, c) => write!(f, "eq {} {} {}", a, b, c),
            AddRelBase(a) => write!(f, "add_rel_base {}", a),
            Halt => write!(f, "hlt"),
        }
    }
}

struct VM {
    memory: Vec<i64>,
    ip: usize,
    rel_base: i64,
    param_modes: i64,
    trace: bool,
    trace_load_store: bool,
    prompt_input: bool,
}

impl VM {
    fn new(memory: Vec<i64>, trace: bool, trace_load_store: bool, prompt_input: bool) -> Self {
        Self {
            memory,
            ip: 0,
            param_modes: 0,
            rel_base: 0,
            trace,
            trace_load_store,
            prompt_input,
        }
    }

    fn make_instr1(&mut self, cons: fn(Param) -> Instr) -> Result<Instr> {
        Ok(cons(self.fetch_param()?))
    }

    fn make_instr2(&mut self, cons: fn(Param, Param) -> Instr) -> Result<Instr> {
        Ok(cons(self.fetch_param()?, self.fetch_param()?))
    }

    fn make_instr3(&mut self, cons: fn(Param, Param, Param) -> Instr) -> Result<Instr> {
        Ok(cons(
            self.fetch_param()?,
            self.fetch_param()?,
            self.fetch_param()?,
        ))
    }

    fn get_memory(&self, index: usize) -> i64 {
        self.memory.get(index).copied().unwrap_or_default()
    }

    fn fetch(&mut self) -> i64 {
        let val = self.get_memory(self.ip);
        self.ip += 1;
        val
    }

    fn fetch_param(&mut self) -> Result<Param> {
        let val = self.fetch();
        let mode = self.param_modes % 10;
        self.param_modes /= 10;
        Ok(Param {
            mode: mode.try_into()?,
            val,
        })
    }

    fn decode(&mut self) -> Result<Instr> {
        let instr = self.fetch();
        let opcode = instr % 100;
        self.param_modes = instr / 100;

        match opcode {
            1 => self.make_instr3(Instr::Add),
            2 => self.make_instr3(Instr::Mul),
            3 => self.make_instr1(Instr::In),
            4 => self.make_instr1(Instr::Out),
            5 => self.make_instr2(Instr::JumpTrue),
            6 => self.make_instr2(Instr::JumpFalse),
            7 => self.make_instr3(Instr::LessThan),
            8 => self.make_instr3(Instr::Equal),
            9 => self.make_instr1(Instr::AddRelBase),
            99 => Ok(Instr::Halt),
            _ => Err(format!(
                "Invalid opcode: {} at position {}",
                opcode,
                self.ip - 1
            )),
        }
    }

    fn load(&self, param: Param) -> Result<i64> {
        let index = match param.mode {
            ParamMode::Positional => {
                if param.val < 0 {
                    return Err(format!(
                        "Invalid access to negative memory index: {}",
                        param.val
                    ));
                } else {
                    param.val
                }
            }
            ParamMode::Immediate => return Ok(param.val),
            ParamMode::Relative => {
                let index = param.val + self.rel_base;
                if index < 0 {
                    return Err(format!(
                        "Invalid access to negative memory index: {}",
                        index
                    ));
                } else {
                    index
                }
            }
        };
        let val = self.get_memory(index as usize);
        if self.trace_load_store {
            println!("  Load {} = {} => {}", param, index, val);
        }
        Ok(val)
    }

    fn store(&mut self, param: Param, val: i64) -> Result {
        let index = match param.mode {
            ParamMode::Positional => param.val as usize,
            ParamMode::Relative => (param.val + self.rel_base) as usize,
            ParamMode::Immediate => return Err("Immediate param as target of write".to_string()),
        };
        if index >= self.memory.len() {
            self.memory.resize(index + 1, 0);
        }
        if self.trace_load_store {
            println!("  Store {} to {} = {}", val, param, index);
        }
        self.memory[index] = val;
        Ok(())
    }

    pub fn run(&mut self) -> Result {
        loop {
            use Instr::*;

            let ip = self.ip;
            let instr = self.decode()?;

            if self.trace {
                println!("{}: {}", ip, instr);
            }

            match instr {
                Add(a, b, c) => self.store(c, self.load(a)? + self.load(b)?)?,
                Mul(a, b, c) => self.store(c, self.load(a)? * self.load(b)?)?,
                In(a) => loop {
                    if self.prompt_input {
                        print!("Input: ");
                    }
                    io::stdout().flush().unwrap();
                    let stdin = io::stdin();
                    let line = stdin.lock().lines().next();
                    if let Some(line) = line {
                        let input = line.expect("error during stdin read").parse();
                        match input {
                            Ok(input) => {
                                self.store(a, input)?;
                                break;
                            }
                            Err(_) => println!("Not a number. Try again."),
                        }
                    } else {
                        return Err("Unexpected EOF".to_string());
                    }
                },
                Out(a) => println!("Output: {}", self.load(a)?),
                JumpTrue(cond, target) => {
                    if self.load(cond)? != 0 {
                        self.ip = self.load(target)? as usize;
                    }
                }
                JumpFalse(cond, target) => {
                    if self.load(cond)? == 0 {
                        self.ip = self.load(target)? as usize;
                    }
                }
                LessThan(a, b, c) => {
                    self.store(c, if self.load(a)? < self.load(b)? { 1 } else { 0 })?
                }
                Equal(a, b, c) => {
                    self.store(c, if self.load(a)? == self.load(b)? { 1 } else { 0 })?
                }
                AddRelBase(a) => self.rel_base += self.load(a)?,
                Halt => break,
            }
        }
        Ok(())
    }
}

pub fn run(code: Vec<i64>, trace: bool, trace_load_store: bool, prompt_input: bool) {
    if let Err(error) = VM::new(code, trace, trace_load_store, prompt_input).run() {
        eprintln!("Error during execution:");
        eprintln!("{}", error);
    }
}

pub fn run_file(file: &str, trace: bool, trace_load_store: bool, prompt_input: bool) {
    let reader = std::io::BufReader::new(std::fs::File::open(file).unwrap());
    let line = reader.lines().next().unwrap().unwrap();
    let code = line.split(',').map(|c| c.parse().unwrap()).collect();

    run(code, trace, trace_load_store, prompt_input);
}
