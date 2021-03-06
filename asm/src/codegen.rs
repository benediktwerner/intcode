use std::collections::{hash_map::Entry, HashMap};

const ADD: i64 = 1;
const MUL: i64 = 2;
const IN: i64 = 3;
const OUT: i64 = 4;
const JUMP_TRUE: i64 = 5;
const JUMP_FALSE: i64 = 6;
const LESS_THAN: i64 = 7;
const EQUAL: i64 = 8;
const ADD_REL_BASE: i64 = 9;
const HALT: i64 = 99;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Ident(pub &'static str, pub u32);

impl Ident {
    pub fn to_pos(self) -> Param {
        Param::Ident(ParamMode::Positional, self)
    }
    pub fn to_imm(self) -> Param {
        Param::Ident(ParamMode::Immediate, self)
    }
    pub fn to_rel(self) -> Param {
        Param::Ident(ParamMode::Relative, self)
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.1 == 0 {
            write!(f, "{}", self.0)
        } else {
            write!(f, "{}_{}", self.0, self.1)
        }
    }
}

impl From<&'static str> for Ident {
    fn from(s: &'static str) -> Self {
        Ident(s, 0)
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ParamMode {
    Positional,
    Immediate,
    Relative,
}

impl From<ParamMode> for i64 {
    fn from(mode: ParamMode) -> i64 {
        match mode {
            ParamMode::Positional => 0,
            ParamMode::Immediate => 1,
            ParamMode::Relative => 2,
        }
    }
}

impl From<&ParamMode> for i64 {
    fn from(mode: &ParamMode) -> i64 {
        (*mode).into()
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Param {
    Ident(ParamMode, Ident),
    Value(ParamMode, i64),
}

impl Param {
    pub fn mode(&self) -> ParamMode {
        match self {
            Self::Ident(mode, _) => *mode,
            Self::Value(mode, _) => *mode,
        }
    }

    fn macro_ident(name: &'static str, asm: &mut Assembler) -> Self {
        let (mode, name) = match &name[0..1] {
            ":" => (ParamMode::Immediate, &name[1..]),
            "%" => (ParamMode::Relative, &name[1..]),
            _ => (ParamMode::Positional, name),
        };
        Self::Ident(mode, Ident(name, asm.get_macro_id()))
    }
}

impl std::fmt::Display for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Ident(ParamMode::Positional, ident) => write!(f, "{}", ident),
            Self::Ident(ParamMode::Immediate, ident) => write!(f, ":{}", ident),
            Self::Ident(ParamMode::Relative, ident) => write!(f, "%{}", ident),
            Self::Value(ParamMode::Positional, val) => write!(f, "[{}]", val),
            Self::Value(ParamMode::Immediate, val) => write!(f, "{}", val),
            Self::Value(ParamMode::Relative, val) => write!(f, "%{}", val),
        }
    }
}

impl From<&'static str> for Param {
    fn from(name: &'static str) -> Self {
        let (mode, name) = match &name[0..1] {
            ":" => (ParamMode::Immediate, &name[1..]),
            "%" => (ParamMode::Relative, &name[1..]),
            _ => (ParamMode::Positional, name),
        };
        Self::Ident(mode, Ident(name, 0))
    }
}

impl From<i64> for Param {
    fn from(x: i64) -> Self {
        Self::Value(ParamMode::Immediate, x)
    }
}

impl From<&Param> for Param {
    fn from(p: &Param) -> Self {
        *p
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Stmt {
    Label(Ident),
    Add(Param, Param, Param),
    Mul(Param, Param, Param),
    In(Param),
    Out(Param),
    JumpTrue(Param, Param),
    JumpFalse(Param, Param),
    LessThan(Param, Param, Param),
    Equal(Param, Param, Param),
    Data(Vec<Param>),
    DataArray(i64, usize),
    Halt,

    Load(Param, Param),
    Store(Param, Param),

    AddRelBase(Param),
    Push(Param),
    Pop(Param),
    Call(Param),
    Ret,

    Mov(Param, Param),
    Sub(Param, Param, Param),
    Div(Param, Param, Param),
    Mod(Param, Param, Param),
    DivMod(Param, Param, Param, Param),
    Jmp(Param),
    And(Param, Param, Param),
    Or(Param, Param, Param),
    Not(Param, Param),
    NotEqual(Param, Param, Param),
    LessEqual(Param, Param, Param),
    GreaterThan(Param, Param, Param),
    GreaterEqual(Param, Param, Param),
}

impl Stmt {
    fn opcode(&self) -> i64 {
        use Stmt::*;
        match self {
            Add(..) => ADD,
            Mul(..) => MUL,
            In(..) => IN,
            Out(..) => OUT,
            JumpTrue(..) => JUMP_TRUE,
            JumpFalse(..) => JUMP_FALSE,
            LessThan(..) => LESS_THAN,
            Equal(..) => EQUAL,
            AddRelBase(..) => ADD_REL_BASE,
            Halt => HALT,
            _ => panic!("Stmt {:?} has no opcode", self),
        }
    }

    pub fn length(&self) -> usize {
        match self {
            Self::Label(..) => 0,
            Self::Add(..) => 4,
            Self::Mul(..) => 4,
            Self::In(..) => 2,
            Self::Out(..) => 2,
            Self::JumpTrue(..) => 3,
            Self::JumpFalse(..) => 3,
            Self::LessThan(..) => 4,
            Self::Equal(..) => 4,
            Self::Data(..) => 1,
            Self::DataArray(_, length) => *length,
            Self::Halt => 1,
            Self::AddRelBase(_) => 2,
            Self::Load(..) => 8,
            Self::Store(..) => 8,
            Self::Push(..) => 6,
            Self::Pop(..) => 6,
            Self::Call(..) => 13,
            Self::Ret => 13,
            Self::Mov(..) => 4,
            Self::Sub(..) => 8,
            Self::Div(..) => 38,
            Self::Mod(..) => 30,
            Self::DivMod(..) => 38,
            Self::Jmp(..) => 7,
            Self::And(..) => 18,
            Self::Or(..) => 18,
            Self::Not(..) => 18,
            Self::NotEqual(..) => 4 + 18,
            Self::LessEqual(..) => 4 + 4 + 18,
            Self::GreaterThan(..) => 4,
            Self::GreaterEqual(..) => 4 + 4 + 18,
        }
    }
}

impl std::fmt::Display for Stmt {
    #[allow(clippy::many_single_char_names)]
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Stmt::*;
        match self {
            Label(ident) => write!(f, "{}:", ident),
            Data(vals) => {
                write!(f, "data")?;
                for val in vals {
                    write!(f, " {}", val)?;
                }
                Ok(())
            }
            DataArray(a, b) => write!(f, "array {} {}", a, b),

            Ret => write!(f, "ret"),
            Halt => write!(f, "hlt"),

            In(a) => write!(f, "in {}", a),
            Out(a) => write!(f, "out {}", a),
            AddRelBase(a) => write!(f, "add_rel_base {}", a),
            Push(a) => write!(f, "push {}", a),
            Pop(a) => write!(f, "pop {}", a),
            Call(a) => write!(f, "call {}", a),
            Jmp(a) => write!(f, "jmp {}", a),

            Not(a, b) => write!(f, "not {} {}", a, b),
            JumpTrue(a, b) => write!(f, "jmp_true {} {}", a, b),
            JumpFalse(a, b) => write!(f, "jmp_false {} {}", a, b),
            Mov(a, b) => write!(f, "mov {} {}", a, b),
            Load(a, b) => write!(f, "load {} {}", a, b),
            Store(a, b) => write!(f, "store {} {}", a, b),

            Add(a, b, c) => write!(f, "add {} {} {}", a, b, c),
            Mul(a, b, c) => write!(f, "mul {} {} {}", a, b, c),
            Sub(a, b, c) => write!(f, "sub {} {} {}", a, b, c),
            Div(a, b, c) => write!(f, "div {} {} {}", a, b, c),
            Mod(a, b, c) => write!(f, "mod {} {} {}", a, b, c),
            Equal(a, b, c) => write!(f, "eq {} {} {}", a, b, c),
            NotEqual(a, b, c) => write!(f, "neq {} {} {}", a, b, c),
            LessThan(a, b, c) => write!(f, "lt {} {} {}", a, b, c),
            LessEqual(a, b, c) => write!(f, "leq {} {} {}", a, b, c),
            GreaterThan(a, b, c) => write!(f, "gt {} {} {}", a, b, c),
            GreaterEqual(a, b, c) => write!(f, "geq {} {} {}", a, b, c),
            And(a, b, c) => write!(f, "and {} {} {}", a, b, c),
            Or(a, b, c) => write!(f, "or {} {} {}", a, b, c),

            DivMod(a, b, c, d) => write!(f, "divmod {} {} {} {}", a, b, c, d),
        }
    }
}

macro_rules! param {
    ([ $e:expr ]) => {
        Param::Value(ParamMode::Positional, $e)
    };
    ((rel $e:expr)) => {
        Param::Value(ParamMode::Relative, $e)
    };
    (($self:ident $l:literal)) => {
        Param::macro_ident($l, $self)
    };
    ($e:expr) => {
        Param::from($e)
    };
}

macro_rules! assemble {
    [$self:ident, $([$($t:tt)*]),* $(,)?] => {
        {
            let stmts = &[$(stmt!($($t)*)),*];
            $self.assemble_stmts(stmts);
        }
    }
}

// macro_rules! stmts {
//     [$([$($t:tt)*]),* $(,)?] => {
//         &[$(stmt!($($t)*)),*]
//     };
// }

macro_rules! stmt {
    (label $n:literal) => {
        Stmt::Label(Ident($n, 0))
    };
    (label $self:ident $n:literal) => {
        Stmt::Label(Ident($n, $self.get_macro_id()))
    };

    (add $a:tt $b:tt $c:tt) => {
        Stmt::Add(param!($a), param!($b), param!($c))
    };
    (mul $a:tt $b:tt $c:tt) => {
        Stmt::Mul(param!($a), param!($b), param!($c))
    };
    (sub $a:tt $b:tt $c:tt) => {
        Stmt::Sub(param!($a), param!($b), param!($c))
    };
    (div $a:tt $b:tt $c:tt) => {
        Stmt::Div(param!($a), param!($b), param!($c))
    };
    (divmod $a:tt $b:tt $c:tt $d:tt) => {
        Stmt::DivMod(param!($a), param!($b), param!($c), param!($d))
    };

    (eq $a:tt $b:tt $c:tt) => {
        Stmt::Equal(param!($a), param!($b), param!($c))
    };
    (lt $a:tt $b:tt $c:tt) => {
        Stmt::LessThan(param!($a), param!($b), param!($c))
    };
    (leq $a:tt $b:tt $c:tt) => {
        Stmt::LessEqual(param!($a), param!($b), param!($c))
    };
    (gt $a:tt $b:tt $c:tt) => {
        Stmt::GreaterThan(param!($a), param!($b), param!($c))
    };
    (geq $a:tt $b:tt $c:tt) => {
        Stmt::GreaterEqual(param!($a), param!($b), param!($c))
    };

    (and $a:tt $b:tt $c:tt) => {
        Stmt::And(param!($a), param!($b), param!($c))
    };
    (or $a:tt $b:tt $c:tt) => {
        Stmt::Or(param!($a), param!($b), param!($c))
    };
    (not $a:tt $b:tt) => {
        Stmt::Not(param!($a), param!($b))
    };

    (in $a:tt) => {
        Stmt::In(param!($a))
    };
    (out $a:tt) => {
        Stmt::Out(param!($a))
    };
    (add_rel_base $a:tt) => {
        Stmt::AddRelBase(param!($a))
    };
    (mov $a:tt $b:tt) => {
        Stmt::Mov(param!($a), param!($b))
    };
    (load $a:tt $b:tt) => {
        Stmt::Load(param!($a), param!($b))
    };
    (store $a:tt $b:tt) => {
        Stmt::Store(param!($a), param!($b))
    };
    (push $a:tt) => {
        Stmt::Push(param!($a))
    };
    (pop $a:tt) => {
        Stmt::Pop(param!($a))
    };
    (call $a:tt) => {
        Stmt::Call(param!($a))
    };
    (ret) => {
        Stmt::Ret
    };

    (jmp $a:tt) => {
        Stmt::Jmp(param!($a))
    };
    (jnz $a:tt $b:tt) => {
        Stmt::JumpTrue(param!($a), param!($b))
    };
    (jmp_true $a:tt $b:tt) => {
        Stmt::JumpTrue(param!($a), param!($b))
    };
    (jz $a:tt $b:tt) => {
        Stmt::JumpFalse(param!($a), param!($b))
    };
    (jmp_false $a:tt $b:tt) => {
        Stmt::JumpFalse(param!($a), param!($b))
    };

    (data $($t:tt)+) => {
        Stmt::Data(vec![$(param!($t)),+])
    };

    (array $a:tt $b:expr) => {
        Stmt::DataArray($a, $b)
    };
}

#[derive(Default)]
struct Assembler {
    code: Vec<i64>,
    labels: HashMap<Ident, usize>,
    patches: HashMap<Ident, Vec<usize>>,
    macro_counter: u32,
    increase_macro_counter: bool,
    has_halt: bool,
}

impl Assembler {
    fn new() -> Self {
        Self {
            macro_counter: 1,
            ..Self::default()
        }
    }

    fn assemble(mut self, stmts: &[Stmt]) -> Vec<i64> {
        self.assemble_stmts(stmts);

        let end_patches = self.patches.remove(&Ident("__end", 0));
        for (k, locs) in self.patches {
            let pos = match self.labels.get(&k) {
                Some(pos) => *pos,
                None => {
                    let pos = self.code.len();
                    self.code.push(0);
                    pos
                }
            } as i64;

            for loc in locs {
                self.code[loc] += pos;
            }
        }

        if let Some(end_patches) = end_patches {
            for loc in end_patches {
                self.code[loc] += self.code.len() as i64;
            }
        }

        if !self.has_halt {
            eprintln!("Warning: The program has no 'hlt' instruction!");
        }

        while let Some(val) = self.code.last() {
            if *val != 0 {
                break;
            }
            self.code.pop();
        }

        self.code
    }

    fn assemble_stmts(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.assemble_stmt(stmt);
        }
    }

    fn assemble_stmt(&mut self, stmt: &Stmt) {
        use Stmt::*;
        match stmt {
            Label(ident) => match self.labels.entry(*ident) {
                Entry::Occupied(_) => panic!("Duplicate label definition: {:?}", ident),
                Entry::Vacant(entry) => {
                    entry.insert(self.code.len());
                }
            },
            Add(a, b, c) | Mul(a, b, c) | LessThan(a, b, c) | Equal(a, b, c) => {
                let (a, ma) = self.assemble_param(1, a);
                let (b, mb) = self.assemble_param(2, b);
                let (c, mc) = self.assemble_param(3, c);
                self.push(stmt.opcode() + ma * 100 + mb * 1_000 + mc * 10_000);
                self.push(a);
                self.push(b);
                self.push(c);
            }
            JumpTrue(a, b) | JumpFalse(a, b) => {
                let (a, ma) = self.assemble_param(1, a);
                let (b, mb) = self.assemble_param(2, b);
                self.push(stmt.opcode() + ma * 100 + mb * 1_000);
                self.push(a);
                self.push(b);
            }
            In(a) | Out(a) | AddRelBase(a) => {
                let (a, ma) = self.assemble_param(1, a);
                self.push(stmt.opcode() + ma * 100);
                self.push(a);
            }
            Data(vals) => {
                for val in vals {
                    let (v, _) = self.assemble_param(0, val);
                    self.push(v);
                }
            }
            DataArray(val, len) => {
                for _ in 0..*len as usize {
                    self.push(*val);
                }
            }
            Halt => {
                self.has_halt = true;
                self.push(99);
            }

            // target = memory[a]
            Load(a, b) => {
                let pos = self.code.len() as i64 + 5;
                assemble!(self,
                    [add a 0 [pos]],
                    [add [0] 0 b],
                );
            }
            // memory[b] = a
            Store(a, b) => {
                let pos = self.code.len() as i64 + 7;
                assemble!(self,
                    [add b 0 [pos]],
                    [add a 0 [0]],
                );
            }
            Push(a) => assemble!(self,
                [mov a (rel 0)],
                [add_rel_base 1],
            ),
            Pop(a) => assemble!(self,
                [add_rel_base (-1)],
                [mov (rel 0) a],
            ),
            Call(a) => assemble!(self,
                    [push (self ":__ret")],
                    [jmp a],
                    [label self "__ret"],
                ),
            Ret => assemble!(self,
                [add_rel_base (-1)],
                [jmp (rel 0)],
            ),

            Mov(a, b) => assemble!(self, [add a 0 b]),
            Sub(a, b, c) => assemble!(self,
                [mul (-1) b "__tmp"],
                [add a "__tmp" c],
            ),
            Div(a, b, c) => assemble!(self, [divmod a b c "__rest"]),
            Mod(a, b, c) => assemble!(self,
                [mov b "__div"],
                [mov a c],
                [label self "__loop"],
                [lt c "__div" "__tmp"],
                [jmp_true "__tmp" (self ":__end")],
                [sub c "__div" c],
                [jmp (self ":__loop")],
                [label self "__end"],
            ),
            DivMod(a, b, c, d) => assemble!(self,
                [mov b "__div"],
                [mov a d],
                [mov 0 c],
                [label self "__loop"],
                [lt d "__div" "__tmp"],
                [jmp_true "__tmp" (self ":__end")],
                [sub d "__div" d],
                [add c 1 c],
                [jmp (self ":__loop")],
                [label self "__end"],
            ),
            Jmp(a) => assemble!(self, [jz 0 a]),
            And(a, b, c) => assemble!(self,
                [jmp_false a (self ":__first")],
                [mov b c],
                [jmp (self ":__end")],
                [label self "__first"],
                [mov a c],
                [label self "__end"],
            ),
            Or(a, b, c) => assemble!(self,
                [jmp_true a (self ":__first")],
                [mov b c],
                [jmp (self ":__end")],
                [label self "__first"],
                [mov a c],
                [label self "__end"],
            ),
            Not(a, b) => assemble!(self,
                [jmp_false a (self ":__false")],
                [mov 1 b],
                [jmp (self ":__end")],
                [label self "__false"],
                [mov 0 b],
                [label self "__end"],
            ),
            NotEqual(a, b, c) => assemble!(self,
                [eq a b "__tmp"],
                [not "__tmp" c]
            ),
            LessEqual(a, b, c) => assemble!(self,
                [lt a b "__tmp"],
                [eq a b c],
                [or "__tmp" c c],
            ),
            GreaterThan(a, b, c) => assemble!(self,
                [lt b a c],
            ),
            GreaterEqual(a, b, c) => assemble!(self,
                [leq b a c],
            ),
        }

        if self.increase_macro_counter {
            self.increase_macro_counter = false;
            self.macro_counter += 1;
        }
    }

    fn assemble_param(&mut self, i: usize, param: &Param) -> (i64, i64) {
        match param {
            Param::Ident(mode, ident) => {
                self.patches
                    .entry(*ident)
                    .or_default()
                    .push(self.code.len() + i);
                (0, mode.into())
            }
            Param::Value(mode, val) => (*val, mode.into()),
        }
    }

    fn get_macro_id(&mut self) -> u32 {
        self.increase_macro_counter = true;
        self.macro_counter
    }

    fn push(&mut self, val: i64) {
        self.code.push(val);
    }
}

pub fn assemble(stmts: &[Stmt]) -> Vec<i64> {
    Assembler::new().assemble(stmts)
}
