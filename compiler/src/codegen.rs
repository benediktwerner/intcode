use std::collections::HashMap;

use intcode_asm as asm;

use crate::ast::*;

const TMP: asm::Ident = asm::Ident("__compiler_tmp", 0);
const TMP2: asm::Ident = asm::Ident("__compiler_tmp_2", 0);

type Result<T = ()> = std::result::Result<T, CompilerError>;

pub enum CompilerError {
    DuplicateDeclaration(Ident),
    UndefinedVar(Ident),
    BreakOutsideLoop,
    ContinueOutsideLoop,
    ReturnOutsideFunc,
    FuncInConst,
    IndexInConst,
    UndefinedVarInConst,
}

pub fn gen(program: Program, env: &IdentEnv) -> Result<Vec<asm::Stmt>> {
    Gen::new(env).generate(program)
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Target {
    StackTop,
    Var(VarLocation),
}

impl From<VarLocation> for Target {
    fn from(loc: VarLocation) -> Self {
        Self::Var(loc)
    }
}

impl From<asm::Ident> for Target {
    fn from(ident: asm::Ident) -> Self {
        VarLocation::from(ident).into()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum VarLocation {
    Local(i64),
    Global(asm::Ident),
}

impl From<asm::Ident> for VarLocation {
    fn from(ident: asm::Ident) -> Self {
        Self::Global(ident)
    }
}

struct Gen<'a> {
    scopes: Vec<HashMap<Ident, VarLocation>>,
    code: Vec<asm::Stmt>,
    label_count: u32,
    next_var_offset: u32,
    stack_height: u32,
    global_arrays: Vec<(asm::Ident, u32)>,
    inside_func: bool,
    label_continue: Option<asm::Ident>,
    label_break: Option<asm::Ident>,
    consts: HashMap<Ident, i64>,
    env: &'a IdentEnv<'a>,
}

impl<'a> Gen<'a> {
    fn new(env: &'a IdentEnv<'a>) -> Self {
        Gen {
            scopes: vec![HashMap::new()],
            code: Vec::new(),
            label_count: 0,
            next_var_offset: 0,
            stack_height: 0,
            global_arrays: Vec::new(),
            inside_func: false,
            label_continue: None,
            label_break: None,
            consts: HashMap::new(),
            env,
        }
    }

    fn generate(mut self, program: Program) -> Result<Vec<asm::Stmt>> {
        self.push(asm::Stmt::AddRelBase(asm::Ident("__end", 0).to_imm()));

        for stmt in program.stmts {
            self.collect_consts(&stmt)?;
            self.gen_stmt(stmt)?;
        }

        self.push(asm::Stmt::Halt);

        for func in program.functions {
            self.gen_func(func)?;
        }

        let mut code = self.code;

        for (ident, size) in self.global_arrays {
            code.push(asm::Stmt::Label(ident));
            code.push(asm::Stmt::DataArray(0, size as usize));
        }

        Ok(code)
    }

    fn decl(&mut self, ident: Ident, size: u32) -> Result {
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(&ident) {
            return Err(CompilerError::DuplicateDeclaration(ident));
        }
        if self.inside_func {
            scope.insert(ident, VarLocation::Local(self.next_var_offset as i64));
            self.next_var_offset += size;
        } else {
            let asm_ident = asm::Ident("__compiler_global", ident);
            scope.insert(ident, VarLocation::Global(asm_ident));
            if size > 1 {
                self.global_arrays.push((asm_ident, size));
            }
        }
        Ok(())
    }

    fn gen_func(&mut self, func: Function) -> Result {
        let mut scope = HashMap::new();
        for (i, arg) in func.args.iter().rev().enumerate() {
            scope.insert(*arg, VarLocation::Local(-(i as i64) - 1));
        }
        self.scopes.push(scope);

        self.stack_height = 0;
        self.next_var_offset = 1;
        self.inside_func = true;

        self.push(asm::Stmt::Label(asm::Ident("__compiler_func", func.name)));
        self.collect_consts(&func.body)?;

        let locals_size = self.locals_size(&func.body)?;
        self.add_stack_ptr(locals_size);

        self.gen_stmt(func.body)?;
        self.gen_stmt(Stmt::Return(None))?;

        self.inside_func = false;

        Ok(())
    }

    fn gen_expr(&mut self, expr: Expr, target: impl Into<Target>) -> Result {
        let target = target.into();
        match expr {
            Expr::Func(name, args) => {
                // TODO: check arg count
                match self.env.to_str(name) {
                    "print" => {
                        self.gen_expr(args.into_iter().next().unwrap(), TMP)?;
                        self.push(asm::Stmt::Out(TMP.to_pos()));
                    }
                    "input" => {
                        self.push(asm::Stmt::In(TMP.to_pos()));
                        self.mov(TMP.to_pos(), target);
                    }
                    "exit" => {
                        self.push(asm::Stmt::Halt);
                    }
                    _ => {
                        let argc = args.len() as i64;
                        for arg in args {
                            self.gen_expr(arg, Target::StackTop)?;
                        }
                        self.push(asm::Stmt::Call(
                            asm::Ident("__compiler_func", name).to_imm(),
                        ));
                        self.add_stack_ptr(-argc);
                        self.mov(TMP.to_pos(), target);
                    }
                }
            }
            Expr::Index(name, index) => {
                self.gen_expr(*index, TMP)?;
                match self.get_location(name)? {
                    VarLocation::Global(ident) => {
                        self.push(asm::Stmt::Add(ident.to_imm(), TMP.to_pos(), TMP.to_pos()));
                        self.push(asm::Stmt::Load(TMP.to_pos(), TMP.to_pos()));
                        self.mov(TMP.to_pos(), target);
                    }
                    VarLocation::Local(pos) => {
                        self.push(asm::Stmt::AddRelBase(TMP.to_pos()));
                        self.push(asm::Stmt::Mov(self.stack_index(pos), TMP2.to_pos()));
                        self.push(asm::Stmt::Mul(TMP.to_pos(), (-1).into(), TMP.to_pos()));
                        self.push(asm::Stmt::AddRelBase(TMP.to_pos()));
                        self.mov(TMP2.to_pos(), target);
                    }
                }
            }
            Expr::BinOp(left, op, right) => {
                self.gen_expr(*left, Target::StackTop)?;
                self.gen_expr(*right, TMP)?;
                self.pop_stack(TMP2.to_pos());
                let op = match op {
                    BinOp::Add => asm::Stmt::Add,
                    BinOp::Sub => asm::Stmt::Sub,
                    BinOp::Mul => asm::Stmt::Mul,
                    BinOp::Div => asm::Stmt::Div,
                    BinOp::Mod => asm::Stmt::Mod,
                    BinOp::Equal => asm::Stmt::Equal,
                    BinOp::NotEqual => asm::Stmt::NotEqual,
                    BinOp::LessThan => asm::Stmt::LessThan,
                    BinOp::LessEqual => asm::Stmt::LessEqual,
                    BinOp::GreaterThan => asm::Stmt::GreaterThan,
                    BinOp::GreaterEqual => asm::Stmt::GreaterEqual,
                    BinOp::And => asm::Stmt::And,
                    BinOp::Or => asm::Stmt::Or,
                };
                self.push(op(TMP2.to_pos(), TMP.to_pos(), TMP.to_pos()));
                self.mov(TMP.to_pos(), target);
            }
            Expr::UnOp(op, expr) => {
                self.gen_expr(*expr, TMP)?;
                match op {
                    UnOp::Neg => self.push(asm::Stmt::Mul(TMP.to_pos(), (-1).into(), TMP.to_pos())),
                    UnOp::Not => self.push(asm::Stmt::Not(TMP.to_pos(), TMP.to_pos())),
                }
                self.mov(TMP.to_pos(), target);
            }
            Expr::Var(name) => {
                if let Some(val) = self.consts.get(&name) {
                    let val = *val;
                    self.mov(val, target);
                } else {
                    match self.get_location(name)? {
                        VarLocation::Global(ident) => self.mov(ident.to_pos(), target),
                        VarLocation::Local(pos) => {
                            self.push(asm::Stmt::Mov(self.stack_index(pos), TMP.to_pos()));
                            self.mov(TMP.to_pos(), target);
                        }
                    }
                }
            }
            Expr::Literal(val) => self.mov(val, target),
        }
        Ok(())
    }

    fn get_location(&self, ident: Ident) -> Result<VarLocation> {
        for scope in self.scopes.iter().rev() {
            if let Some(loc) = scope.get(&ident) {
                return Ok(*loc);
            }
        }
        Err(CompilerError::UndefinedVar(ident))
    }

    fn mov(&mut self, val: impl Into<asm::Param>, target: Target) {
        let val = val.into();
        match target {
            Target::StackTop => self.push_stack(val),
            Target::Var(VarLocation::Global(target)) => {
                if let asm::Param::Ident(asm::ParamMode::Positional, val) = val {
                    if val == target {
                        return;
                    }
                }
                self.push(asm::Stmt::Mov(val, target.to_pos()));
            }
            Target::Var(VarLocation::Local(pos)) => {
                self.push(asm::Stmt::Mov(val, self.stack_index(pos)));
            }
        }
    }
    fn gen_stmt(&mut self, stmt: Stmt) -> Result {
        match stmt {
            Stmt::Decl(ident) => self.decl(ident, 1)?,
            Stmt::DeclAssign(ident, expr) => {
                self.decl(ident, 1)?;
                self.gen_expr(expr, self.get_location(ident)?)?;
            }
            Stmt::ConstAssign(..) => (),
            Stmt::DeclArray(ident, size) => self.decl(ident, self.const_eval(&size)? as u32)?,
            Stmt::Assign(ident, expr) => self.gen_expr(expr, self.get_location(ident)?)?,
            Stmt::AssignOp(ident, op, expr) => {
                let expr = Expr::BinOp(Box::new(Expr::Var(ident)), op, Box::new(expr));
                self.gen_expr(expr, self.get_location(ident)?)?;
            }
            Stmt::AssignIndex(ident, index, expr) => {
                self.gen_expr(expr, Target::StackTop)?;
                self.gen_expr(index, TMP)?;
                self.pop_stack(TMP2.to_pos());
                match self.get_location(ident)? {
                    VarLocation::Global(ident) => {
                        self.push(asm::Stmt::Add(ident.to_imm(), TMP.to_pos(), TMP.to_pos()));
                        self.push(asm::Stmt::Store(TMP2.to_pos(), TMP.to_pos()));
                    }
                    VarLocation::Local(pos) => {
                        self.push(asm::Stmt::AddRelBase(TMP.to_pos()));
                        self.push(asm::Stmt::Mov(TMP2.to_pos(), self.stack_index(pos)));
                        self.push(asm::Stmt::Mul(TMP.to_pos(), (-1).into(), TMP.to_pos()));
                        self.push(asm::Stmt::AddRelBase(TMP.to_pos()));
                    }
                }
            }
            Stmt::Block(stmts) => {
                self.scopes.push(HashMap::new());
                for stmt in stmts {
                    self.gen_stmt(stmt)?;
                }
                self.scopes.pop();
            }
            Stmt::If(cond, body) => {
                self.gen_expr(cond, TMP)?;
                let label = self.make_label();
                self.jmp_false(TMP, label);
                self.gen_stmt(*body)?;
                self.label(label);
            }
            Stmt::IfElse(cond, if_body, else_body) => {
                self.gen_expr(cond, TMP)?;
                let label_false = self.make_label();
                let label_end = self.make_label();
                self.jmp_false(TMP, label_false);
                self.gen_stmt(*if_body)?;
                self.jmp(label_end);
                self.label(label_false);
                self.gen_stmt(*else_body)?;
                self.label(label_end);
            }
            Stmt::While(cond, body) => {
                let label_start = self.make_label();
                let label_end = self.make_label();
                self.label_continue = Some(label_start);
                self.label_break = Some(label_end);

                self.label(label_start);
                self.gen_expr(cond, TMP)?;
                self.jmp_false(TMP, label_end);
                self.gen_stmt(*body)?;
                self.jmp(label_start);
                self.label(label_end);

                self.label_continue = None;
                self.label_break = None;
            }
            Stmt::Break => {
                if let Some(target) = self.label_break {
                    self.jmp(target);
                } else {
                    return Err(CompilerError::BreakOutsideLoop);
                }
            }
            Stmt::Continue => {
                if let Some(target) = self.label_continue {
                    self.jmp(target);
                } else {
                    return Err(CompilerError::ContinueOutsideLoop);
                }
            }
            Stmt::Return(val) => {
                if !self.inside_func {
                    return Err(CompilerError::ReturnOutsideFunc);
                }
                if let Some(val) = val {
                    self.gen_expr(val, TMP)?;
                } else {
                    self.push(asm::Stmt::Mov(0.into(), TMP.to_pos()));
                }
                self.add_stack_ptr(-(self.stack_height as i64));
                self.push(asm::Stmt::Ret);
            }
            Stmt::Expr(expr) => self.gen_expr(expr, TMP)?,
        }
        Ok(())
    }

    fn make_label(&mut self) -> asm::Ident {
        let index = self.label_count;
        self.label_count += 1;
        asm::Ident("__compiler_label", index)
    }

    fn jmp(&mut self, target: asm::Ident) {
        self.push(asm::Stmt::Jmp(target.to_imm()))
    }

    fn jmp_false(&mut self, cond: asm::Ident, target: asm::Ident) {
        self.push(asm::Stmt::JumpFalse(cond.to_pos(), target.to_imm()))
    }

    fn label(&mut self, ident: impl Into<asm::Ident>) {
        self.push(asm::Stmt::Label(ident.into()))
    }

    fn push_stack(&mut self, val: asm::Param) {
        self.push(asm::Stmt::Push(val));
        self.stack_height += 1;
    }

    fn pop_stack(&mut self, val: asm::Param) {
        self.push(asm::Stmt::Pop(val));
        self.stack_height -= 1;
    }

    fn stack_index(&self, offset: i64) -> asm::Param {
        asm::Param::Value(
            asm::ParamMode::Relative,
            offset - self.stack_height as i64 - 1,
        )
    }

    fn add_stack_ptr(&mut self, amount: i64) {
        if amount != 0 {
            self.push(asm::Stmt::AddRelBase(amount.into()));
            self.stack_height = (self.stack_height as i64 + amount) as u32;
        }
    }

    fn push(&mut self, stmt: asm::Stmt) {
        self.code.push(stmt);
    }

    fn const_eval(&self, expr: &Expr) -> Result<i64> {
        let val = match expr {
            Expr::Func(..) => return Err(CompilerError::FuncInConst),
            Expr::Index(..) => return Err(CompilerError::IndexInConst),
            Expr::BinOp(left, op, right) => {
                let left = self.const_eval(left)?;
                let right = self.const_eval(right)?;
                match op {
                    BinOp::Add => left + right,
                    BinOp::Sub => left - right,
                    BinOp::Mul => left * right,
                    BinOp::Div => left / right,
                    BinOp::Mod => left % right,
                    BinOp::Equal => bool2int(left == right),
                    BinOp::NotEqual => bool2int(left != right),
                    BinOp::LessThan => bool2int(left < right),
                    BinOp::LessEqual => bool2int(left <= right),
                    BinOp::GreaterThan => bool2int(left > right),
                    BinOp::GreaterEqual => bool2int(left >= right),
                    BinOp::And => bool2int(left != 0 && right != 0),
                    BinOp::Or => bool2int(left != 0 || right != 0),
                }
            }
            Expr::UnOp(op, expr) => {
                let val = self.const_eval(expr)?;
                match op {
                    UnOp::Neg => -val,
                    UnOp::Not => bool2int(val == 0),
                }
            }
            Expr::Var(ident) => {
                if let Some(val) = self.consts.get(ident) {
                    *val
                } else {
                    return Err(CompilerError::UndefinedVarInConst);
                }
            }
            Expr::Literal(val) => *val,
        };
        Ok(val)
    }

    fn locals_size(&self, stmt: &Stmt) -> Result<i64> {
        Ok(match stmt {
            Stmt::Decl(..) => 1,
            Stmt::DeclAssign(..) => 1,
            Stmt::ConstAssign(..) => 0,
            Stmt::DeclArray(_, size) => self.const_eval(size)?,
            Stmt::Assign(..) => 0,
            Stmt::AssignOp(..) => 0,
            Stmt::AssignIndex(..) => 0,
            Stmt::Block(stmts) => {
                let mut total = 0;
                for stmt in stmts {
                    total += self.locals_size(stmt)?;
                }
                total
            }
            Stmt::If(_, body) => self.locals_size(body)?,
            Stmt::IfElse(_, if_body, else_body) => {
                self.locals_size(if_body)? + self.locals_size(else_body)?
            }
            Stmt::While(_, body) => self.locals_size(body)?,
            Stmt::Break => 0,
            Stmt::Continue => 0,
            Stmt::Return(..) => 0,
            Stmt::Expr(..) => 0,
        })
    }

    fn collect_consts(&mut self, stmt: &Stmt) -> Result {
        match stmt {
            Stmt::Decl(..) => (),
            Stmt::DeclAssign(..) => (),
            Stmt::ConstAssign(ident, expr) => {
                let val = self.const_eval(expr)?;
                self.consts.insert(*ident, val);
            }
            Stmt::AssignOp(..) => (),
            Stmt::DeclArray(..) => (),
            Stmt::Assign(..) => (),
            Stmt::AssignIndex(..) => (),
            Stmt::Block(stmts) => {
                for stmt in stmts {
                    self.collect_consts(stmt)?;
                }
            }
            Stmt::If(_, body) => self.collect_consts(body)?,
            Stmt::IfElse(_, if_body, else_body) => {
                self.collect_consts(if_body)?;
                self.collect_consts(else_body)?;
            }
            Stmt::While(_, body) => self.collect_consts(body)?,
            Stmt::Break => (),
            Stmt::Continue => (),
            Stmt::Return(..) => (),
            Stmt::Expr(..) => (),
        }
        Ok(())
    }
}

fn bool2int(val: bool) -> i64 {
    if val {
        1
    } else {
        0
    }
}
