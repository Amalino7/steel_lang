pub mod analysis;

use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::Literal;
use crate::typechecker::type_ast::{
    BinaryOp, ExprKind, LogicalOp, StmtKind, TypedExpr, TypedStmt, UnaryOp,
};
use crate::vm::bytecode::{Chunk, Opcode};
use crate::vm::gc::GarbageCollector;
use crate::vm::value::{Function, Value};

pub struct Compiler<'a> {
    function: Function,
    gc: &'a mut GarbageCollector,
}

impl<'a> Compiler<'a> {
    pub fn new(name: String, gc: &'a mut GarbageCollector) -> Self {
        Self {
            function: Function::new(name, Chunk::new()),
            gc,
        }
    }
    fn chunk(&mut self) -> &mut Chunk {
        &mut self.function.chunk
    }

    pub fn compile(mut self, typed_ast: &TypedStmt) -> Function {
        self.compile_stmt(typed_ast);

        self.chunk().write_constant(Value::Nil, 0);
        self.emit_op(Opcode::Return, 0);
        self.function
    }
    fn compile_stmt(&mut self, stmt: &TypedStmt) {
        match &stmt.kind {
            StmtKind::Expression(e) => {
                self.compile_expr(e);
                self.emit_op(Opcode::Pop, stmt.line);
            }
            StmtKind::Let { target, value } => {
                self.compile_expr(value);

                match target {
                    ResolvedVar::Local(_) => {
                        // there is no need to set the variable it is already in the right slot
                    }
                    ResolvedVar::Global(idx) => {
                        self.emit_op(Opcode::SetGlobal, stmt.line);
                        self.emit_byte(*idx as u8, stmt.line);

                        self.emit_op(Opcode::Pop, stmt.line);
                    }
                    ResolvedVar::Closure(_) => {
                        unreachable!("Closures shouldn't be assigned to")
                    }
                }
            }
            StmtKind::Block {
                body: stmts,
                variable_count,
            } => {
                for stmt in stmts {
                    self.compile_stmt(stmt);
                }
                for _ in 0..*variable_count {
                    self.emit_op(Opcode::Pop, stmt.line);
                }
            }
            StmtKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let line = stmt.line;
                self.compile_expr(condition);
                let then_jump = self.emit_jump(Opcode::JumpIfFalse, line);
                self.emit_op(Opcode::Pop, line);

                self.compile_stmt(then_branch);

                let else_jump = self.emit_jump(Opcode::Jump, line);
                self.patch_jump(then_jump);

                self.emit_op(Opcode::Pop, line);

                if let Some(else_branch) = else_branch {
                    self.compile_stmt(else_branch);
                }

                self.patch_jump(else_jump);
            }
            StmtKind::While { condition, body } => {
                let line = stmt.line;

                let loop_start = self.chunk().instructions.len();
                self.compile_expr(condition);

                let exit_jump = self.emit_jump(Opcode::JumpIfFalse, line);
                self.emit_op(Opcode::Pop, line);

                self.compile_stmt(body);
                self.emit_jump_back(loop_start, body.line);

                self.patch_jump(exit_jump);
                self.emit_op(Opcode::Pop, body.line);
            }
            StmtKind::Impl { methods, vtables } => {
                for method in methods {
                    self.compile_stmt(method);
                }

                for vtable in vtables.iter() {
                    for method_loc in vtable.iter().rev() {
                        self.emit_var_access(method_loc, stmt.line);
                    }

                    self.emit_op(Opcode::MakeVTable, stmt.line);
                    self.emit_byte(vtable.len() as u8, stmt.line);
                }
            }
            StmtKind::Function {
                target,
                name,
                body,
                captures,
            } => {
                let line = stmt.line;
                let func_compiler = Compiler::new(name.to_string(), self.gc);

                let compiled_fn = func_compiler.compile(body);
                let constant = Value::Function(self.gc.alloc(compiled_fn));
                self.chunk().write_constant(constant, line as usize);

                // Add captures
                // Reverse captures to make sure they are in the right order
                for capture in captures.iter().rev() {
                    self.emit_var_access(capture, line)
                }
                if !captures.is_empty() {
                    self.emit_op(Opcode::MakeClosure, line);
                    self.emit_byte(captures.len() as u8, line);
                }

                match target {
                    ResolvedVar::Local(_) => {
                        // there is no need to set the variable it is already in the right slot
                    }
                    ResolvedVar::Global(idx) => {
                        self.emit_op(Opcode::SetGlobal, line);
                        self.emit_byte(*idx as u8, line);

                        self.emit_op(Opcode::Pop, line);
                    }
                    ResolvedVar::Closure(_) => {
                        unreachable!("Closures shouldn't be assigned to")
                    }
                }
            }
            StmtKind::Return(val) => {
                self.compile_expr(val);
                self.emit_op(Opcode::Return, stmt.line);
            }
            StmtKind::Global { stmts: stmt, .. } => {
                for s in stmt {
                    match &s.kind {
                        StmtKind::Function { .. } => self.compile_stmt(s),
                        StmtKind::Impl { .. } => {
                            self.compile_stmt(s);
                        }
                        _ => {}
                    }
                }

                // Second: compile the rest
                for s in stmt {
                    match &s.kind {
                        StmtKind::Function { .. } | StmtKind::Impl { .. } => continue,
                        _ => self.compile_stmt(s),
                    }
                }
            }
            StmtKind::StructDecl { .. } => {}
        }
    }

    fn emit_var_access(&mut self, var_ctx: &ResolvedVar, line: u32) {
        match var_ctx {
            ResolvedVar::Local(idx) => {
                self.emit_op(Opcode::GetLocal, line);
                self.emit_byte(*idx, line);
            }
            ResolvedVar::Global(idx) => {
                self.emit_op(Opcode::GetGlobal, line);
                self.emit_byte(*idx as u8, line);
            }
            ResolvedVar::Closure(idx) => {
                self.emit_op(Opcode::GetCapture, line);
                self.emit_byte(*idx, line);
            }
        }
    }

    fn emit_jump(&mut self, op: Opcode, line: u32) -> usize {
        self.emit_op(op, line);
        self.emit_byte(0xff, line); // Placeholder
        self.emit_byte(0xff, line); // Placeholder
        self.chunk().instructions.len() - 2
    }
    fn emit_jump_back(&mut self, start: usize, line: u32) {
        self.emit_op(Opcode::JumpBack, line);
        let offset = self.chunk().instructions.len() - start + 2;
        if offset > u16::MAX as usize {
            panic!("Loop body too large!");
        }
        self.emit_byte(((offset >> 8) & 0xff) as u8, line);
        self.emit_byte((offset & 0xff) as u8, line);
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.chunk().instructions.len() - offset - 2;
        if jump > u16::MAX as usize {
            panic!("Too much code to jump over!");
        }
        self.chunk().instructions[offset] = ((jump >> 8) & 0xff) as u8;
        self.chunk().instructions[offset + 1] = (jump & 0xff) as u8;
    }

    fn compile_expr(&mut self, expr: &TypedExpr) {
        let line = expr.line;
        match &expr.kind {
            ExprKind::Binary {
                left,
                right,
                operator,
            } => {
                self.compile_expr(left);
                self.compile_expr(right);
                match operator {
                    BinaryOp::Concat => self.emit_op(Opcode::Concat, line),
                    BinaryOp::Add => self.emit_op(Opcode::Add, line),
                    BinaryOp::Subtract => self.emit_op(Opcode::Subtract, line),
                    BinaryOp::Multiply => self.emit_op(Opcode::Multiply, line),
                    BinaryOp::Divide => self.emit_op(Opcode::Divide, line),
                    BinaryOp::LessString => self.emit_op(Opcode::LessString, line),
                    BinaryOp::LessNumber => self.emit_op(Opcode::LessNumber, line),
                    BinaryOp::GreaterString => self.emit_op(Opcode::GreaterString, line),
                    BinaryOp::GreaterNumber => self.emit_op(Opcode::GreaterNumber, line),
                    BinaryOp::GreaterEqualString => {
                        self.emit_op(Opcode::LessString, line);
                        self.emit_op(Opcode::Not, line);
                    }
                    BinaryOp::GreaterEqualNumber => {
                        self.emit_op(Opcode::LessNumber, line);
                        self.emit_op(Opcode::Not, line);
                    }
                    BinaryOp::LessEqualNumber => {
                        self.emit_op(Opcode::GreaterNumber, line);
                        self.emit_op(Opcode::Not, line);
                    }
                    BinaryOp::LessEqualString => {
                        self.emit_op(Opcode::GreaterString, line);
                        self.emit_op(Opcode::Not, line);
                    }
                    BinaryOp::EqualEqualNumber => self.emit_op(Opcode::EqualNumber, line),
                    BinaryOp::EqualEqualString => self.emit_op(Opcode::EqualString, line),
                    BinaryOp::EqualEqual => self.emit_op(Opcode::Equal, line),
                }
            }
            ExprKind::Literal(literal) => match literal {
                Literal::Number(n) => self
                    .chunk()
                    .write_constant(Value::Number(*n), line as usize),
                Literal::String(s) => {
                    let str = self.gc.alloc(s.to_string());
                    self.chunk()
                        .write_constant(Value::String(str), line as usize)
                }
                Literal::Boolean(b) => {
                    self.chunk()
                        .write_constant(Value::Boolean(*b), line as usize);
                }
                Literal::Void => {
                    self.chunk().write_constant(Value::Nil, line as usize);
                }
            },

            ExprKind::Unary { operator, operand } => {
                self.compile_expr(operand);
                match operator {
                    UnaryOp::Negate => self.emit_op(Opcode::Negate, line),
                    UnaryOp::Not => self.emit_op(Opcode::Not, line),
                }
            }
            ExprKind::GetVar(resolved) => self.emit_var_access(resolved, line),
            ExprKind::Assign { target, value } => {
                self.compile_expr(value);

                match target {
                    ResolvedVar::Local(idx) => {
                        self.emit_op(Opcode::SetLocal, line);
                        self.emit_byte(*idx as u8, line);
                    }
                    ResolvedVar::Global(idx) => {
                        self.emit_op(Opcode::SetGlobal, line);
                        self.emit_byte(*idx as u8, line);
                    }
                    ResolvedVar::Closure(_) => {
                        unreachable!("Closures shouldn't be assigned to")
                    }
                }
            }
            ExprKind::Logical {
                left,
                operator,
                right,
            } => {
                self.compile_expr(left);
                match operator {
                    LogicalOp::Or => {
                        // TODO Add jump if true for better performance
                        let else_jump = self.emit_jump(Opcode::JumpIfFalse, line);
                        let end_jump = self.emit_jump(Opcode::Jump, line);
                        self.patch_jump(else_jump);
                        self.emit_op(Opcode::Pop, line);
                        self.compile_expr(right);
                        self.patch_jump(end_jump);
                    }
                    LogicalOp::And => {
                        let short_circuit = self.emit_jump(Opcode::JumpIfFalse, line);
                        self.emit_op(Opcode::Pop, line);
                        self.compile_expr(right);
                        self.patch_jump(short_circuit);
                    }
                }
            }
            ExprKind::Call { callee, arguments } => {
                if let ExprKind::MethodGet { object, method } = &callee.kind {
                    self.emit_var_access(method, line);
                    self.compile_expr(object);
                    for arg in arguments {
                        self.compile_expr(arg);
                    }

                    self.emit_op(Opcode::Call, line);
                    self.emit_byte((arguments.len() + 1) as u8, line);
                } else if let ExprKind::InterfaceMethodGet {
                    object,
                    method_index,
                } = &callee.kind
                {
                    self.compile_expr(object);
                    self.emit_op(Opcode::GetInterfaceMethod, line);
                    self.emit_byte(*method_index, line);

                    for arg in arguments {
                        self.compile_expr(arg);
                    }
                    self.emit_op(Opcode::Call, line);
                    self.emit_byte((arguments.len() + 1) as u8, line);
                } else {
                    self.compile_expr(callee);
                    for arg in arguments {
                        self.compile_expr(arg);
                    }
                    self.emit_op(Opcode::Call, callee.line);
                    self.emit_byte(
                        arguments.len() as u8,
                        arguments.last().map(|e| e.line).unwrap_or(callee.line),
                    );
                }
            }
            ExprKind::StructInit { args, name } => {
                let str_name = self.gc.alloc(name.to_string());

                self.chunk()
                    .write_constant(Value::String(str_name), line as usize);
                for arg in args.iter().rev() {
                    self.compile_expr(arg);
                }
                self.emit_op(Opcode::StructAlloc, line);
                self.emit_byte(args.len() as u8, line);
            }

            ExprKind::GetField { object, index } => {
                self.compile_expr(object);
                self.emit_op(Opcode::GetField, line);
                self.emit_byte(*index, line);
            }
            ExprKind::SetField {
                object,
                index,
                value,
            } => {
                self.compile_expr(value);
                self.compile_expr(object);
                self.emit_op(Opcode::SetField, line);
                self.emit_byte(*index, line);
            }

            ExprKind::MethodGet { object, method } => {
                self.compile_expr(object);

                self.emit_var_access(method, line);

                self.emit_op(Opcode::BindMethod, line);
            }

            ExprKind::InterfaceUpcast {
                expr: inner,
                vtable_idx,
                ..
            } => {
                self.compile_expr(inner);
                self.emit_op(Opcode::MakeInterfaceObj, line);
                self.emit_byte(*vtable_idx as u8, line);
            }

            ExprKind::InterfaceMethodGet {
                object,
                method_index,
            } => {
                self.compile_expr(object);
                self.emit_op(Opcode::InterfaceBindMethod, line);
                self.emit_byte(*method_index, line);
            }
        }
    }

    fn emit_op(&mut self, op: Opcode, line: u32) {
        self.chunk().write_op(op as u8, line as usize);
    }
    fn emit_byte(&mut self, byte: u8, line: u32) {
        self.chunk().write_op(byte, line as usize);
    }
}
