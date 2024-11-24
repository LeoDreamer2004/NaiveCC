//! Generate IR from AST

use super::ast::*;
use super::builtin::set_up_builtins;
use super::env::Environment;
use super::eval::Eval;
use super::loops::*;
use super::opt::*;
use super::symbol::*;
use crate::utils::namer::{global_ident, normal_ident};
use koopa::back::KoopaGenerator;
use koopa::ir::builder::*;
use koopa::ir::Type;
use koopa::ir::TypeKind;
use koopa::ir::{BinaryOp, FunctionData, Program, Value};
use koopa::opt::{Pass, PassManager};
use std::io;

pub fn build_ir(ast: CompUnit) -> Result<Program, AstError> {
    let mut env = Environment::default();
    ast.generate_on(&mut env)?;
    let mut program = env.program;

    let mut passman = PassManager::new();
    passman.register(Pass::Function(Box::new(DeadBlockElimination::default())));
    // passman.register(Pass::Function(Box::new(BlockGraphSimplifier::default())));
    passman.register(Pass::Function(Box::new(ConstantsInline::default())));
    passman.register(Pass::Function(Box::new(DeadCodeElimination::default())));
    passman.register(Pass::Function(Box::new(CommonSubexpression::default())));
    passman.run_passes(&mut program);
    Ok(program)
}

pub fn emit_ir(program: &mut Program, output: impl io::Write) -> Result<(), io::Error> {
    KoopaGenerator::new(output).generate_on(program)
}

/*********************  Structs  *********************/

#[derive(Debug)]
pub enum AstError {
    FunctionNotFoundError(String),
    IllegalConstExpError(String),
    SymbolNotFoundError(String),
    InitializeError(String),
    IllegalAccessError(String),
    LoopStackError(String),
    AssignError(String),
    TypeError(String),
    UnknownError(String),
}

/// Trait which should be implemented by AST types to generate IR
///
/// Return a value of type `T` if needed
pub trait GenerateIr {
    type IrTarget;

    /// Generate IR on the given env
    ///
    /// # Errors
    /// Returns an [`ParseError`] if the generation fails
    fn generate_on(&self, env: &mut Environment) -> Result<Self::IrTarget, AstError>;
}

impl GenerateIr for CompUnit {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        set_up_builtins(env);

        for comp_item in &self.comp_items {
            match comp_item {
                CompItem::FuncDef(func_def) => {
                    func_def.generate_on(env)?;
                }
                CompItem::Decl(decl) => {
                    decl.generate_on(env)?;
                }
            }
        }
        Ok(())
    }
}

impl GenerateIr for FuncDef {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        env.syb_table.enter_scope();

        let mut p_types = vec![];
        let mut params = vec![];
        for param in &self.params {
            let symbol = env.syb_table.new_symbol(param)?.symbol();
            let ty = symbol.get_type(param.b_type.into())?;
            p_types.push(ty.clone());
            params.push((Some(global_ident(symbol.ident())), ty));
        }

        let func = FunctionData::with_param_names(
            global_ident(&self.ident),
            params,
            self.func_type.into(),
        );

        let func = env.program.new_func(func);
        env.func(func);
        env.create_block(Some("%entry".into()));

        // Add parameters to the symbol table
        for (i, param) in self.params.iter().enumerate() {
            let value = env.func_data().params()[i];
            let alloc = env.alloc_and_store(value, p_types[i].clone());
            env.set_value_name(alloc, normal_ident(&param.ident));
            env.syb_table.set_alloc(&param.get_ident(), alloc)?;
        }
        self.block.generate_on(env)?;
        env.syb_table.exit_scope();

        let ret = env.local_value().ret(None);
        env.add_inst(ret);
        env.block = None;
        env.func = None;
        Ok(())
    }
}

impl GenerateIr for Decl {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        match self {
            Decl::ConstDecl(const_decl) => const_decl.generate_on(env),
            Decl::VarDecl(var_decl) => var_decl.generate_on(env),
        }
    }
}

impl GenerateIr for Block {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        if self.block_items.is_empty() {
            return Ok(());
        }
        env.new_block(None, true);
        env.syb_table.enter_scope();

        for block_item in &self.block_items {
            match block_item {
                BlockItem::Stmt(stmt) => {
                    stmt.generate_on(env)?;
                    // end block if the block item is a
                    // return/break/continue statement
                    match stmt {
                        Stmt::Return(_) => {
                            break;
                        }
                        Stmt::Break(_) => {
                            break;
                        }
                        Stmt::Continue(_) => {
                            break;
                        }
                        _ => {}
                    }
                }
                BlockItem::Decl(decl) => decl.generate_on(env)?,
            };
        }
        env.syb_table.exit_scope();
        env.new_block(None, true);
        Ok(())
    }
}

impl GenerateIr for Stmt {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        match self {
            Stmt::Empty => Ok(()),
            Stmt::Exp(exp) => exp.generate_on(env).map(|_| ()),
            Stmt::Block(stmt) => stmt.generate_on(env),
            Stmt::Assign(stmt) => stmt.generate_on(env),
            Stmt::If(stmt) => stmt.generate_on(env),
            Stmt::While(stmt) => stmt.generate_on(env),
            Stmt::Return(stmt) => stmt.generate_on(env),
            Stmt::Break(stmt) => stmt.generate_on(env),
            Stmt::Continue(stmt) => stmt.generate_on(env),
        }
    }
}

impl GenerateIr for LVal {
    type IrTarget = Value;

    /// Returns the address of the left value
    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        match env.syb_table.lookup_item(&self.ident) {
            Some(item) => match item {
                SymbolItem::Var(symbol) => symbol.get_alloc(),
                SymbolItem::VarArray(symbol) => {
                    let symbol = symbol.clone();
                    let mut indexes = vec![];
                    for exp in &self.array_index {
                        indexes.push(exp.generate_on(env)?);
                    }
                    let addr = symbol.index(&indexes, env)?;
                    Ok(addr)
                }
                SymbolItem::FParamArray(symbol) => {
                    let symbol = symbol.clone();
                    let mut indexes = vec![];
                    for exp in &self.array_index {
                        indexes.push(exp.generate_on(env)?);
                    }
                    let addr = symbol.index(&indexes, env)?;
                    Ok(addr)
                }
                SymbolItem::Const(_) => panic!("Constant are not a left value!"),
                SymbolItem::ConstArray(symbol) => {
                    let symbol = symbol.clone();
                    let mut indexes = vec![];
                    for exp in &self.array_index {
                        indexes.push(exp.generate_on(env)?);
                    }
                    let addr = symbol.index(&indexes, env)?;
                    Ok(addr)
                }
                SymbolItem::ScopeSeparator(_) => unreachable!(),
            },
            None => Err(AstError::SymbolNotFoundError(self.ident.clone())),
        }
    }
}

impl GenerateIr for Return {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        let ret = match &self.exp {
            Some(exp) => Some(exp.generate_on(env)?),
            None => None,
        };
        let ret = env.local_value().ret(ret);
        env.add_inst(ret);
        Ok(())
    }
}

impl GenerateIr for Assign {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        let alloc = self.l_val.generate_on(env)?;
        let value = self.exp.generate_on(env)?;
        let store = env.local_value().store(value, alloc);
        env.add_inst(store);
        Ok(())
    }
}

impl GenerateIr for If {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        let cond = self.cond.generate_on(env)?;
        let base_bb = env.block.unwrap();

        let true_bb = env.new_block(Some("%then".into()), false);
        self.stmt.generate_on(env)?;
        // always create a new ending block for the then statement
        // even if it is not a block statement
        let true_end_bb = env.new_block(Some("%then_end".into()), true);

        if let Some(else_stmt) = &self.else_stmt {
            let false_bb = env.new_block(Some("%else".into()), false);
            else_stmt.generate_on(env)?;
            let end_bb = env.new_block(None, true);

            // branch to true/false
            let branch = env.local_value().branch(cond, true_bb, false_bb);
            // true jump to end
            let jump = env.local_value().jump(end_bb);
            add_inst!(env.func_data(), base_bb, branch);
            add_inst!(env.func_data(), true_end_bb, jump);
        } else {
            let end_bb = env.new_block(None, true);
            // branch to true/end
            let branch = env.local_value().branch(cond, true_bb, end_bb);
            add_inst!(env.func_data(), base_bb, branch);
        };

        Ok(())
    }
}

impl GenerateIr for While {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        let block = env.block.unwrap();
        let cond_bb = if env.in_entry() {
            // The entry basic block is not allowed to have predecessors
            // so create a new block for the condition
            let cond_bb = env.new_block(Some("%cond".into()), false);
            let jump = env.local_value().jump(cond_bb);
            add_inst!(env.func_data(), block, jump);
            cond_bb
        } else {
            env.new_block(Some("%cond".into()), true)
        };

        let cond = self.cond.generate_on(env)?;
        let cond_end_bb = env.block.unwrap();
        // use a temporary end block to serve for "break"
        let temp_end_bb = env.new_block(None, false);

        env.loop_stack.push(Loop {
            cond_bb,
            end_bb: temp_end_bb,
        });
        let body_bb = env.new_block(Some("%body".into()), false);
        self.stmt.generate_on(env)?;

        // jump back to the condition
        if !env.if_block_ended(&env.block.unwrap()) {
            let jump = env.local_value().jump(cond_bb);
            env.add_inst(jump);
        }

        let end_bb = env.new_block(None, true);
        // branch to body/end
        let branch = env.local_value().branch(cond, body_bb, end_bb);
        // temp end block
        let jump = env.local_value().jump(end_bb);

        add_inst!(env.func_data(), cond_end_bb, branch);
        add_inst!(env.func_data(), temp_end_bb, jump);

        env.loop_stack.pop();
        Ok(())
    }
}

impl GenerateIr for Break {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        let end_bb = env
            .loop_stack
            .last()
            .ok_or(AstError::LoopStackError("break".into()))?
            .end_bb;
        let jump = env.local_value().jump(end_bb);
        env.add_inst(jump);
        Ok(())
    }
}

impl GenerateIr for Continue {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        let cond_bb = env
            .loop_stack
            .last()
            .ok_or(AstError::LoopStackError("continue".into()))?
            .cond_bb;
        let jump = env.local_value().jump(cond_bb);
        env.add_inst(jump);
        Ok(())
    }
}

impl GenerateIr for ConstDecl {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        for const_def in self.const_defs.clone() {
            let item = env.syb_table.new_symbol(&const_def)?;
            let init = Some(Init::Const(const_def.const_init_val));
            match item {
                SymbolItem::Const(symbol) => {
                    symbol.clone().gen_value(self.b_type.into(), &init, env)?;
                }
                SymbolItem::ConstArray(symbol) => {
                    symbol.clone().gen_value(self.b_type.into(), &init, env)?;
                }
                _ => unreachable!(),
            }
        }
        Ok(())
    }
}

impl GenerateIr for VarDecl {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        for var_def in self.var_defs.clone() {
            let item = env.syb_table.new_symbol(&var_def)?;
            let init = var_def.init_val.map(|init| Init::Var(init));
            match item {
                SymbolItem::Var(symbol) => {
                    symbol.clone().gen_value(self.b_type.into(), &init, env)?;
                }
                SymbolItem::VarArray(symbol) => {
                    symbol.clone().gen_value(self.b_type.into(), &init, env)?;
                }
                _ => unreachable!(),
            }
        }
        Ok(())
    }
}

impl GenerateIr for LValAssign {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        let symbol = env
            .syb_table
            .lookup(&self.ident)
            .ok_or(AstError::SymbolNotFoundError(self.ident.clone()))?;

        if symbol.is_const() {
            return Err(AstError::AssignError(self.ident.clone()));
        }

        let l_val = LVal {
            ident: self.ident.clone(),
            array_index: self.array_index.clone(),
        };
        l_val.generate_on(env)
    }
}

impl GenerateIr for Exp {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        if let Ok(value) = self.eval(&env.syb_table) {
            return value.generate_on(env);
        }

        match self {
            Exp::LOrExp(l_or_exp) => l_or_exp.generate_on(env),
        }
    }
}

impl GenerateIr for ConstExp {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        // Const expressions must be evaluated at compile time
        self.eval(&env.syb_table)?.generate_on(env)
    }
}

impl GenerateIr for Cond {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        match self {
            Cond::LOrExp(exp) => exp.generate_on(env),
        }
    }
}

impl GenerateIr for LOrExp {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        match self {
            LOrExp::LAndExp(l_and_exp) => l_and_exp.generate_on(env),
            LOrExp::LOrOpExp(op_exp) => {
                // short circuit

                // base block
                let zero = env.local_value().integer(0);
                let lhs = op_exp.l_or_exp.generate_on(env)?;
                let l_binary = env.local_value().binary(BinaryOp::NotEq, lhs, zero);
                env.add_inst(l_binary);
                let result = env.alloc_and_store(l_binary, Type::get_i32());
                let base_bb = env.block.unwrap();

                // false block
                let false_bb = env.new_block(Some("%or_else".into()), false);
                let rhs = op_exp.l_and_exp.generate_on(env)?;
                let r_binary = env.local_value().binary(BinaryOp::NotEq, zero, rhs);
                let cover = env.local_value().store(r_binary, result);
                env.add_inst(r_binary);
                env.add_inst(cover);

                // end block
                let end_bb = env.new_block(None, true);
                let branch = env.local_value().branch(l_binary, end_bb, false_bb);
                add_inst!(env.func_data(), base_bb, branch);

                let result = env.local_value().load(result);
                env.add_inst(result);
                Ok(result)
            }
        }
    }
}

impl GenerateIr for LAndExp {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.generate_on(env),
            LAndExp::LAndOpExp(op_exp) => {
                // short circuit

                // base block
                let zero = env.local_value().integer(0);
                let lhs = op_exp.l_and_exp.generate_on(env)?;
                let l_binary = env.local_value().binary(BinaryOp::NotEq, lhs, zero);
                env.add_inst(l_binary);
                let result = env.alloc_and_store(l_binary, Type::get_i32());
                let base_bb = env.block.unwrap();

                // true block
                let true_bb = env.new_block(Some("%and_else".into()), false);
                let rhs = op_exp.eq_exp.generate_on(env)?;
                let r_binary = env.local_value().binary(BinaryOp::NotEq, rhs, zero);
                let cover = env.local_value().store(r_binary, result);
                env.add_inst(r_binary);
                env.add_inst(cover);

                // end block
                let end_bb = env.new_block(None, true);
                let branch = env.local_value().branch(l_binary, true_bb, end_bb);
                add_inst!(env.func_data(), base_bb, branch);

                let result = env.local_value().load(result);
                env.add_inst(result);
                Ok(result)
            }
        }
    }
}

impl GenerateIr for EqExp {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.generate_on(env),
            EqExp::EqOpExp(op_exp) => {
                let lhs = op_exp.eq_exp.generate_on(env)?;
                let rhs = op_exp.rel_exp.generate_on(env)?;
                let value = env.local_value().binary(op_exp.eq_op.into(), lhs, rhs);
                env.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr for RelExp {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        match self {
            RelExp::AddExp(add_exp) => add_exp.generate_on(env),
            RelExp::RelOpExp(op_exp) => {
                let lhs = op_exp.rel_exp.generate_on(env)?;
                let rhs = op_exp.add_exp.generate_on(env)?;
                let value = env.local_value().binary(op_exp.rel_op.into(), lhs, rhs);
                env.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr for AddExp {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        match self {
            AddExp::MulExp(mul) => mul.generate_on(env),
            AddExp::AddOpExp(op_exp) => {
                let lhs = op_exp.add_exp.generate_on(env)?;
                let rhs = op_exp.mul_exp.generate_on(env)?;
                let value = env.local_value().binary(op_exp.add_op.into(), lhs, rhs);
                env.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr for MulExp {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        match self {
            MulExp::UnaryExp(unary) => unary.generate_on(env),
            MulExp::MulOpExp(mul_op_exp) => {
                let lhs = mul_op_exp.mul_exp.generate_on(env)?;
                let rhs = mul_op_exp.unary_exp.generate_on(env)?;
                let value = env.local_value().binary(mul_op_exp.mul_op.into(), lhs, rhs);
                env.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr for UnaryExp {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.generate_on(env),

            UnaryExp::UnaryOpExp(unary_op_exp) => {
                let exp = unary_op_exp.unary_exp.generate_on(env)?;
                let zero = env.local_value().integer(0);
                let value = match unary_op_exp.unary_op {
                    UnaryOp::Pos => return Ok(exp),
                    UnaryOp::Neg => env.local_value().binary(BinaryOp::Sub, zero, exp),
                    UnaryOp::Not => env.local_value().binary(BinaryOp::Eq, exp, zero),
                };
                env.add_inst(value);
                Ok(value)
            }

            UnaryExp::FuncCall(func_call) => {
                let mut callee = None;
                for (&func, data) in env.program.funcs() {
                    if data.name() == global_ident(&func_call.ident).as_str() {
                        callee = Some(func);
                        break;
                    }
                }
                let callee =
                    callee.ok_or(AstError::FunctionNotFoundError(func_call.ident.clone()))?;

                let mut args = vec![];
                for exp in &func_call.args {
                    let arg = exp.generate_on(env)?;
                    args.push(arg);
                }

                let call = env.local_value().call(callee, args);
                env.add_inst(call);
                Ok(call)
            }
        }
    }
}

impl GenerateIr for PrimaryExp {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        match self {
            PrimaryExp::Number(number) => number.generate_on(env),
            PrimaryExp::LValExp(l_val) => l_val.generate_on(env),
            PrimaryExp::Exp(exp) => exp.generate_on(env),
        }
    }
}

impl GenerateIr for LValExp {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        match env
            .syb_table
            .lookup_item(&self.ident)
            .ok_or(AstError::SymbolNotFoundError(self.ident.clone()))?
        {
            item => match item {
                SymbolItem::Const(symbol) => symbol.value.clone().generate_on(env),
                _ => {
                    // if not a const, load the value
                    let lval = LVal {
                        ident: self.ident.clone(),
                        array_index: self.array_index.clone(),
                    };
                    let alloc = lval.generate_on(env)?;
                    let data = env.func_data().dfg().values().get(&alloc);
                    let kind = match data {
                        Some(data) => data.ty().kind().clone(),
                        None =>
                        env.program.borrow_values().get(&alloc).ok_or(AstError::UnknownError("The generated left value should have a data in the env, but it didn't.".into()))?.ty().kind().clone()
                    };
                    match kind {
                        TypeKind::Pointer(ty) => match ty.kind() {
                            TypeKind::Array(_, _) => {
                                let zero = env.local_value().integer(0);
                                let ptr = env.local_value().get_elem_ptr(alloc, zero);
                                env.add_inst(ptr);
                                Ok(ptr)
                            }
                            _ => {
                                let load = env.local_value().load(alloc);
                                env.add_inst(load);
                                Ok(load)
                            }
                        },
                        _ => unreachable!("LValExp::generate_on: not a pointer type"),
                    }
                }
            },
        }
    }
}

impl GenerateIr for Number {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        match self {
            Number::Int(int) => int.generate_on(env),
        }
    }
}

impl GenerateIr for i32 {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        let int = env.local_value().integer(*self);
        Ok(int)
    }
}
