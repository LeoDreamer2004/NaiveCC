//! Generate IR from AST

use super::ast::*;
use super::builtin::set_up_builtins;
use super::context::Context;
use super::eval::Eval;
use super::loops::*;
use super::symbol::*;
use crate::utils::namer::{global_ident, normal_ident};
use koopa::back::KoopaGenerator;
use koopa::ir::builder::LocalInstBuilder;
use koopa::ir::builder::ValueBuilder;
use koopa::ir::Type;
use koopa::ir::{BinaryOp, FunctionData, Program, Value};
use std::io;

pub fn build_ir(ast: CompUnit) -> Result<Program, AstError> {
    let mut context = Context::default();
    ast.generate_on(&mut context)?;
    Ok(context.program)
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
pub trait GenerateIr<T> {
    /// Generate IR on the given context
    ///
    /// # Errors
    /// Returns an [`ParseError`] if the generation fails
    fn generate_on(&self, context: &mut Context) -> Result<T, AstError>;
}

impl GenerateIr<()> for CompUnit {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        set_up_builtins(context);

        for comp_item in &self.comp_items {
            match comp_item {
                CompItem::FuncDef(func_def) => {
                    func_def.generate_on(context)?;
                }
                CompItem::Decl(decl) => {
                    decl.generate_on(context)?;
                }
            }
        }
        Ok(())
    }
}

impl GenerateIr<()> for FuncDef {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let func = FunctionData::with_param_names(
            global_ident(&self.ident),
            self.params
                .iter()
                .map(|param| (Some(global_ident(&param.ident)), param.b_type.into()))
                .collect(),
            self.func_type.into(),
        );

        let func = context.program.new_func(func);
        context.func(func);
        context.create_block(Some("%entry".into()));

        context.syb_table.enter_scope();

        // Add parameters to the symbol table
        for (i, param) in (&self.params).into_iter().enumerate() {
            context.syb_table.new_symbol(param)?;
            let value = context.func_data().params()[i];
            let alloc = context.alloc_and_store(value, param.b_type.into());
            context.set_value_name(alloc, normal_ident(&param.ident));
            context.syb_table.set_alloc(&param.get_ident(), alloc)?;
        }

        self.block.generate_on(context)?;
        context.syb_table.exit_scope();

        let ret = context.new_value().ret(None);
        context.add_inst(ret);
        context.block = None;
        context.func = None;
        Ok(())
    }
}

impl GenerateIr<()> for Decl {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        match self {
            Decl::ConstDecl(const_decl) => const_decl.generate_on(context),
            Decl::VarDecl(var_decl) => var_decl.generate_on(context),
        }
    }
}

impl GenerateIr<()> for Block {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        if self.block_items.is_empty() {
            return Ok(());
        }

        context.new_block(None, true);
        context.syb_table.enter_scope();

        for block_item in &self.block_items {
            match block_item {
                BlockItem::Stmt(stmt) => {
                    stmt.generate_on(context)?;
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
                BlockItem::Decl(decl) => decl.generate_on(context)?,
            };
        }
        context.syb_table.exit_scope();
        context.new_block(None, true);
        Ok(())
    }
}

impl GenerateIr<()> for Stmt {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        match self {
            Stmt::Empty => Ok(()),
            Stmt::Exp(exp) => exp.generate_on(context).map(|_| ()),
            Stmt::Block(stmt) => stmt.generate_on(context),
            Stmt::Assign(stmt) => stmt.generate_on(context),
            Stmt::If(stmt) => stmt.generate_on(context),
            Stmt::While(stmt) => stmt.generate_on(context),
            Stmt::Return(stmt) => stmt.generate_on(context),
            Stmt::Break(stmt) => stmt.generate_on(context),
            Stmt::Continue(stmt) => stmt.generate_on(context),
        }
    }
}

impl GenerateIr<()> for Return {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let ret = match &self.exp {
            Some(exp) => Some(exp.generate_on(context)?),
            None => None,
        };
        let ret = context.new_value().ret(ret);
        context.add_inst(ret);
        Ok(())
    }
}

impl GenerateIr<()> for Assign {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let alloc = self.l_val.generate_on(context)?;
        let value = self.exp.generate_on(context)?;
        let store = context.new_value().store(value, alloc);
        context.add_inst(store);
        Ok(())
    }
}

impl GenerateIr<()> for If {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let cond = self.cond.generate_on(context)?;
        let base_bb = context.block.unwrap();

        let true_bb = context.new_block(Some("%then".into()), false);
        self.stmt.generate_on(context)?;
        // always create a new ending block for the then statement
        // even if it is not a block statement
        let true_end_bb = context.new_block(Some("%then_end".into()), true);

        if let Some(else_stmt) = &self.else_stmt {
            let false_bb = context.new_block(Some("%else".into()), false);
            else_stmt.generate_on(context)?;
            let end_bb = context.new_block(None, true);

            // branch to true/false
            let branch = context.new_value().branch(cond, true_bb, false_bb);
            // true jump to end
            let jump = context.new_value().jump(end_bb);
            add_inst!(context.func_data(), base_bb, branch);
            add_inst!(context.func_data(), true_end_bb, jump);
        } else {
            let end_bb = context.new_block(None, true);
            // branch to true/end
            let branch = context.new_value().branch(cond, true_bb, end_bb);
            add_inst!(context.func_data(), base_bb, branch);
        };

        Ok(())
    }
}

impl GenerateIr<()> for While {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let block = context.block.unwrap();
        let cond_bb = if context.in_entry() {
            // The entry basic block is not allowed to have predecessors
            // so create a new block for the condition
            let cond_bb = context.new_block(Some("%cond".into()), false);
            let jump = context.new_value().jump(cond_bb);
            add_inst!(context.func_data(), block, jump);
            cond_bb
        } else {
            context.new_block(Some("%cond".into()), true)
        };

        let cond = self.cond.generate_on(context)?;
        // use a temporary end block to serve for "break"
        let temp_end_bb = context.new_block(None, false);

        context.loop_stack.push(Loop {
            cond_bb,
            end_bb: temp_end_bb,
        });
        let body_bb = context.new_block(Some("%body".into()), false);
        self.stmt.generate_on(context)?;

        // jump back to the condition
        if !context.if_block_ended(&context.block.unwrap()) {
            let jump = context.new_value().jump(cond_bb);
            context.add_inst(jump);
        }

        let end_bb = context.new_block(None, true);
        // branch to body/end
        let branch = context.new_value().branch(cond, body_bb, end_bb);
        // temp end block
        let jump = context.new_value().jump(end_bb);

        add_inst!(context.func_data(), cond_bb, branch);
        add_inst!(context.func_data(), temp_end_bb, jump);

        context.loop_stack.pop();
        Ok(())
    }
}

impl GenerateIr<()> for Break {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let end_bb = context
            .loop_stack
            .last()
            .ok_or(AstError::LoopStackError("break".into()))?
            .end_bb;
        let jump = context.new_value().jump(end_bb);
        context.add_inst(jump);
        Ok(())
    }
}

impl GenerateIr<()> for Continue {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let cond_bb = context
            .loop_stack
            .last()
            .ok_or(AstError::LoopStackError("continue".into()))?
            .cond_bb;
        let jump = context.new_value().jump(cond_bb);
        context.add_inst(jump);
        Ok(())
    }
}

impl GenerateIr<()> for ConstDecl {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        for const_def in self.const_defs.clone() {
            let item = context.syb_table.new_symbol(&const_def)?;
            let init = Some(Init::Const(const_def.const_init_val));
            match item {
                SymbolItem::Const(symbol) => {
                    symbol
                        .clone()
                        .gen_value(self.b_type.into(), &init, context)?;
                }
                SymbolItem::ConstArray(symbol) => {
                    symbol
                        .clone()
                        .gen_value(self.b_type.into(), &init, context)?;
                }
                _ => unreachable!(),
            }
        }
        Ok(())
    }
}

impl GenerateIr<()> for VarDecl {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        for var_def in self.var_defs.clone() {
            let item = context.syb_table.new_symbol(&var_def)?;
            let init = var_def.init_val.map(|init| Init::Var(init));
            match item {
                SymbolItem::Var(symbol) => {
                    symbol
                        .clone()
                        .gen_value(self.b_type.into(), &init, context)?;
                }
                SymbolItem::VarArray(symbol) => {
                    symbol
                        .clone()
                        .gen_value(self.b_type.into(), &init, context)?;
                }
                _ => unreachable!(),
            }
        }
        Ok(())
    }
}

impl GenerateIr<Value> for LVal {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match context.syb_table.lookup_item(&self.ident) {
            Some(item) => match &item {
                SymbolItem::Var(symbol) => symbol.get_alloc(),
                SymbolItem::VarArray(symbol) => {
                    let symbol = symbol.clone();
                    let mut indexes = vec![];
                    for exp in &self.array_index {
                        indexes.push(exp.generate_on(context)?);
                    }
                    let addr = symbol.index(&indexes, context)?;
                    Ok(addr)
                }
                _ => unreachable!("Constants are not allowed to be assigned!"),
            },
            None => Err(AstError::SymbolNotFoundError(self.ident.clone())),
        }
    }
}

impl GenerateIr<Value> for Exp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        if let Ok(value) = self.eval(&context.syb_table) {
            return value.generate_on(context);
        }

        match self {
            Exp::LOrExp(l_or_exp) => l_or_exp.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for ConstExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        // Const expressions must be evaluated at compile time
        self.eval(&context.syb_table)?.generate_on(context)
    }
}

impl GenerateIr<Value> for Cond {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            Cond::LOrExp(exp) => exp.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for LOrExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            LOrExp::LAndExp(l_and_exp) => l_and_exp.generate_on(context),
            LOrExp::LOrOpExp(op_exp) => {
                // short circuit

                // base block
                let zero = context.new_value().integer(0);
                let lhs = op_exp.l_or_exp.generate_on(context)?;
                let l_binary = context.new_value().binary(BinaryOp::NotEq, lhs, zero);
                context.add_inst(l_binary);
                let result = context.alloc_and_store(l_binary, Type::get_i32());
                let base_bb = context.block.unwrap();

                // false block
                let false_bb = context.new_block(Some("%or_else".into()), false);
                let rhs = op_exp.l_and_exp.generate_on(context)?;
                let r_binary = context.new_value().binary(BinaryOp::NotEq, zero, rhs);
                let cover = context.new_value().store(r_binary, result);
                context.add_inst(r_binary);
                context.add_inst(cover);

                // end block
                let end_bb = context.new_block(None, true);
                let branch = context.new_value().branch(l_binary, end_bb, false_bb);
                add_inst!(context.func_data(), base_bb, branch);

                let result = context.new_value().load(result);
                context.add_inst(result);
                Ok(result)
            }
        }
    }
}

impl GenerateIr<Value> for LAndExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.generate_on(context),
            LAndExp::LAndOpExp(op_exp) => {
                // short circuit

                // base block
                let zero = context.new_value().integer(0);
                let lhs = op_exp.l_and_exp.generate_on(context)?;
                let l_binary = context.new_value().binary(BinaryOp::NotEq, lhs, zero);
                context.add_inst(l_binary);
                let result = context.alloc_and_store(l_binary, Type::get_i32());
                let base_bb = context.block.unwrap();

                // true block
                let true_bb = context.new_block(Some("%and_else".into()), false);
                let rhs = op_exp.eq_exp.generate_on(context)?;
                let r_binary = context.new_value().binary(BinaryOp::NotEq, rhs, zero);
                let cover = context.new_value().store(r_binary, result);
                context.add_inst(r_binary);
                context.add_inst(cover);

                // end block
                let end_bb = context.new_block(None, true);
                let branch = context.new_value().branch(l_binary, true_bb, end_bb);
                add_inst!(context.func_data(), base_bb, branch);

                let result = context.new_value().load(result);
                context.add_inst(result);
                Ok(result)
            }
        }
    }
}

impl GenerateIr<Value> for EqExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.generate_on(context),
            EqExp::EqOpExp(op_exp) => {
                let lhs = op_exp.eq_exp.generate_on(context)?;
                let rhs = op_exp.rel_exp.generate_on(context)?;
                let value = context.new_value().binary(op_exp.eq_op.into(), lhs, rhs);
                context.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for RelExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            RelExp::AddExp(add_exp) => add_exp.generate_on(context),
            RelExp::RelOpExp(op_exp) => {
                let lhs = op_exp.rel_exp.generate_on(context)?;
                let rhs = op_exp.add_exp.generate_on(context)?;
                let value = context.new_value().binary(op_exp.rel_op.into(), lhs, rhs);
                context.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for AddExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            AddExp::MulExp(mul) => mul.generate_on(context),
            AddExp::AddOpExp(op_exp) => {
                let lhs = op_exp.add_exp.generate_on(context)?;
                let rhs = op_exp.mul_exp.generate_on(context)?;
                let value = context.new_value().binary(op_exp.add_op.into(), lhs, rhs);
                context.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for MulExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            MulExp::UnaryExp(unary) => unary.generate_on(context),
            MulExp::MulOpExp(mul_op_exp) => {
                let lhs = mul_op_exp.mul_exp.generate_on(context)?;
                let rhs = mul_op_exp.unary_exp.generate_on(context)?;
                let value = context
                    .new_value()
                    .binary(mul_op_exp.mul_op.into(), lhs, rhs);
                context.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for UnaryExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.generate_on(context),

            UnaryExp::UnaryOpExp(unary_op_exp) => {
                let exp = unary_op_exp.unary_exp.generate_on(context)?;
                let zero = context.new_value().integer(0);
                let value = match unary_op_exp.unary_op {
                    UnaryOp::Pos => return Ok(exp),
                    UnaryOp::Neg => context.new_value().binary(BinaryOp::Sub, zero, exp),
                    UnaryOp::Not => context.new_value().binary(BinaryOp::Eq, exp, zero),
                };
                context.add_inst(value);
                Ok(value)
            }

            UnaryExp::FuncCall(func_call) => {
                let mut callee = None;
                let mut args = vec![];
                for arg in &func_call.args {
                    args.push(arg.generate_on(context)?);
                }
                for (&func, data) in context.program.funcs() {
                    if data.name() == global_ident(&func_call.ident).as_str() {
                        callee = Some(func);
                        break;
                    }
                }
                if callee.is_none() {
                    return Err(AstError::FunctionNotFoundError(func_call.ident.clone()));
                }

                let call = context.new_value().call(callee.unwrap(), args);
                context.add_inst(call);
                Ok(call)
            }
        }
    }
}

impl GenerateIr<Value> for PrimaryExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            PrimaryExp::Number(number) => number.generate_on(context),
            PrimaryExp::LValExp(l_val) => l_val.generate_on(context),
            PrimaryExp::Exp(exp) => exp.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for LValExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match context.syb_table.lookup_item(&self.ident) {
            Some(item) => match &item {
                SymbolItem::Const(symbol) => symbol.value.clone().generate_on(context),
                SymbolItem::ConstArray(symbol) => {
                    let symbol = symbol.clone();
                    let mut indexes = vec![];
                    for exp in &self.array_index {
                        indexes.push(exp.generate_on(context)?);
                    }
                    let addr = symbol.index(&indexes, context)?;
                    let load = context.new_value().load(addr);
                    context.add_inst(load);
                    Ok(load)
                }
                _ => {
                    let lval = LVal {
                        ident: self.ident.clone(),
                        array_index: self.array_index.clone(),
                    };
                    let alloc = lval.generate_on(context)?;
                    let load = context.new_value().load(alloc);
                    context.add_inst(load);
                    Ok(load)
                }
            },
            None => Err(AstError::SymbolNotFoundError(self.ident.clone())),
        }
    }
}

impl GenerateIr<Value> for Number {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            Number::Int(int) => int.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for i32 {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        let int = context.new_value().integer(*self);
        Ok(int)
    }
}
