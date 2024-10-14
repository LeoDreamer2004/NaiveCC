//! Generate IR from AST

use super::ast::*;
use super::eval::*;
use super::util::identifier_rename;
use super::util::IrIndentify;
use koopa::back::KoopaGenerator;
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value};
use std::io;

pub fn build_program(mut ast: CompUnit) -> Result<Program, ParseError> {
    let mut program = Program::new();
    let zero = program
        .new_value()
        .zero_init(Type::get_array(Type::get_i32(), 65536));
    let ptr = program.new_value().global_alloc(zero);
    program.set_value_name(ptr, Some("@data_arr".into()));

    let mut context = Context {
        program,
        func: None,
        block: None,
    };
    ast.generate_on(&mut context)?;
    Ok(context.program)
}

pub fn emit_ir(program: &mut Program, output: impl io::Write) -> Result<(), io::Error> {
    KoopaGenerator::new(output).generate_on(program)
}

/*********************  Structs  *********************/

/// Context for current generating
pub struct Context {
    pub program: Program,
    pub func: Option<Function>,
    pub block: Option<BasicBlock>,
}

impl Context {
    pub fn func(&mut self, func: Function) -> &mut Self {
        self.func = Some(func);
        self
    }

    pub fn block(&mut self, block: BasicBlock) -> &mut Self {
        self.block = Some(block);
        self
    }
}

#[derive(Debug)]
pub enum ParseError {
    FunctionNotFoundError(String),
    IllegalConstExpError(String),
    AssignError(String),
    UnknownError(String),
}

/*********************  Traits  *********************/

/// Trait which should be implemented by AST types to generate IR
///
/// Return a value of type `T` if needed
trait GenerateIr<T> {
    /// Generate IR on the given context
    ///
    /// # Errors
    /// Returns an [`ParseError`] if the generation fails
    fn generate_on(&mut self, context: &mut Context) -> Result<T, ParseError>;
}

/*********************  Macros  *********************/

/// Create a new [`BasicBlock`] in [`Function`]
macro_rules! new_bb {
    ($func:expr) => {
        $func.dfg_mut().new_bb()
    };
}

/// Create a new [`Value`] in [`Function`]
macro_rules! new_value {
    ($func:expr) => {
        $func.dfg_mut().new_value()
    };
}

/// Add a [`BasicBlock`] to the layout of [`Function`]
macro_rules! add_bb {
    ($func:expr, $bb:expr) => {
        $func.layout_mut().bbs_mut().push_key_back($bb).unwrap()
    };
}

/// Add an [`Value`] to a [`BasicBlock`] in [`Function`]
macro_rules! add_inst {
    ($func:expr, $bb:expr, $inst:expr) => {
        $func
            .layout_mut()
            .bb_mut($bb)
            .insts_mut()
            .push_key_back($inst)
            .unwrap()
    };
}

/// Get the current [`FunctionData`] in the [`Context`]
macro_rules! func_data {
    ($context:expr) => {
        $context.program.func_mut($context.func.unwrap())
    };
}

/*********************  Implementations  *********************/

impl GenerateIr<()> for CompUnit {
    fn generate_on(&mut self, context: &mut Context) -> Result<(), ParseError> {
        for comp_item in &mut self.comp_items {
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
    fn generate_on(&mut self, context: &mut Context) -> Result<(), ParseError> {
        let func = FunctionData::new_decl(
            self.global_ident(),
            self.params
                .iter()
                .map(|param| param.b_type.into())
                .collect(),
            self.func_type.into(),
        );
        let func = context.program.new_func(func);
        self.block.generate_on(context.func(func))?;
        Ok(())
    }
}

impl GenerateIr<()> for Decl {
    fn generate_on(&mut self, context: &mut Context) -> Result<(), ParseError> {
        match self {
            Decl::ConstDecl(const_decl) => const_decl.generate_on(context),
            Decl::VarDecl(var_decl) => var_decl.generate_on(context),
        }
    }
}

impl GenerateIr<()> for Block {
    fn generate_on(&mut self, context: &mut Context) -> Result<(), ParseError> {
        let func_data = func_data!(context);
        let entry = new_bb!(func_data).basic_block(Some("%entry".into()));
        add_bb!(func_data, entry);
        context.block(entry);
        for block_item in &mut self.block_items {
            block_item.generate_on(context)?;
        }
        Ok(())
    }
}

impl GenerateIr<()> for BlockItem {
    fn generate_on(&mut self, context: &mut Context) -> Result<(), ParseError> {
        match self {
            BlockItem::Stmt(stmt) => {
                stmt.generate_on(context)?;
            }
            BlockItem::Decl(decl) => {
                decl.generate_on(context)?;
            }
        }
        Ok(())
    }
}

impl GenerateIr<()> for Stmt {
    fn generate_on(&mut self, context: &mut Context) -> Result<(), ParseError> {
        match self {
            Stmt::Return(return_stmt) => {
                return_stmt.generate_on(context)?;
            }
            _ => todo!(),
        }
        Ok(())
    }
}

impl GenerateIr<()> for Return {
    fn generate_on(&mut self, context: &mut Context) -> Result<(), ParseError> {
        let ret = match &mut self.exp {
            Some(exp) => Some(exp.generate_on(context)?),
            None => None,
        };

        let func_data = func_data!(context);
        let ret = new_value!(func_data).ret(ret);
        add_inst!(func_data, context.block.unwrap(), ret);
        Ok(())
    }
}

impl GenerateIr<()> for ConstDecl {
    fn generate_on(&mut self, context: &mut Context) -> Result<(), ParseError> {
        for const_def in &mut self.const_defs {
            if const_def.array_size.is_empty() {
                let exp = &mut const_def.const_init_val;
                if let ConstInitVal::ConstExp(const_exp) = exp {
                    let value = const_exp.eval()?;
                    let value = match self.b_type {
                        BType::Int => context.program.new_value().integer(value),
                    };

                    if !(context.func.is_none()) {
                        let func_data = func_data!(context);
                        let block_data = func_data.dfg().bb(context.block.unwrap());
                        identifier_rename(&mut const_def.ident, func_data, block_data);
                    }

                    let value = context.program.new_value().global_alloc(value);
                    context
                        .program
                        .set_value_name(value, Some(const_def.global_ident()));
                } else {
                    return Err(ParseError::AssignError(
                        "Cannot assign array to a single value".into(),
                    ));
                }
            } else {
                // const int x[2][2] = {{1, 2}, {3, 4}};
                // recursive, maybe?
                todo!()
            }
        }
        Ok(())
    }
}

impl GenerateIr<()> for VarDecl {
    fn generate_on(&mut self, context: &mut Context) -> Result<(), ParseError> {
        let program = &context.program;
        if let Some(func) = &context.func {
            self.generate_on_local(context)
        } else {
            self.generate_on_global(context)
        }
    }
}

impl VarDecl {
    fn generate_on_global(&self, context: &mut Context) -> Result<(), ParseError> {
        todo!()
    }

    fn generate_on_local(&self, context: &mut Context) -> Result<(), ParseError> {
        todo!()
    }
}

impl GenerateIr<Value> for Exp {
    fn generate_on(&mut self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            Exp::LOrExp(l_or_exp) => l_or_exp.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for ConstExp {
    fn generate_on(&mut self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            ConstExp::LOrExp(l_or_exp) => l_or_exp.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for LOrExp {
    fn generate_on(&mut self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            LOrExp::LAndExp(l_and_exp) => l_and_exp.generate_on(context),
            LOrExp::LOrOpExp(op_exp) => {
                let lhs = op_exp.l_or_exp.generate_on(context)?;
                let rhs = op_exp.l_and_exp.generate_on(context)?;
                let func_data = func_data!(context);
                let value = new_value!(func_data).binary(BinaryOp::Or, lhs, rhs);
                add_inst!(func_data, context.block.unwrap(), value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for LAndExp {
    fn generate_on(&mut self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.generate_on(context),
            LAndExp::LAndOpExp(op_exp) => {
                let lhs = op_exp.l_and_exp.generate_on(context)?;
                let rhs = op_exp.eq_exp.generate_on(context)?;
                let func_data = func_data!(context);
                let value = new_value!(func_data).binary(BinaryOp::And, lhs, rhs);
                add_inst!(func_data, context.block.unwrap(), value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for EqExp {
    fn generate_on(&mut self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.generate_on(context),
            EqExp::EqOpExp(op_exp) => {
                let lhs = op_exp.eq_exp.generate_on(context)?;
                let rhs = op_exp.rel_exp.generate_on(context)?;
                let func_data = func_data!(context);
                let value = new_value!(func_data).binary(op_exp.eq_op.into(), lhs, rhs);
                add_inst!(func_data, context.block.unwrap(), value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for RelExp {
    fn generate_on(&mut self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            RelExp::AddExp(add_exp) => add_exp.generate_on(context),
            RelExp::RelOpExp(op_exp) => {
                let lhs = op_exp.rel_exp.generate_on(context)?;
                let rhs = op_exp.add_exp.generate_on(context)?;
                let func_data = func_data!(context);
                let value = new_value!(func_data).binary(op_exp.rel_op.into(), lhs, rhs);
                add_inst!(func_data, context.block.unwrap(), value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for AddExp {
    fn generate_on(&mut self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            AddExp::MulExp(mul) => mul.generate_on(context),
            AddExp::AddOpExp(op_exp) => {
                let lhs = op_exp.add_exp.generate_on(context)?;
                let rhs = op_exp.mul_exp.generate_on(context)?;
                let func_data = func_data!(context);
                let value = new_value!(func_data).binary(op_exp.add_op.into(), lhs, rhs);
                add_inst!(func_data, context.block.unwrap(), value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for MulExp {
    fn generate_on(&mut self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            MulExp::UnaryExp(unary) => unary.generate_on(context),
            MulExp::MulOpExp(mul_op_exp) => {
                let lhs = mul_op_exp.mul_exp.generate_on(context)?;
                let rhs = mul_op_exp.unary_exp.generate_on(context)?;
                let func_data = func_data!(context);
                let value = new_value!(func_data).binary(mul_op_exp.mul_op.into(), lhs, rhs);
                add_inst!(func_data, context.block.unwrap(), value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for UnaryExp {
    fn generate_on(&mut self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.generate_on(context),

            UnaryExp::UnaryOpExp(unary_op_exp) => {
                let exp = unary_op_exp.unary_exp.generate_on(context)?;
                let func_data = func_data!(context);
                let zero = new_value!(func_data).integer(0);
                let value = match unary_op_exp.unary_op {
                    UnaryOp::Pos => return Ok(exp),
                    UnaryOp::Neg => new_value!(func_data).binary(BinaryOp::Sub, zero, exp),
                    UnaryOp::Not => new_value!(func_data).binary(BinaryOp::NotEq, exp, zero),
                };
                add_inst!(func_data, context.block.unwrap(), value);
                Ok(value)
            }

            UnaryExp::FuncCall(func_call) => {
                let mut callee = None;
                let mut args = vec![];
                for arg in &mut func_call.args {
                    args.push(arg.generate_on(context)?);
                }
                for (&func, data) in context.program.funcs() {
                    if data.name() == func_call.global_ident().as_str() {
                        callee = Some(func);
                        break;
                    }
                }
                if callee.is_none() {
                    return Err(ParseError::FunctionNotFoundError(func_call.ident.clone()));
                }

                let func_data = func_data!(context);
                let call = new_value!(func_data).call(callee.unwrap(), args);
                add_inst!(func_data, context.block.unwrap(), call);
                Ok(call)
            }
        }
    }
}

impl GenerateIr<Value> for PrimaryExp {
    fn generate_on(&mut self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            PrimaryExp::Number(number) => match number {
                Number::Int(int) => {
                    let func_data = func_data!(context);
                    let int = new_value!(func_data).integer(*int);
                    Ok(int)
                }
            },
            PrimaryExp::LVal(l_val) => l_val.generate_on(context),
            PrimaryExp::Exp(exp) => exp.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for LVal {
    fn generate_on(&mut self, context: &mut Context) -> Result<Value, ParseError> {
        todo!()
    }
}
