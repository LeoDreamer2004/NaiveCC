//! Generate IR from AST

use super::ast::*;
use super::Error;
use crate::{add_bb, add_inst, new_bb, new_value};
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, Function, FunctionData, Program, Type, Value};

pub fn build_program(ast: CompUnit) -> Result<Program, Error> {
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

/// Helper macro to get the current function data
macro_rules! func_data {
    ($context:expr) => {
        $context.program.func_mut($context.func.unwrap())
    };
}

impl CompUnit {
    pub fn generate_on(&self, context: &mut Context) -> Result<(), Error> {
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

impl FuncDef {
    pub fn generate_on(&self, context: &mut Context) -> Result<(), Error> {
        let func = FunctionData::new_decl(
            format!("@{}", self.ident.clone()),
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

impl Decl {
    pub fn generate_on(&self, context: &mut Context) -> Result<(), Error> {
        match self {
            Decl::ConstDecl(const_decl) => const_decl.generate_on(context),
            Decl::VarDecl(var_decl) => var_decl.generate_on(context),
        }
    }
}

impl Block {
    pub fn generate_on(&self, context: &mut Context) -> Result<(), Error> {
        let func_data = func_data!(context);
        let entry = new_bb!(func_data).basic_block(Some("%entry".into()));
        add_bb!(func_data, entry);
        context.block(entry);
        for block_item in &self.block_items {
            block_item.generate_on(context)?;
        }
        Ok(())
    }
}

impl BlockItem {
    pub fn generate_on(&self, context: &mut Context) -> Result<(), Error> {
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

impl Stmt {
    pub fn generate_on(&self, context: &mut Context) -> Result<(), Error> {
        match self {
            Stmt::Return(return_stmt) => {
                return_stmt.generate_on(context)?;
            }
            _ => todo!(),
        }
        Ok(())
    }
}

impl Return {
    pub fn generate_on(&self, context: &mut Context) -> Result<(), Error> {
        let ret = match &self.exp {
            Some(exp) => Some(exp.generate_on(context)?),
            None => None,
        };

        let func_data = func_data!(context);
        let ret = new_value!(func_data).ret(ret);
        add_inst!(func_data, context.block.unwrap(), ret);
        Ok(())
    }
}

impl ConstDecl {
    pub fn generate_on(&self, context: &mut Context) -> Result<(), Error> {
        let program = &mut context.program;
        if let Some(func) = &context.func {
            self.generate_on_local(program, func)
        } else {
            self.generate_on_global(program)
        }
    }

    fn generate_on_global(&self, program: &mut Program) -> Result<(), Error> {
        for const_def in &self.const_defs {}
        todo!()
    }

    fn generate_on_local(&self, program: &mut Program, func: &Function) -> Result<(), Error> {
        todo!()
    }
}

impl VarDecl {
    pub fn generate_on(&self, context: &mut Context) -> Result<(), Error> {
        let program = &context.program;
        if let Some(func) = &context.func {
            self.generate_on_local(program, func)
        } else {
            self.generate_on_global(program)
        }
    }

    fn generate_on_global(&self, program: &Program) -> Result<(), Error> {
        todo!()
    }

    fn generate_on_local(&self, program: &Program, func: &Function) -> Result<(), Error> {
        todo!()
    }
}

impl Exp {
    pub fn generate_on(&self, context: &mut Context) -> Result<Value, Error> {
        match self {
            Exp::AddExp(add_exp) => add_exp.generate_on(context),
        }
    }
}

impl AddExp {
    pub fn generate_on(&self, context: &mut Context) -> Result<Value, Error> {
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

impl MulExp {
    pub fn generate_on(&self, context: &mut Context) -> Result<Value, Error> {
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

impl UnaryExp {
    pub fn generate_on(&self, context: &mut Context) -> Result<Value, Error> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.generate_on(context),

            UnaryExp::UnaryOpExp(unary_op_exp) => {
                todo!()
            }

            UnaryExp::FuncCall(func_call) => {
                let mut callee = None;
                let mut args = vec![];
                for arg in &func_call.args {
                    args.push(arg.generate_on(context)?);
                }
                for (&func, data) in context.program.funcs() {
                    if data.name() == format!("@{}", func_call.ident).as_str() {
                        callee = Some(func);
                        break;
                    }
                }
                if callee.is_none() {
                    return Err(Error::Parse(format!(
                        "Function {} not found",
                        func_call.ident
                    )));
                }

                let func_data = func_data!(context);
                let call = new_value!(func_data).call(callee.unwrap(), args);
                add_inst!(func_data, context.block.unwrap(), call);
                Ok(call)
            }
        }
    }
}

impl PrimaryExp {
    pub fn generate_on(&self, context: &mut Context) -> Result<Value, Error> {
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

impl LVal {
    pub fn generate_on(&self, context: &mut Context) -> Result<Value, Error> {
        todo!()
    }
}
