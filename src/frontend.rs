use koopa::back::KoopaGenerator;
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, Function, FunctionData, Program, Type, Value};
use std::io;

use crate::ast::*;

macro_rules! new_bb {
    ($func:expr) => {
        $func.dfg_mut().new_bb()
    };
}

macro_rules! new_value {
    ($func:expr) => {
        $func.dfg_mut().new_value()
    };
}

macro_rules! add_bb {
    ($func:expr, $bb:expr) => {
        $func.layout_mut().bbs_mut().push_key_back($bb).unwrap()
    };
}

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

#[derive(Debug)]
pub enum Error {
    InvalidArgs,
    InvalidFile(io::Error),
    Parse,
    Io(io::Error),
}

pub fn emit_ir(program: &mut Program, output: impl io::Write) -> Result<(), Error> {
    KoopaGenerator::new(output)
        .generate_on(program)
        .map_err(Error::Io)
}

pub fn build_program(ast: CompUnit) -> Result<Program, Error> {
    let mut program = Program::new();
    // create data array
    let zero = program
        .new_value()
        .zero_init(Type::get_array(Type::get_i32(), 65536));
    let ptr = program.new_value().global_alloc(zero);
    program.set_value_name(ptr, Some("@data_arr".into()));

    // create function declarations
    // let putchar = FunctionData::new_decl("@putchar".into(), vec![Type::get_i32()], Type::get_i32());
    // let putchar = program.new_func(putchar);
    // let getchar = FunctionData::new_decl("@getchar".into(), Vec::new(), Type::get_i32());
    // let getchar = program.new_func(getchar);

    ast.generate_on(&mut program)?;
    Ok(program)
}

impl BType {
    fn to_type(&self) -> Type {
        match self {
            BType::Int => Type::get_i32(),
        }
    }
}

impl FuncType {
    fn to_type(&self) -> Type {
        match self {
            FuncType::Void => Type::get_unit(),
            FuncType::BType(b_type) => b_type.to_type(),
        }
    }
}

impl CompUnit {
    fn generate_on(&self, program: &mut Program) -> Result<(), Error> {
        for comp_item in &self.comp_items {
            match comp_item {
                CompItem::FuncDef(func_def) => {
                    func_def.generate_on(program)?;
                }
                CompItem::Decl(decl) => {
                    decl.generate_on(program)?;
                }
            }
        }
        Ok(())
    }
}

impl FuncDef {
    fn generate_on(&self, program: &mut Program) -> Result<(), Error> {
        let func = FunctionData::new_decl(
            format!("@{}", self.ident.clone()),
            self.params
                .iter()
                .map(|param| param.b_type.to_type())
                .collect(),
            self.func_type.to_type(),
        );
        let func = program.new_func(func);
        self.block.generate_on(program, func)?;
        Ok(())
    }
}

impl Decl {
    fn generate_on(&self, program: &mut Program) -> Result<(), Error> {
        todo!()
    }
}

impl Block {
    fn generate_on(&self, program: &mut Program, func: Function) -> Result<(), Error> {
        let func_data = program.func_mut(func);
        let entry = new_bb!(func_data).basic_block(Some("%entry".into()));
        add_bb!(func_data, entry);
        for block_item in &self.block_items {
            block_item.generate_on(program, &func, &entry)?;
        }
        Ok(())
    }
}

impl BlockItem {
    fn generate_on(
        &self,
        program: &mut Program,
        func: &Function,
        block: &BasicBlock,
    ) -> Result<(), Error> {
        match self {
            BlockItem::Stmt(stmt) => {
                stmt.generate_on(program, func, block)?;
            }
            BlockItem::Decl(decl) => {
                decl.generate_on(program)?;
            }
        }
        Ok(())
    }
}

impl Stmt {
    fn generate_on(
        &self,
        program: &mut Program,
        func: &Function,
        block: &BasicBlock,
    ) -> Result<(), Error> {
        match self {
            Stmt::Return(return_stmt) => {
                return_stmt.generate_on(program, func, block)?;
            }
            _ => todo!(),
        }
        Ok(())
    }
}

impl Return {
    fn generate_on(
        &self,
        program: &mut Program,
        func: &Function,
        block: &BasicBlock,
    ) -> Result<(), Error> {
        let ret = match &self.exp {
            Some(exp) => Some(exp.generate_on(program, func, block)?),
            None => None,
        };

        let func_data = program.func_mut(*func);
        let ret = new_value!(func_data).ret(ret);
        add_inst!(func_data, *block, ret);
        Ok(())
    }
}

impl Exp {
    fn generate_on(
        &self,
        program: &mut Program,
        func: &Function,
        block: &BasicBlock,
    ) -> Result<Value, Error> {
        match self {
            Exp::AddExp(add_exp) => add_exp.generate_on(program, func, block),
        }
    }
}

impl AddExp {
    fn generate_on(
        &self,
        program: &mut Program,
        func: &Function,
        block: &BasicBlock,
    ) -> Result<Value, Error> {
        match self {
            AddExp::MulExp(mul) => mul.generate_on(program, func, block),
            AddExp::AddOpExp(add_op_exp) => {
                todo!()
            }
        }
    }
}

impl MulExp {
    fn generate_on(
        &self,
        program: &mut Program,
        func: &Function,
        block: &BasicBlock,
    ) -> Result<Value, Error> {
        match self {
            MulExp::UnaryExp(unary) => unary.generate_on(program, func, block),
            MulExp::MulOpExp(mul_op_exp) => {
                todo!()
            }
        }
    }
}

impl UnaryExp {
    fn generate_on(
        &self,
        program: &mut Program,
        func: &Function,
        block: &BasicBlock,
    ) -> Result<Value, Error> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.generate_on(program, func, block),
            _ => {
                todo!()
            }
        }
    }
}

impl PrimaryExp {
    fn generate_on(
        &self,
        program: &mut Program,
        func: &Function,
        block: &BasicBlock,
    ) -> Result<Value, Error> {
        match self {
            PrimaryExp::LVal(l_val) => {
                todo!()
            }
            PrimaryExp::Number(number) => match number {
                Number::Int(int) => {
                    let func_data = program.func_mut(*func);
                    let int = new_value!(func_data).integer(*int);
                    Ok(int)
                }
            },
            PrimaryExp::Exp(exp) => {
                todo!()
            }
        }
    }
}
