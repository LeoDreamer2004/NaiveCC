//! Transform some basic AST types to Koopa

use super::ast::*;
use koopa::ir::{BinaryOp, Type};

impl Into<Type> for BType {
    fn into(self) -> Type {
        match self {
            BType::Int => Type::get_i32(),
        }
    }
}

impl Into<Type> for FuncType {
    fn into(self) -> Type {
        match self {
            FuncType::Void => Type::get_unit(),
            FuncType::BType(b_type) => b_type.into(),
        }
    }
}

impl Into<BinaryOp> for AddOp {
    fn into(self) -> BinaryOp {
        match self {
            AddOp::Add => BinaryOp::Add,
            AddOp::Sub => BinaryOp::Sub,
        }
    }
}

impl Into<BinaryOp> for MulOp {
    fn into(self) -> BinaryOp {
        match self {
            MulOp::Mul => BinaryOp::Mul,
            MulOp::Div => BinaryOp::Div,
            MulOp::Mod => BinaryOp::Mod,
        }
    }
}
