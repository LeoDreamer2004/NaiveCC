use super::ast::*;
use super::ParseError;

pub trait Eval<T> {
    fn eval(&self) -> Result<T, ParseError>;
}

impl Eval<i32> for Exp {
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            Exp::LOrExp(exp) => exp.eval(),
        }
    }
}

impl Eval<i32> for ConstExp {
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            ConstExp::LOrExp(exp) => exp.eval(),
        }
    }
}

impl Eval<i32> for LOrExp {
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            LOrExp::LAndExp(exp) => exp.eval(),
            LOrExp::LOrOpExp(op_exp) => {
                let lhs = op_exp.l_or_exp.eval()?;
                let rhs = op_exp.l_and_exp.eval()?;
                Ok(if lhs != 0 || rhs != 0 { 1 } else { 0 })
            }
        }
    }
}

impl Eval<i32> for LAndExp {
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            LAndExp::EqExp(exp) => exp.eval(),
            LAndExp::LAndOpExp(op_exp) => {
                let lhs = op_exp.l_and_exp.eval()?;
                let rhs = op_exp.eq_exp.eval()?;
                Ok(if lhs != 0 && rhs != 0 { 1 } else { 0 })
            }
        }
    }
}

impl Eval<i32> for EqExp {
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            EqExp::RelExp(exp) => exp.eval(),
            EqExp::EqOpExp(op_exp) => {
                let lhs = op_exp.eq_exp.eval()?;
                let rhs = op_exp.rel_exp.eval()?;
                match op_exp.eq_op {
                    EqOp::Eq => Ok(if lhs == rhs { 1 } else { 0 }),
                    EqOp::Ne => Ok(if lhs != rhs { 1 } else { 0 }),
                }
            }
        }
    }
}

impl Eval<i32> for RelExp {
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            RelExp::AddExp(exp) => exp.eval(),
            RelExp::RelOpExp(op_exp) => {
                let lhs = op_exp.rel_exp.eval()?;
                let rhs = op_exp.add_exp.eval()?;
                match op_exp.rel_op {
                    RelOp::Lt => Ok(if lhs < rhs { 1 } else { 0 }),
                    RelOp::Le => Ok(if lhs <= rhs { 1 } else { 0 }),
                    RelOp::Gt => Ok(if lhs > rhs { 1 } else { 0 }),
                    RelOp::Ge => Ok(if lhs >= rhs { 1 } else { 0 }),
                }
            }
        }
    }
}

impl Eval<i32> for AddExp {
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            AddExp::MulExp(exp) => exp.eval(),
            AddExp::AddOpExp(op_exp) => {
                let lhs = op_exp.add_exp.eval()?;
                let rhs = op_exp.mul_exp.eval()?;
                match op_exp.add_op {
                    AddOp::Add => Ok(lhs + rhs),
                    AddOp::Sub => Ok(lhs - rhs),
                }
            }
        }
    }
}

impl Eval<i32> for MulExp {
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            MulExp::UnaryExp(exp) => exp.eval(),
            MulExp::MulOpExp(op_exp) => {
                let lhs = op_exp.mul_exp.eval()?;
                let rhs = op_exp.unary_exp.eval()?;
                match op_exp.mul_op {
                    MulOp::Mul => Ok(lhs * rhs),
                    MulOp::Div => Ok(lhs / rhs),
                    MulOp::Mod => Ok(lhs % rhs),
                }
            }
        }
    }
}

impl Eval<i32> for UnaryExp {
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            UnaryExp::PrimaryExp(exp) => exp.eval(),
            UnaryExp::FuncCall(_) => Err(ParseError::IllegalConstExpError(String::from(
                "Function Call",
            ))),
            UnaryExp::UnaryOpExp(op_exp) => {
                let exp = op_exp.unary_exp.eval()?;
                match op_exp.unary_op {
                    UnaryOp::Pos => Ok(exp),
                    UnaryOp::Neg => Ok(-exp),
                    UnaryOp::Not => Ok(if exp == 0 { 1 } else { 0 }),
                }
            }
        }
    }
}

impl Eval<i32> for PrimaryExp {
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            PrimaryExp::Exp(exp) => exp.eval(),
            PrimaryExp::LVal(l_val) => {
                Err(ParseError::IllegalConstExpError(format!("{:?}", l_val)))
            }
            PrimaryExp::Number(num) => match num {
                Number::Int(i) => Ok(*i),
            },
        }
    }
}

pub fn eval_array_size(array_index: &Vec<ConstExp>) -> Result<i32, ParseError> {
    let mut length = 1;
    for exp in array_index {
        length *= exp.eval()?;
    }
    Ok(length)
}
