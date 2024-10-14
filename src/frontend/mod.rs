pub mod ast;
mod generate;
mod transform;
mod eval;
mod util;

pub use generate::build_program;
pub use generate::emit_ir;

#[derive(Debug)]
pub enum ParseError {
    FunctionNotFoundError(String),
    IllegalConstExpError(String),
    AssignError(String),

    UnknownError(String),
}
