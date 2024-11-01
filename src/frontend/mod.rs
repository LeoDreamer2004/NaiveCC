pub mod ast;
mod builtin;
#[macro_use]
mod context;
mod eval;
mod generate;
mod loops;
mod opt;
mod symbol;
mod transform;

pub use generate::{build_ir, emit_ir, AstError};
