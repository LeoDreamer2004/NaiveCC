mod bfs;
mod ci;
mod cse;
mod dbe;
mod dce;

pub use bfs::BlockFlowSimplifier;
pub use ci::ConstantsInline;
pub use cse::CommonSubexpression;
pub use dbe::DeadBlockElimination;
pub use dce::DeadCodeElimination;
