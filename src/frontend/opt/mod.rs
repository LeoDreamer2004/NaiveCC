mod bfs;
mod ci;
mod cse;
mod dbe;
mod dce;
mod uce;

pub use bfs::BlockFlowSimplifier;
pub use ci::ConstantsInline;
pub use cse::CommonSubexpression;
pub use dbe::DeadBlockElimination;
pub use dce::DeadCodeElimination;
pub use uce::UnreadCodeElimination;
