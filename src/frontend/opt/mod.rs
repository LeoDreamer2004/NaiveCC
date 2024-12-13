mod bfs;
mod cp;
mod cf;
mod cse;
mod dbe;
mod dce;
mod uce;
mod util;

pub use bfs::BlockFlowSimplify;
pub use cp::CopyPropagation;
pub use cf::ConstantsFold;
pub use cse::CommonSubexpression;
pub use dbe::DeadBlockElimination;
pub use dce::DeadCodeElimination;
pub use uce::UnreadCodeElimination;
