use koopa::ir::entities::BasicBlockData;
use koopa::ir::FunctionData;

pub fn identifier_rename(
    ident: &mut String,
    func_data: &FunctionData,
    block_data: &BasicBlockData,
) {
    // TODO: If blockdata doesn't have a name, generate a name for it
    (*ident) = format!(
        "__{}_{}_{}",
        func_data.name()[1..].to_string(),
        block_data.name().clone().unwrap_or_default()[1..].to_string(),
        ident
    );
}
