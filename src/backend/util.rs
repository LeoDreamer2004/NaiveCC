use koopa::ir::FunctionData;

pub trait AsmIdentify {
    fn original_ident(&self) -> String;
}

impl AsmIdentify for FunctionData {
    fn original_ident(&self) -> String {
        self.name()[1..].to_string()
    }
}
