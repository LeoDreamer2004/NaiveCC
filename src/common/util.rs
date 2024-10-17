use std::marker::PhantomData;

use koopa::ir::{entities::BasicBlockData, FunctionData};

/// An easy generator for unique IDs.
#[derive(Debug, Default)]
pub struct IDGenerator {
    current_id: u64,
}

impl IDGenerator {
    pub fn new() -> Self {
        IDGenerator {
            current_id: 0,
        }
    }

    pub fn generate(&mut self) -> u64 {
        let id = self.current_id;
        self.current_id += 1;
        id
    }
}

pub fn local_name(ident: &String, func_data: &FunctionData, id: u64) -> String{
    // TODO: If blockdata doesn't have a name, generate a name for it
    format!("__{}_{}_{}", func_data.name()[1..].to_string(), ident, id)
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn generate_id() {
        let mut int_generator: IDGenerator = IDGenerator::new();
        assert_eq!(int_generator.generate(), 0);
        assert_eq!(int_generator.generate(), 1);
        assert_eq!(int_generator.generate(), 2);
    }
}
