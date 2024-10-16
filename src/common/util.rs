use std::marker::PhantomData;

/// An easy generator for unique IDs.
pub struct IDGenerator<T> {
    current_id: u64,
    _marker: PhantomData<T>,
}

impl<T> IDGenerator<T> {
    pub fn new() -> Self {
        IDGenerator {
            current_id: 0,
            _marker: PhantomData,
        }
    }

    pub fn generate(&mut self) -> u64 {
        let id = self.current_id;
        self.current_id += 1;
        id
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn generate_id() {
        let mut int_generator: IDGenerator<i32> = IDGenerator::new();
        assert_eq!(int_generator.generate(), 0);
        assert_eq!(int_generator.generate(), 1);
        assert_eq!(int_generator.generate(), 2);
    }
}
