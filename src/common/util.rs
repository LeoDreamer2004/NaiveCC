use std::{collections::HashMap, hash::Hash};

type NameMapper = fn(u64) -> String;

/// An easy generator for unique IDs and names.
#[derive(Debug)]
pub struct NameGenerator<T> {
    current_id: u64,
    map: HashMap<T, u64>,
    f: NameMapper,
}

impl<T> NameGenerator<T>
where
    T: Eq + Hash + Clone,
{
    pub fn new(f: NameMapper) -> Self {
        NameGenerator {
            current_id: 0,
            map: HashMap::new(),
            f,
        }
    }

    /// Generate a unique ID for the given data.
    pub fn get_id(&mut self, data: T) -> u64 {
        if let Some(id) = self.map.get(&data) {
            *id
        } else {
            let id = self.current_id;
            self.map.insert(data, id);
            self.current_id += 1;
            id
        }
    }

    /// Get the name of the given data.
    pub fn get_name(&mut self, data: T) -> String {
        let id = self.get_id(data);
        (self.f)(id)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn generate_id() {
        let mut int_generator = NameGenerator::new(|id| id.to_string());
        assert_eq!(int_generator.get_id(2), 0);
        assert_eq!(int_generator.get_id(3), 1);
        assert_eq!(int_generator.get_name(2), "0".to_string());
    }
}
