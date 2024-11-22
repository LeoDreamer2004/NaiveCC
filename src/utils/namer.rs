use std::{collections::HashMap, hash::Hash};

type NameMapper = fn(u64) -> String;

/// An easy generator for unique IDs and names.
#[derive(Debug, Clone)]
pub struct IdGenerator<T> {
    current_id: u64,
    map: HashMap<T, u64>,
    f: NameMapper,
}

impl<T> Default for IdGenerator<T>
where
    T: Eq + Hash,
{
    fn default() -> Self {
        IdGenerator::new(|id| id.to_string())
    }
}

impl<T> IdGenerator<T>
where
    T: Eq + Hash,
{
    pub fn new(f: NameMapper) -> Self {
        IdGenerator {
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

#[derive(Debug, Clone)]
pub struct UniqueNameGenerator {
    suffix: String,
    map: HashMap<String, u64>,
}

impl UniqueNameGenerator {
    pub fn new(suffix: String) -> Self {
        Self {
            suffix,
            map: HashMap::new(),
        }
    }

    pub fn get_name(&mut self, name: String) -> String {
        match self.map.get_mut(&name) {
            Some(i) => {
                *i += 1;
                format!("{}{}{}", name, &self.suffix, i)
            }
            None => {
                self.map.insert(name.clone(), 0);
                name
            }
        }
    }
}

impl Default for UniqueNameGenerator {
    fn default() -> Self {
        Self::new(String::from("_"))
    }
}

pub fn normal_ident(ident: &String) -> String {
    format!("%{}", ident)
}

pub fn global_ident(ident: &String) -> String {
    format!("@{}", ident)
}

pub fn original_ident(ident: &String) -> String {
    ident[1..].to_string()
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn generate_id() {
        let mut int_gen = IdGenerator::new(|id| id.to_string());
        assert_eq!(int_gen.get_id(2), 0);
        assert_eq!(int_gen.get_id(3), 1);
        assert_eq!(int_gen.get_name(2), "0".to_string());

        let mut name_gen = UniqueNameGenerator::default();
        assert_eq!(name_gen.get_name("Alice".to_string()), "Alice".to_string());
        assert_eq!(
            name_gen.get_name("Alice".to_string()),
            "Alice_1".to_string()
        );
    }
}
