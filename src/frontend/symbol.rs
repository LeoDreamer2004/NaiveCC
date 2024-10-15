use super::ast::{ConstDef, VarDef};

/// Symbol Table
/// Implemented as a stack of scopes
pub type SymbolTable = Vec<SymbolTableItem>;

#[derive(Debug)]
pub enum SymbolTableItem {
    Symbol(Symbol),
    ScopeSeparator,
}

#[derive(Debug)]
pub enum Symbol {
    Const(ConstDef),
    Var(VarDef),
}

pub trait SymbolTableTrait {
    fn add(&mut self, symbol: Symbol);
    fn lookup(&self, ident: &str) -> Option<&Symbol>;
    fn enter_scope(&mut self);
    fn exit_scope(&mut self);
}

impl SymbolTableTrait for SymbolTable {
    fn add(&mut self, symbol: Symbol) {
        self.push(SymbolTableItem::Symbol(symbol));
    }

    fn lookup(&self, ident: &str) -> Option<&Symbol> {
        for item in self {
            match item {
                SymbolTableItem::Symbol(symbol) => match symbol {
                    Symbol::Const(const_def) => {
                        if const_def.ident == ident {
                            return Some(symbol);
                        }
                    }
                    Symbol::Var(var_def) => {
                        if var_def.ident == ident {
                            return Some(symbol);
                        }
                    }
                },
                SymbolTableItem::ScopeSeparator => {
                    continue;
                }
            }
        }
        None
    }

    fn enter_scope(&mut self) {
        self.push(SymbolTableItem::ScopeSeparator);
    }

    fn exit_scope(&mut self) {
        loop {
            match self.pop() {
                Some(SymbolTableItem::ScopeSeparator) => break,
                None => break,
                _ => {}
            }
        }
    }
}
