use super::ast::{ConstDef, ConstInitVal, FuncFParam, VarDef};
use super::eval::*;
use super::generate::Context;
use super::AstError;
use koopa::ir::Value;

/// Symbol Table
/// Implemented as a stack of scopes
#[derive(Debug, Default)]
pub struct SymbolTable {
    pub items: Vec<SymbolTableItem>,
}

#[derive(Debug)]
pub struct SymbolTableItem {
    pub symbol: Symbol,
    pub alloc: Option<Value>,
}

impl SymbolTable {
    pub fn add_const(&mut self, symbol: Symbol) {
        self.items.push(SymbolTableItem {
            symbol,
            alloc: None,
        });
    }

    pub fn add_var(&mut self, symbol: Symbol, alloc: Value) {
        self.items.push(SymbolTableItem {
            symbol,
            alloc: Some(alloc),
        });
    }

    pub fn lookup(&self, ident: &String) -> Option<&SymbolTableItem> {
        // Search from the top of the stack
        for item in self.items.iter().rev() {
            let symbol = &item.symbol;
            match symbol {
                Symbol::Const(const_def) => {
                    if const_def.ident == *ident {
                        return Some(item);
                    }
                }
                Symbol::Var(var_def) => {
                    if var_def.ident == *ident {
                        return Some(item);
                    }
                }
                Symbol::ConstArray(const_array_def) => {
                    if const_array_def.ident == *ident {
                        return Some(item);
                    }
                }
                Symbol::VarArray(var_array_def) => {
                    if var_array_def.ident == *ident {
                        return Some(item);
                    }
                }
                _ => {}
            }
        }
        None
    }

    pub fn lookup_const(&self, ident: &String) -> Option<&ConstSymbol> {
        if let Some(SymbolTableItem {
            symbol: Symbol::Const(const_def),
            ..
        }) = self.lookup(ident)
        {
            Some(const_def)
        } else {
            None
        }
    }

    pub fn lookup_const_array(&self, ident: &String) -> Option<&ConstArraySymbol> {
        if let Some(SymbolTableItem {
            symbol: Symbol::ConstArray(const_array_def),
            ..
        }) = self.lookup(ident)
        {
            Some(const_array_def)
        } else {
            None
        }
    }

    pub fn lookup_var(&self, ident: &String) -> Option<(&VarSymbol, &Value)> {
        if let Some(SymbolTableItem {
            symbol: Symbol::Var(var_def),
            alloc,
        }) = self.lookup(ident)
        {
            Some((var_def, alloc.as_ref().unwrap()))
        } else {
            None
        }
    }

    pub fn lookup_var_array(&self, ident: &String) -> Option<(&VarArraySymbol, &Value)> {
        if let Some(SymbolTableItem {
            symbol: Symbol::VarArray(var_array_def),
            alloc,
        }) = self.lookup(ident)
        {
            Some((var_array_def, alloc.as_ref().unwrap()))
        } else {
            None
        }
    }

    pub fn enter_scope(&mut self) {
        self.add_const(Symbol::ScopeSeparator);
    }

    pub fn exit_scope(&mut self) {
        loop {
            match self.items.pop().unwrap().symbol {
                Symbol::ScopeSeparator => break,
                _ => {}
            }
        }
    }
}

/****************** Symbol Definitions *******************/

#[derive(Debug)]
pub enum Symbol {
    ScopeSeparator,
    Const(ConstSymbol),
    ConstArray(ConstArraySymbol),
    Var(VarSymbol),
    VarArray(VarArraySymbol),
}

#[derive(Debug)]
pub struct ConstSymbol {
    pub ident: String,
    pub value: i32,
}

#[derive(Debug)]
pub struct ConstArraySymbol {
    pub ident: String,
    pub size: usize,
    pub value: Vec<i32>,
}

#[derive(Debug)]
pub struct VarSymbol {
    pub ident: String,
}

#[derive(Debug)]
pub struct VarArraySymbol {
    pub ident: String,
    pub size: usize,
}

/****************** Symbol Traits & Implementations *******************/

pub trait ArraySizable {
    fn is_single(&self) -> bool;
}

impl ArraySizable for ConstDef {
    fn is_single(&self) -> bool {
        self.array_size.is_empty()
    }
}

impl ArraySizable for VarDef {
    fn is_single(&self) -> bool {
        self.array_size.is_empty()
    }
}

impl ArraySizable for FuncFParam {
    fn is_single(&self) -> bool {
        !self.is_array
    }
}

pub trait IntoSymbol {
    fn to_symbol(&self, context: &Context) -> Result<Symbol, AstError>;
}

impl IntoSymbol for ConstDef {
    fn to_symbol(&self, context: &Context) -> Result<Symbol, AstError> {
        let ident = self.ident.clone();
        match &self.const_init_val {
            ConstInitVal::ConstExp(const_exp) => {
                let value = const_exp.eval(context)?;
                Ok(Symbol::Const(ConstSymbol { ident, value }))
            }
            ConstInitVal::ConstInitVals(init_vals) => {
                todo!();
            }
        }
    }
}

impl IntoSymbol for VarDef {
    fn to_symbol(&self, context: &Context) -> Result<Symbol, AstError> {
        if self.is_single() {
            let ident = self.ident.clone();
            Ok(Symbol::Var(VarSymbol { ident }))
        } else {
            todo!();
        }
    }
}

impl IntoSymbol for FuncFParam {
    fn to_symbol(&self, context: &Context) -> Result<Symbol, AstError> {
        if self.is_single() {
            let ident = self.ident.clone();
            Ok(Symbol::Var(VarSymbol { ident }))
        } else {
            todo!();
        }
    }
}
