use super::ast::{ConstDef, ConstExp, ConstInitVal, Exp, FuncFParam, InitVal, VarDef};
use super::eval::Eval;
use super::generate::Context;
use super::AstError;
use core::any::Any;
use koopa::ir::builder::{LocalInstBuilder, ValueBuilder};
use koopa::ir::{BinaryOp, Value};

/// Symbol Table
/// Implemented as a stack of scopes
#[derive(Default)]
pub struct SymbolTable {
    pub items: Vec<SymbolItem>,
}

#[derive(Debug)]
pub enum SymbolItem {
    Var(VarSymbol),
    VarArray(VarArraySymbol),
    Const(ConstSymbol),
    ConstArray(ConstArraySymbol),
    ScopeSeparator,
}

impl SymbolItem {
    fn symbol(&self) -> Option<&dyn Symbol> {
        match self {
            SymbolItem::Var(symbol) => Some(symbol),
            SymbolItem::VarArray(symbol) => Some(symbol),
            SymbolItem::Const(symbol) => Some(symbol),
            SymbolItem::ConstArray(symbol) => Some(symbol),
            SymbolItem::ScopeSeparator => None,
        }
    }

    fn symbol_mut(&mut self) -> Option<&mut dyn Symbol> {
        match self {
            SymbolItem::Var(symbol) => Some(symbol),
            SymbolItem::VarArray(symbol) => Some(symbol),
            SymbolItem::Const(symbol) => Some(symbol),
            SymbolItem::ConstArray(symbol) => Some(symbol),
            SymbolItem::ScopeSeparator => None,
        }
    }
}

impl SymbolTable {
    pub fn new_symbol(&mut self, symbol: &impl SymbolLike) -> Result<(), AstError> {
        self.items.push(symbol.wrap(self)?);
        Ok(())
    }

    pub fn set_alloc(&mut self, ident: &String, alloc: Value) -> Result<(), AstError> {
        self.items
            .last_mut()
            .unwrap()
            .symbol_mut()
            .ok_or(AstError::SymbolNotFoundError(ident.clone()))?
            .set_alloc(alloc)
    }

    pub fn lookup_item(&self, ident: &String) -> Option<&SymbolItem> {
        // Search from the top of the stack
        for item in self.items.iter().rev() {
            let symbol = item.symbol();
            if let Some(symbol) = symbol {
                if ident == symbol.ident() {
                    return Some(item);
                }
            }
        }
        None
    }

    pub fn lookup(&self, ident: &String) -> Option<&dyn Symbol> {
        self.lookup_item(ident)?.symbol()
    }

    pub fn lookup_spec<T: Symbol + 'static>(&self, ident: &String) -> Option<&T> {
        let symbol = self.lookup(ident)?;
        symbol.as_any().downcast_ref::<T>()
    }

    pub fn enter_scope(&mut self) {
        self.items.push(SymbolItem::ScopeSeparator);
    }

    pub fn exit_scope(&mut self) {
        loop {
            match self.items.pop().unwrap() {
                SymbolItem::ScopeSeparator => break,
                _ => {}
            }
        }
    }
}

/****************** Symbol Definitions *******************/

pub trait Symbol {
    fn as_any(&self) -> &dyn Any;
    /// Returns the identifier of the symbol
    fn ident(&self) -> &String;
    /// Returns true if the symbol is a single variable
    fn is_single(&self) -> bool;
    /// Returns true if the symbol is a constant
    fn is_const(&self) -> bool;
    /// Sets the allocation of the symbol
    ///
    /// # Error
    /// If the symbol is not a variable
    fn set_alloc(&mut self, alloc: Value) -> Result<(), AstError>;
    /// Returns the allocation of the symbol
    ///     
    /// # Error
    /// If the symbol does not have an allocation, or it is not a variable
    fn get_alloc(&self) -> Result<Value, AstError>;
    /// Returns the dimension bias of the symbol
    fn get_bias(&self) -> Result<&Vec<usize>, AstError>;
    /// Converts the symbol to a symbol item
    fn to_item(self) -> SymbolItem;
    /// Returns the size of the symbol
    ///
    /// # Error
    /// If the symbol has not been added to the symbol table
    /// or the symbol is not an array
    fn size(&self) -> Result<usize, AstError> {
        if self.is_single() {
            Err(AstError::TypeError("Not an array".to_string()))
        } else {
            self.get_bias().map(|bias| bias[0])
        }
    }
    /// Returns the index of the symbol
    ///
    /// # Error
    /// If the symbol has not been added to the symbol table
    /// or the symbol is not an array
    fn index(&self, indexes: &Vec<Value>, context: &mut Context) -> Result<Value, AstError> {
        if self.is_single() {
            return Err(AstError::TypeError("Not an array".to_string()));
        }
        let bias = self.get_bias()?;
        if indexes.len() != bias.len() - 1 {
            return Err(AstError::IllegalAccessError(
                "Array dimensions mismatch".to_string(),
            ));
        }

        // indexes[0] * bias[1] + indexes[1] * bias[2] + ... + indexes[n-1] * bias[n]
        // simple optimization: bias[n] always equals to 1
        let len = indexes.len();
        let mut res = indexes[len - 1];
        for (i, &value) in indexes.iter().enumerate() {
            if i == len - 1 {
                break;
            }
            let b = context.new_value().integer(bias[i + 1] as i32);
            let mul = context.new_value().binary(BinaryOp::Mul, value, b);
            context.add_inst(mul);
            res = context.new_value().binary(BinaryOp::Add, res, mul);
            context.add_inst(res);
        }
        Ok(res)
    }
}

#[derive(Debug, Clone)]
pub struct ConstSymbol {
    ident: String,
    pub value: i32,
}

#[derive(Debug, Clone)]
pub struct ConstArraySymbol {
    ident: String,
    alloc: Option<Value>,
    // for a[2][3], it's ptr_bias = [6, 3, 1]
    ptr_bias: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct VarSymbol {
    ident: String,
    alloc: Option<Value>,
}

#[derive(Debug, Clone)]
pub struct VarArraySymbol {
    ident: String,
    alloc: Option<Value>,
    ptr_bias: Vec<usize>,
}

// What is different from VarArraySymbol is that the first dimension of the array is unknown
// Which means it can reach an infinite size (unsafe though)
#[derive(Debug)]
pub struct ParamArraySymbol {
    ident: String,
    alloc: Option<Value>,
    ptr_bias: Vec<usize>,
}

/****************** Symbol Traits & Implementations *******************/

impl Symbol for VarSymbol {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn ident(&self) -> &String {
        &self.ident
    }

    fn is_single(&self) -> bool {
        true
    }

    fn is_const(&self) -> bool {
        false
    }

    fn set_alloc(&mut self, alloc: Value) -> Result<(), AstError> {
        self.alloc = Some(alloc);
        Ok(())
    }

    fn get_alloc(&self) -> Result<Value, AstError> {
        self.alloc
            .ok_or(AstError::IllegalAccessError("No allocation".to_string()))
    }

    fn get_bias(&self) -> Result<&Vec<usize>, AstError> {
        Err(AstError::TypeError("Not an array".to_string()))
    }

    fn to_item(self) -> SymbolItem {
        SymbolItem::Var(self)
    }
}

impl Symbol for ConstSymbol {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn ident(&self) -> &String {
        &self.ident
    }

    fn is_single(&self) -> bool {
        true
    }

    fn is_const(&self) -> bool {
        true
    }

    fn set_alloc(&mut self, _: Value) -> Result<(), AstError> {
        Err(AstError::TypeError(
            "Constants cannot bind allocation".to_string(),
        ))
    }

    fn get_alloc(&self) -> Result<Value, AstError> {
        Err(AstError::TypeError(
            "Constants do not have allocation".to_string(),
        ))
    }

    fn get_bias(&self) -> Result<&Vec<usize>, AstError> {
        Err(AstError::TypeError("Not an array".to_string()))
    }

    fn to_item(self) -> SymbolItem {
        SymbolItem::Const(self)
    }
}

impl Symbol for VarArraySymbol {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn ident(&self) -> &String {
        &self.ident
    }

    fn is_single(&self) -> bool {
        false
    }

    fn is_const(&self) -> bool {
        false
    }

    fn set_alloc(&mut self, alloc: Value) -> Result<(), AstError> {
        self.alloc = Some(alloc);
        Ok(())
    }

    fn get_alloc(&self) -> Result<Value, AstError> {
        self.alloc
            .ok_or(AstError::IllegalAccessError("No allocation".to_string()))
    }

    fn get_bias(&self) -> Result<&Vec<usize>, AstError> {
        Ok(&self.ptr_bias)
    }

    fn to_item(self) -> SymbolItem {
        SymbolItem::VarArray(self)
    }
}

impl Symbol for ConstArraySymbol {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn ident(&self) -> &String {
        &self.ident
    }

    fn is_single(&self) -> bool {
        false
    }

    fn is_const(&self) -> bool {
        true
    }

    fn set_alloc(&mut self, alloc: Value) -> Result<(), AstError> {
        self.alloc = Some(alloc);
        Ok(())
    }

    fn get_alloc(&self) -> Result<Value, AstError> {
        self.alloc
            .ok_or(AstError::IllegalAccessError("No allocation".to_string()))
    }

    fn get_bias(&self) -> Result<&Vec<usize>, AstError> {
        Ok(&self.ptr_bias)
    }

    fn to_item(self) -> SymbolItem {
        SymbolItem::ConstArray(self)
    }
}

pub trait SymbolLike {
    fn get_ident(&self) -> &String;
    fn wrap(&self, table: &SymbolTable) -> Result<SymbolItem, AstError>;
}

impl SymbolLike for ConstDef {
    fn get_ident(&self) -> &String {
        &self.ident
    }

    fn wrap(&self, table: &SymbolTable) -> Result<SymbolItem, AstError> {
        let ident = self.get_ident().clone();
        match &self.const_init_val {
            ConstInitVal::ConstExp(const_exp) => {
                if !self.array_size.is_empty() {
                    return Err(AstError::InitializeError(
                        "Cannot use an array to initialize a single constant".to_string(),
                    ));
                }
                let value = const_exp.eval(table)?;
                Ok(SymbolItem::Const(ConstSymbol { ident, value }))
            }
            ConstInitVal::ConstInitVals(_) => {
                let mut bias = 1;
                let mut ptr_bias = vec![1];
                for exp in self.array_size.iter().rev() {
                    bias *= exp.eval(table)? as usize;
                    ptr_bias.push(bias);
                }
                ptr_bias.reverse();
                Ok(SymbolItem::ConstArray(ConstArraySymbol {
                    ident,
                    alloc: None,
                    ptr_bias,
                }))
            }
        }
    }
}

impl SymbolLike for VarDef {
    fn get_ident(&self) -> &String {
        &self.ident
    }

    fn wrap(&self, table: &SymbolTable) -> Result<SymbolItem, AstError> {
        let ident = self.get_ident().clone();
        if self.array_size.is_empty() {
            Ok(SymbolItem::Var(VarSymbol { ident, alloc: None }))
        } else {
            let mut bias = 1;
            let mut ptr_bias = vec![1];
            for size in self.array_size.iter().rev() {
                bias *= size.eval(table)? as usize;
                ptr_bias.push(bias);
            }
            ptr_bias.reverse();

            Ok(SymbolItem::VarArray(VarArraySymbol {
                ident,
                alloc: None,
                ptr_bias,
            }))
        }
    }
}

impl SymbolLike for FuncFParam {
    fn get_ident(&self) -> &String {
        &self.ident
    }

    fn wrap(&self, table: &SymbolTable) -> Result<SymbolItem, AstError> {
        if !self.is_array {
            let ident = self.get_ident().clone();
            Ok(SymbolItem::Var(VarSymbol { ident, alloc: None }))
        } else {
            todo!();
        }
    }
}

#[derive(Debug)]
pub struct ArrayParseResult<'a, E> {
    elements: Vec<ArrayElements<&'a E>>,
}

impl<'a, E> ArrayParseResult<'a, E> {
    /// Unfold the array elements
    pub fn unfold<F, T>(&self, map: F, zero: T) -> Vec<T>
    where
        F: Fn(&E) -> T,
        T: Clone + Sized,
    {
        let mut res = vec![];
        for element in self.elements.clone() {
            match element {
                ArrayElements::Word(e) => res.push(map(e)),
                ArrayElements::Zero(size) => res.extend(vec![zero.clone(); size]),
            }
        }
        res
    }

    /// Filter the zero array elements, return with the elements and its index in the array
    pub fn filter(&self) -> Vec<(&E, usize)> {
        let mut cursor = 0;
        let mut res = vec![];
        for element in self.elements.clone() {
            match element {
                ArrayElements::Word(e) => {
                    res.push((e, cursor));
                    cursor += 1;
                }
                ArrayElements::Zero(size) => cursor += size,
            }
        }
        res
    }
}

/// Array element, used to represent the elements of an array
#[derive(Debug, Clone)]
enum ArrayElements<E> {
    /// An element
    Word(E),
    /// A series of zeros, with the length of the series
    Zero(usize),
}

pub enum ArrayType<'a, A: ArrayInitilizer> {
    Element(&'a A::E),
    Array(&'a Box<Vec<A>>),
}

pub trait ArrayInitilizer
where
    Self: Sized,
{
    /// The type of the elements in the array
    type E;

    fn as_type(&self) -> ArrayType<Self>;
    fn as_element(&self) -> Result<&Self::E, AstError> {
        match self.as_type() {
            ArrayType::Element(e) => Ok(e),
            _ => Err(AstError::TypeError("Not an element".to_string())),
        }
    }
    fn parse(&self, bias_stack: &Vec<usize>) -> Result<ArrayParseResult<Self::E>, AstError> {
        if bias_stack.is_empty() {
            return Err(AstError::InitializeError(
                "Array dimensions mismatch".to_string(),
            ));
        }
        let size = bias_stack[0];
        match self.as_type() {
            ArrayType::Element(e) => {
                if size == 1 {
                    let elements = vec![ArrayElements::Word(e)];
                    Ok(ArrayParseResult { elements })
                } else {
                    let elements = vec![ArrayElements::Word(e), ArrayElements::Zero(size - 1)];
                    Ok(ArrayParseResult { elements })
                }
            }
            ArrayType::Array(children) => {
                let mut cursor = 0;
                let mut elements = Vec::new();
                for child in children.iter() {
                    let res = match child.as_type() {
                        ArrayType::Element(e) => {
                            cursor += 1;
                            vec![ArrayElements::Word(e)]
                        }
                        ArrayType::Array(_) => {
                            let mut next_stack = vec![];
                            for (i, bias) in bias_stack.iter().enumerate() {
                                if i == 0 {
                                    continue;
                                }
                                if cursor % bias == 0 {
                                    next_stack = bias_stack[i..].to_vec();
                                    cursor += bias;
                                    break;
                                }
                            }
                            child.parse(&next_stack)?.elements
                        }
                    };
                    elements.extend(res);
                }
                elements.push(ArrayElements::Zero(size - cursor));
                Ok(ArrayParseResult { elements })
            }
        }
    }
}

impl ArrayInitilizer for ConstInitVal {
    type E = ConstExp;
    fn as_type(&self) -> ArrayType<Self> {
        match self {
            ConstInitVal::ConstExp(exp) => ArrayType::Element(exp),
            ConstInitVal::ConstInitVals(vals) => ArrayType::Array(vals),
        }
    }
}

impl ArrayInitilizer for InitVal {
    type E = Exp;
    fn as_type(&self) -> ArrayType<Self> {
        match self {
            InitVal::Exp(exp) => ArrayType::Element(exp),
            InitVal::InitVals(vals) => ArrayType::Array(vals),
        }
    }
}
