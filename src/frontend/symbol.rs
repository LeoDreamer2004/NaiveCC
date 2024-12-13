//! Symbol table on parsing.

use super::ast::{ConstDef, ConstExp, ConstInitVal, Exp, FuncFParam, InitVal, VarDef};
use super::env::{Context, Environment};
use super::eval::Eval;
use super::generate::GenerateIr;
use super::AstError;
use crate::utils::namer::{global_ident, normal_ident};
use koopa::ir::builder::{GlobalInstBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::{Type, Value};

/// Symbol Table
/// Implemented as a stack of scopes
#[derive(Debug, Default)]
pub struct SymbolTable {
    items: Vec<SymbolItem>,
}

/// A collection of all symbols
#[derive(Debug, Clone)]
pub enum SymbolItem {
    Var(VarSymbol),
    VarArray(VarArraySymbol),
    Const(ConstSymbol),
    ConstArray(ConstArraySymbol),
    FParamArray(FParamArraySymbol),
    ScopeSeparator,
}

impl SymbolItem {
    fn symbol(&self) -> &dyn Symbol {
        match self {
            SymbolItem::Var(symbol) => symbol,
            SymbolItem::VarArray(symbol) => symbol,
            SymbolItem::Const(symbol) => symbol,
            SymbolItem::ConstArray(symbol) => symbol,
            SymbolItem::FParamArray(symbol) => symbol,
            SymbolItem::ScopeSeparator => panic!("Invalid access to scope separator"),
        }
    }

    fn symbol_mut(&mut self) -> &mut dyn Symbol {
        match self {
            SymbolItem::Var(symbol) => symbol,
            SymbolItem::VarArray(symbol) => symbol,
            SymbolItem::Const(symbol) => symbol,
            SymbolItem::ConstArray(symbol) => symbol,
            SymbolItem::FParamArray(symbol) => symbol,
            SymbolItem::ScopeSeparator => panic!("Invalid access to scope separator"),
        }
    }

    fn symbol_clone(&self) -> Box<dyn Symbol> {
        match self {
            SymbolItem::Var(symbol) => Box::new(symbol.clone()),
            SymbolItem::VarArray(symbol) => Box::new(symbol.clone()),
            SymbolItem::Const(symbol) => Box::new(symbol.clone()),
            SymbolItem::ConstArray(symbol) => Box::new(symbol.clone()),
            SymbolItem::FParamArray(symbol) => Box::new(symbol.clone()),
            SymbolItem::ScopeSeparator => panic!("Invalid access to scope separator"),
        }
    }
}

impl SymbolTable {
    pub fn new_symbol(&mut self, symbol: &impl SymbolLike) -> Result<Box<dyn Symbol>, AstError> {
        let item = symbol.wrap(self)?;
        self.items.push(item.clone());
        Ok(item.symbol_clone())
    }

    pub fn set_alloc(&mut self, ident: &String, alloc: Value) -> Result<(), AstError> {
        for item in &mut self.items.iter_mut().rev() {
            if matches!(item, SymbolItem::ScopeSeparator) {
                continue;
            }
            if ident == item.symbol().ident() {
                return item.symbol_mut().set_alloc(alloc);
            }
        }
        Err(AstError::SymbolNotFoundError(ident.clone()))
    }

    fn lookup_item(&self, ident: &String) -> Option<&SymbolItem> {
        // Search from the top of the stack
        for item in self.items.iter().rev() {
            if matches!(item, SymbolItem::ScopeSeparator) {
                continue;
            }
            if ident == item.symbol().ident() {
                return Some(item);
            }
        }
        None
    }

    pub fn lookup(&self, ident: &String) -> Option<&dyn Symbol> {
        self.lookup_item(ident).map(|item| item.symbol())
    }

    pub fn lookup_or(&self, ident: &String) -> Result<&dyn Symbol, AstError> {
        self.lookup(ident)
            .ok_or(AstError::SymbolNotFoundError(ident.clone()))
    }

    pub fn lookup_const_val(&self, ident: &String) -> Option<i32> {
        match self.lookup_item(ident) {
            Some(SymbolItem::Const(symbol)) => Some(symbol.value),
            _ => None,
        }
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

/// Symbol trait
///
/// Symbols are always with an identifier,
/// including variables, constants, and function params
pub trait Symbol {
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
    ///
    /// # Error
    /// If the symbol is not an array
    fn get_bias(&self) -> Result<&Vec<usize>, AstError>;
    /// Returns the index of the symbol
    ///
    /// # Error
    /// If the symbol has not been added to the symbol table
    /// or the symbol is not an array
    fn index(&self, indexes: &Vec<Value>, ctx: &mut Context) -> Result<Value, AstError> {
        if self.is_single() {
            return Err(AstError::TypeError("Not an array".to_string()));
        }
        let bias = self.get_bias()?;
        if indexes.len() > bias.len() - 1 {
            return Err(AstError::IllegalAccessError(
                "Array dimensions mismatch".to_string(),
            ));
        }
        let mut ptr = self.get_alloc()?;
        for &index in indexes {
            ptr = ctx.local_val().get_elem_ptr(ptr, index);
            ctx.add_inst(ptr);
        }
        Ok(ptr)
    }
    /// Returns the type of the symbol
    fn get_type(&self, ty: Type) -> Type {
        if self.is_single() {
            ty.clone()
        } else {
            gen_array_type(ty, self.get_bias().unwrap())
        }
    }
    /// Generates the value of the symbol
    fn gen_value(
        &self,
        ty: Type,
        init: Option<Init>,
        env: &mut Environment,
    ) -> Result<(), AstError>;
}

#[derive(Debug, Clone)]
pub struct ConstSymbol {
    ident: String,
    value: i32,
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
// In code, we make a trick to set the first demension size = 1
#[derive(Debug, Clone)]
pub struct FParamArraySymbol {
    ident: String,
    alloc: Option<Value>,
    ptr_bias: Vec<usize>,
}

/****************** Symbol Traits & Implementations *******************/

impl Symbol for VarSymbol {
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

    fn gen_value(
        &self,
        ty: Type,
        init: Option<Init>,
        env: &mut Environment,
    ) -> Result<(), AstError> {
        let mut has_init = false;
        let init = match init {
            Some(Init::Var(symbol)) => {
                has_init = true;
                if env.ctx.is_global() {
                    let int = symbol.as_element()?.eval(&env.table)?;
                    env.ctx.glb_val().integer(int)
                } else {
                    symbol.as_element()?.generate_on(env)?.value()
                }
            }
            None => env.ctx.val().zero_init(ty.clone()),
            _ => unreachable!(),
        };
        let alloc = if env.ctx.is_global() {
            env.ctx.glb_val().global_alloc(init)
        } else {
            let alloc = env.ctx.local_val().alloc(ty.clone());
            env.ctx.add_inst(alloc);
            if has_init {
                let store = env.ctx.local_val().store(init, alloc);
                env.ctx.add_inst(store);
            }
            alloc
        };
        env.table.set_alloc(self.ident(), alloc)?;
        env.ctx.set_value_name(alloc, normal_ident(self.ident()));
        Ok(())
    }
}

impl Symbol for ConstSymbol {
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

    fn gen_value(&self, _: Type, _: Option<Init>, _: &mut Environment) -> Result<(), AstError> {
        Ok(())
    }
}

impl Symbol for VarArraySymbol {
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

    fn gen_value(
        &self,
        ty: Type,
        init: Option<Init>,
        env: &mut Environment,
    ) -> Result<(), AstError> {
        let bias = self.get_bias()?;
        let arr_ty = gen_array_type(ty, bias);
        let alloc = if env.ctx.is_global() {
            match init {
                Some(Init::Var(init)) => {
                    let init = init
                        .parse(self.get_bias()?)?
                        .map(|exp| exp.eval(&env.table).unwrap());
                    fill_global(init, bias, env)
                }
                None => {
                    let value = env.ctx.glb_val().zero_init(arr_ty);
                    env.ctx.glb_val().global_alloc(value)
                }
                _ => unreachable!(),
            }
        } else {
            match init {
                Some(Init::Var(init)) => {
                    let init = init
                        .parse(self.get_bias()?)?
                        .map(|exp| exp.generate_on(env).unwrap().value());
                    fill_local(init, self.get_bias()?, env)
                }
                None => {
                    let alloc = env.ctx.local_val().alloc(arr_ty);
                    env.ctx.add_inst(alloc);
                    alloc
                }
                _ => unreachable!(),
            }
        };
        env.ctx.set_value_name(alloc, global_ident(self.ident()));
        env.table.set_alloc(self.ident(), alloc)?;
        Ok(())
    }
}

impl Symbol for ConstArraySymbol {
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

    fn gen_value(
        &self,
        _: Type,
        init: Option<Init>,
        env: &mut Environment,
    ) -> Result<(), AstError> {
        let init = match init {
            Some(Init::Const(symbol)) => symbol
                .parse(self.get_bias()?)?
                .map(|exp| exp.eval(&env.table).unwrap()),
            _ => unreachable!(),
        };

        let alloc = if env.ctx.is_global() {
            fill_global(init, self.get_bias()?, env)
        } else {
            let init = init.map(|&int| env.ctx.local_val().integer(int));
            fill_local(init, self.get_bias()?, env)
        };
        env.ctx.set_value_name(alloc, global_ident(self.ident()));
        env.table.set_alloc(self.ident(), alloc)?;
        Ok(())
    }
}

impl Symbol for FParamArraySymbol {
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

    fn get_type(&self, ty: Type) -> Type {
        let mut bias = self.ptr_bias.clone();
        bias.remove(0);
        Type::get_pointer(gen_array_type(ty, &bias))
    }

    fn index(&self, indexes: &Vec<Value>, ctx: &mut Context) -> Result<Value, AstError> {
        if indexes.is_empty() {
            return self.get_alloc();
        }
        if indexes.len() > self.get_bias()?.len() - 1 {
            return Err(AstError::IllegalAccessError(
                "Array dimensions mismatch".to_string(),
            ));
        }
        let mut ptr = ctx.local_val().load(self.get_alloc()?);
        ctx.add_inst(ptr);
        ptr = ctx.local_val().get_ptr(ptr, indexes[0]);
        ctx.add_inst(ptr);
        for &index in indexes[1..].iter() {
            ptr = ctx.local_val().get_elem_ptr(ptr, index);
            ctx.add_inst(ptr);
        }
        Ok(ptr)
    }
    fn gen_value(&self, _: Type, _: Option<Init>, _: &mut Environment) -> Result<(), AstError> {
        // Do nothing, as it is finished in Koopa builtin
        Ok(())
    }
}

fn gen_array_type(ty: Type, bias: &Vec<usize>) -> Type {
    let mut ty = ty;
    for i in (0..bias.len() - 1).rev() {
        let dim = bias[i] / bias[i + 1];
        ty = Type::get_array(ty, dim);
    }
    ty
}

fn fill_global(init: ArrayParseResult<i32>, bias: &Vec<usize>, env: &mut Environment) -> Value {
    let ints = init.unfold(0);
    let mut values: Vec<Value> = ints.iter().map(|&x| env.ctx.val().integer(x)).collect();
    for i in (0..bias.len() - 1).rev() {
        let step = bias[i] / bias[i + 1];
        let num = bias[0] / bias[i];
        let mut temp = vec![];
        for j in 0..num {
            let aggr = values[j * step..(j + 1) * step].to_vec();
            temp.push(env.ctx.val().aggregate(aggr));
        }
        values = temp;
    }
    // At last, there will be only one value in the values
    assert!(values.len() == 1, "Global array initialization error");
    env.ctx.glb_val().global_alloc(values[0])
}

fn fill_local(init: ArrayParseResult<Value>, bias: &Vec<usize>, env: &mut Environment) -> Value {
    let arr_ty = gen_array_type(Type::get_i32(), bias);
    let alloc = env.ctx.local_val().alloc(arr_ty.clone());
    env.ctx.add_inst(alloc);
    for (idx, &value) in init
        .unfold(env.ctx.local_val().integer(0))
        .iter()
        .enumerate()
    {
        let mut c_idx = idx;
        let index = env.ctx.local_val().integer((c_idx / bias[1]) as i32);
        let mut ptr = env.ctx.local_val().get_elem_ptr(alloc, index);
        env.ctx.add_inst(ptr);
        for i in 2..bias.len() {
            c_idx %= bias[i - 1];
            let index = env.ctx.local_val().integer((c_idx / bias[i]) as i32);
            ptr = env.ctx.local_val().get_elem_ptr(ptr, index);
            env.ctx.add_inst(ptr);
        }
        let store = env.ctx.local_val().store(value, ptr);
        env.ctx.add_inst(store);
    }
    alloc
}

pub trait SymbolLike {
    fn ident(&self) -> &String;
    fn wrap(&self, table: &SymbolTable) -> Result<SymbolItem, AstError>;
}

impl SymbolLike for ConstDef {
    fn ident(&self) -> &String {
        &self.ident
    }

    fn wrap(&self, table: &SymbolTable) -> Result<SymbolItem, AstError> {
        let ident = self.ident().clone();
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
    fn ident(&self) -> &String {
        &self.ident
    }

    fn wrap(&self, table: &SymbolTable) -> Result<SymbolItem, AstError> {
        let ident = self.ident().clone();
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
    fn ident(&self) -> &String {
        &self.ident
    }

    fn wrap(&self, table: &SymbolTable) -> Result<SymbolItem, AstError> {
        let ident = self.ident().clone();
        if !self.is_array {
            Ok(SymbolItem::Var(VarSymbol { ident, alloc: None }))
        } else {
            let mut bias = 1;
            let mut ptr_bias = vec![1];
            for size in self.array_size.iter().rev() {
                bias *= size.eval(table)? as usize;
                ptr_bias.push(bias);
            }
            ptr_bias.push(*ptr_bias.last().unwrap());
            ptr_bias.reverse();
            Ok(SymbolItem::FParamArray(FParamArraySymbol {
                ident,
                alloc: None,
                ptr_bias,
            }))
        }
    }
}

#[derive(Debug)]
pub struct ArrayParseResult<E> {
    elements: Vec<ArrayElements<E>>,
}

impl<E> ArrayParseResult<E> {
    pub fn map<T>(self, mut map: impl FnMut(&E) -> T) -> ArrayParseResult<T> {
        let elements = self
            .elements
            .iter()
            .map(|e| match e {
                ArrayElements::Word(e) => ArrayElements::Word(map(e)),
                ArrayElements::Zero(size) => ArrayElements::Zero(*size),
            })
            .collect();
        ArrayParseResult { elements }
    }

    /// Unfold the array elements
    pub fn unfold(self, zero: E) -> Vec<E>
    where
        E: Clone + Sized,
    {
        let mut res = vec![];
        for element in self.elements.clone() {
            match element {
                ArrayElements::Word(e) => res.push(e),
                ArrayElements::Zero(size) => res.extend(vec![zero.clone(); size]),
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

pub enum ArrayType<'a, A: Initilizer> {
    Element(&'a A::E),
    Array(&'a Vec<A>),
}

pub enum Init<'a> {
    Const(&'a ConstInitVal),
    Var(&'a InitVal),
}

pub trait Initilizer
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

    /// Parse the initializer of an array, and flatten it as [`ArrayParseResult`]   
    ///
    /// # Examples
    /// To parse the following array:
    /// ```
    /// int a[3][3] = {{1, 2}, 3, 4, {5}, 6}
    /// ```
    /// After parsing, the result will be like:
    /// ```
    /// a = {1, 2, 0, 3, 4, 0, 5, 6, 0}
    /// ```
    fn parse(&self, bias_stack: &Vec<usize>) -> Result<ArrayParseResult<&Self::E>, AstError> {
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

impl Initilizer for ConstInitVal {
    type E = ConstExp;
    fn as_type(&self) -> ArrayType<Self> {
        match self {
            ConstInitVal::ConstExp(exp) => ArrayType::Element(exp),
            ConstInitVal::ConstInitVals(vals) => ArrayType::Array(vals),
        }
    }
}

impl Initilizer for InitVal {
    type E = Exp;
    fn as_type(&self) -> ArrayType<Self> {
        match self {
            InitVal::Exp(exp) => ArrayType::Element(exp),
            InitVal::InitVals(vals) => ArrayType::Array(vals),
        }
    }
}
