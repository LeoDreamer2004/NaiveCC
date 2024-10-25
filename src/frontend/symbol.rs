use super::ast::{ConstDef, ConstExp, ConstInitVal, Exp, FuncFParam, InitVal, VarDef};
use super::eval::Eval;
use super::generate::{Context, GenerateIr};
use super::AstError;
use koopa::ir::builder::{GlobalInstBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::{Type, Value};

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

impl SymbolTable {
    pub fn new_symbol(&mut self, symbol: &impl SymbolLike) -> Result<&SymbolItem, AstError> {
        self.items.push(symbol.wrap(self)?);
        Ok(self.items.last_mut().unwrap())
    }

    pub fn set_alloc(&mut self, ident: &String, alloc: Value) -> Result<(), AstError> {
        match self.items.last_mut().unwrap() {
            SymbolItem::Var(symbol) => symbol.set_alloc(alloc),
            SymbolItem::VarArray(symbol) => symbol.set_alloc(alloc),
            SymbolItem::Const(symbol) => symbol.set_alloc(alloc),
            SymbolItem::ConstArray(symbol) => symbol.set_alloc(alloc),
            SymbolItem::ScopeSeparator => Err(AstError::UnknownError(
                "Trying to allocate scope separator".to_string(),
            )),
        }
    }

    pub fn lookup_item(&self, ident: &String) -> Option<&SymbolItem> {
        // Search from the top of the stack
        for item in self.items.iter().rev() {
            let d = match item {
                SymbolItem::Var(symbol) => symbol.ident().clone(),
                SymbolItem::VarArray(symbol) => symbol.ident().clone(),
                SymbolItem::Const(symbol) => symbol.ident().clone(),
                SymbolItem::ConstArray(symbol) => symbol.ident().clone(),
                SymbolItem::ScopeSeparator => String::new(),
            };
            if ident.clone() == d {
                return Some(item);
            }
        }
        None
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

pub trait Symbol: Clone {
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
    /// Returns the size of the symbol
    ///
    /// # Error
    /// If the symbol has not been added to the symbol table
    /// or the symbol is not an array
    fn size(&self) -> Result<usize, AstError> {
        if self.is_single() {
            Err(AstError::TypeError("Not an array".to_string()))
        } else {
            Ok(self.get_bias()?[0])
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
        if indexes.len() > bias.len() - 1 {
            return Err(AstError::IllegalAccessError(
                "Array dimensions mismatch".to_string(),
            ));
        }
        let mut ptr = self.get_alloc()?;
        for &index in indexes {
            ptr = context.new_value().get_elem_ptr(ptr, index);
            context.add_inst(ptr);
        }
        Ok(ptr)
    }

    fn gen_value(
        &self,
        ty: Type,
        init: &Option<Init>,
        context: &mut Context,
    ) -> Result<(), AstError>;
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
        init: &Option<Init>,
        context: &mut Context,
    ) -> Result<(), AstError> {
        let init = match init {
            Some(Init::Var(symbol)) => symbol.as_element()?.generate_on(context)?,
            None => context.zero_init(ty.clone()),
            _ => unreachable!(),
        };
        let alloc = if context.is_global() {
            context.glb_new_value().global_alloc(init)
        } else {
            context.alloc_and_store(init, ty.clone())
        };
        context.syb_table.set_alloc(self.ident(), alloc)?;
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

    fn gen_value(&self, _: Type, _: &Option<Init>, _: &mut Context) -> Result<(), AstError> {
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
        init: &Option<Init>,
        context: &mut Context,
    ) -> Result<(), AstError> {
        let bias = self.get_bias()?;
        let arr_ty = gen_array_type(&ty, bias);
        let alloc = if context.is_global() {
            match init {
                Some(Init::Var(init)) => {
                    let init = init
                        .parse(self.get_bias()?)?
                        .map(|exp| exp.eval(&context.syb_table).unwrap());
                    fill_global(init, bias, context)
                }
                None => {
                    let value = context.glb_new_value().zero_init(arr_ty);
                    context.glb_new_value().global_alloc(value)
                }
                _ => unreachable!(),
            }
        } else {
            match init {
                Some(Init::Var(init)) => {
                    let init = init
                        .parse(self.get_bias()?)?
                        .map(|exp| exp.generate_on(context).unwrap());
                    fill_local(init, self.get_bias()?, context)
                }
                None => context.new_value().alloc(arr_ty),
                _ => unreachable!(),
            }
        };
        context.syb_table.set_alloc(self.ident(), alloc)?;
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
        ty: Type,
        init: &Option<Init>,
        context: &mut Context,
    ) -> Result<(), AstError> {
        let init = match init {
            Some(Init::Const(symbol)) => symbol
                .parse(self.get_bias()?)?
                .map(|exp| exp.eval(&context.syb_table).unwrap()),
            _ => unreachable!(),
        };

        let alloc = if context.is_global() {
            fill_global(init, self.get_bias()?, context)
        } else {
            let init = init.map(|&int| context.new_value().integer(int));
            fill_local(init, self.get_bias()?, context)
        };
        context.syb_table.set_alloc(self.ident(), alloc)?;
        Ok(())
    }
}

fn gen_array_type(ty: &Type, bias: &Vec<usize>) -> Type {
    let mut ty = ty.clone();
    for i in (0..bias.len() - 1).rev() {
        let dim = bias[i] / bias[i + 1];
        ty = Type::get_array(ty, dim);
    }
    ty
}

fn fill_global(init: ArrayParseResult<i32>, bias: &Vec<usize>, context: &mut Context) -> Value {
    let ints = init.unfold(0);
    let mut values: Vec<Value> = ints.iter().map(|&x| context.integer(x)).collect();
    for i in (0..bias.len() - 1).rev() {
        let step = bias[i] / bias[i + 1];
        let num = bias[0] / bias[i];
        let mut temp = vec![];
        for j in 0..num {
            let aggr = values[j * step..(j + 1) * step].to_vec();
            temp.push(context.aggregate(aggr));
        }
        values = temp;
    }
    // At last, there will be only one value in the values
    context.glb_new_value().global_alloc(values[0])
}

fn fill_local(init: ArrayParseResult<Value>, bias: &Vec<usize>, context: &mut Context) -> Value {
    let arr_ty = gen_array_type(&Type::get_i32(), bias);
    let alloc = context.new_value().alloc(arr_ty);
    context.add_inst(alloc);
    for (value, idx) in init.filter() {
        let mut c_idx = idx;
        let index = context.new_value().integer((c_idx / bias[1]) as i32);
        let mut ptr = context.new_value().get_elem_ptr(alloc, index);
        context.add_inst(ptr);
        for i in 2..bias.len() {
            c_idx %= bias[i - 1];
            let index = context.new_value().integer((c_idx / bias[i]) as i32);
            ptr = context.new_value().get_elem_ptr(ptr, index);
            context.add_inst(ptr);
        }
        let store = context.new_value().store(value, ptr);
        context.add_inst(store);
    }
    alloc
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

    /// Filter the zero array elements, return with the elements and its index in the array
    pub fn filter(self) -> Vec<(E, usize)> {
        let mut cursor = 0;
        let mut res = vec![];
        for element in self.elements {
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

pub enum ArrayType<'a, A: Initilizer> {
    Element(&'a A::E),
    Array(&'a Box<Vec<A>>),
}

pub enum Init {
    Const(ConstInitVal),
    Var(InitVal),
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
