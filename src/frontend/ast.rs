#[derive(Debug, Clone)]
pub struct CompUnit {
    pub comp_items: Vec<CompItem>,
}

#[derive(Debug, Clone)]
pub enum CompItem {
    FuncDef(FuncDef),
    Decl(Decl),
}

#[derive(Debug, Clone)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub b_type: BType,
    pub const_defs: Vec<ConstDef>,
}

#[derive(Debug, Clone, Copy)]
pub enum BType {
    Int,
}

#[derive(Debug, Clone)]
pub struct ConstDef {
    pub ident: String,
    pub array_size: Vec<ConstExp>,
    pub const_init_val: ConstInitVal,
}

#[derive(Debug, Clone)]
pub enum ConstInitVal {
    ConstExp(ConstExp),
    ConstInitVals(Box<Vec<ConstInitVal>>),
}

#[derive(Debug, Clone)]
pub enum ConstExp {
    LOrExp(LOrExp),
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub b_type: BType,
    pub var_defs: Vec<VarDef>,
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub ident: String,
    pub array_size: Vec<ConstExp>,
    pub init_val: Option<InitVal>,
}

#[derive(Debug, Clone)]
pub enum InitVal {
    Exp(Exp),
    InitVals(Box<Vec<InitVal>>),
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub params: Vec<FuncFParam>,
    pub block: Block,
}

#[derive(Debug, Clone, Copy)]
pub enum FuncType {
    Void,
    BType(BType),
}

#[derive(Debug, Clone)]
pub struct FuncFParam {
    pub b_type: BType,
    pub ident: String,
    pub is_array: bool,
    // For a[][5], 'array_size' is Some([5]).
    pub array_size: Vec<Exp>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Stmt(Stmt),
    Decl(Decl),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Empty,
    Assign(Box<Assign>),
    Exp(Box<Exp>),
    Block(Box<Block>),
    If(Box<If>),
    While(Box<While>),
    Break(Break),
    Continue(Continue),
    Return(Box<Return>),
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub l_val: LVal,
    pub exp: Exp,
}

#[derive(Debug, Clone)]
pub struct LVal {
    pub ident: String,
    pub array_index: Vec<Exp>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Cond,
    pub stmt: Stmt,
    pub else_stmt: Option<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Break;

#[derive(Debug, Clone)]
pub struct Continue;

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Cond,
    pub stmt: Stmt,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub exp: Option<Exp>,
}

#[derive(Debug, Clone)]
pub enum Exp {
    LOrExp(LOrExp),
}

#[derive(Debug, Clone)]
pub enum Cond {
    LOrExp(Box<LOrExp>),
}

#[derive(Debug, Clone)]
pub struct LValExp {
    pub ident: String,
    pub array_index: Vec<Exp>,
}

#[derive(Debug, Clone)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    LValExp(Box<LValExp>),
    Number(Number),
}

#[derive(Debug, Clone)]
pub enum Number {
    Int(i32),
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub ident: String,
    pub args: Vec<Exp>,
}

#[derive(Debug, Clone)]
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    FuncCall(FuncCall),
    UnaryOpExp(Box<UnaryOpExp>),
}

#[derive(Debug, Clone)]
pub struct UnaryOpExp {
    pub unary_op: UnaryOp,
    pub unary_exp: UnaryExp,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum MulExp {
    UnaryExp(UnaryExp),
    MulOpExp(Box<MulOpExp>),
}

#[derive(Debug, Clone)]
pub struct MulOpExp {
    pub mul_exp: MulExp,
    pub mul_op: MulOp,
    pub unary_exp: UnaryExp,
}

#[derive(Debug, Clone, Copy)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone)]
pub enum AddExp {
    MulExp(MulExp),
    AddOpExp(Box<AddOpExp>),
}

#[derive(Debug, Clone)]
pub struct AddOpExp {
    pub add_exp: AddExp,
    pub add_op: AddOp,
    pub mul_exp: MulExp,
}

#[derive(Debug, Clone, Copy)]
pub enum AddOp {
    Add,
    Sub,
}

#[derive(Debug, Clone)]
pub enum RelExp {
    AddExp(AddExp),
    RelOpExp(Box<RelOpExp>),
}

#[derive(Debug, Clone)]
pub struct RelOpExp {
    pub rel_exp: RelExp,
    pub rel_op: RelOp,
    pub add_exp: AddExp,
}

#[derive(Debug, Clone, Copy)]
pub enum RelOp {
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone)]
pub enum EqExp {
    RelExp(RelExp),
    EqOpExp(Box<EqOpExp>),
}

#[derive(Debug, Clone)]
pub struct EqOpExp {
    pub eq_exp: EqExp,
    pub eq_op: EqOp,
    pub rel_exp: RelExp,
}

#[derive(Debug, Clone, Copy)]
pub enum EqOp {
    Eq,
    Ne,
}

#[derive(Debug, Clone)]
pub enum LAndExp {
    EqExp(EqExp),
    LAndOpExp(Box<LAndOpExp>),
}

#[derive(Debug, Clone)]
pub struct LAndOpExp {
    pub l_and_exp: LAndExp,
    pub eq_exp: EqExp,
}

#[derive(Debug, Clone)]
pub enum LOrExp {
    LAndExp(LAndExp),
    LOrOpExp(Box<LOrOpExp>),
}

#[derive(Debug, Clone)]
pub struct LOrOpExp {
    pub l_or_exp: LOrExp,
    pub l_and_exp: LAndExp,
}
