use std::ops;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Literal<S: ops::Deref<Target = str> = String> {
    Integer(i64),
    Float(f64),
    Char(char),
    String(S),
}

/// Represents a part of an operator expression (= an expression using operators).
///
/// Specifically an expression which is either in parenthesis or not an operator expression, with
/// the potentially many or missing unary and the one binary (unless it's the first element in the operator
/// expression, in which case there would be none) operators before it
#[derive(Clone, PartialEq, Debug)]
pub struct WithOperator<T, S: ops::Deref<Target = str> = String> {
    pub operators_before: Vec<S>,
    pub value: T,
}

#[derive(Clone, PartialEq, Debug)]
pub struct TypeInfo<S: ops::Deref<Target = str>> {
    pub name: S,
    pub args: Vec<TypeInfo<S>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct MaybeTyped<S: ops::Deref<Target = str> = String> {
    pub name: S,
    pub type_info: Option<TypeInfo<S>>,
}

/// Represents a block or keyword statement.
///
/// The block follows one of the following patterns:
/// ```simplescript
/// keyword;
///
/// keyword:
///     indented code
///
/// keyword (ident1, ident2, ..., identn);
///
/// keyword (ident1, ident2, ..., identn):
///     indented code
///
/// # All forms can have indented code after them, but they are ommited in the following examples
/// keyword ident();
/// keyword ident(ident1, ident2, ..., identn);
/// keyword <- expr;  # if the expression has no parenthesis
/// keyword <- (expr1, expr2, ..., exprn);
/// keyword (ident1, ident2, ..., identn) <- expr;
/// keyword (ident1, ident2, ..., identn) <- (expr1, expr2, ..., exprn);
///
/// # ... (the other forms can be inferred)
/// ```
#[derive(Clone, PartialEq, Debug)]
pub struct PartialBlock<T, S: ops::Deref<Target = str> = String> {
    pub keyword: S,
    pub ident: Option<S>,
    pub binds: Vec<MaybeTyped<S>>,
    pub args: Vec<T>,
    pub block: Vec<Statement<S>>,
}

/// Represents a statement.
/// 
/// A statement may be part of an expression, this is possible by putting it in parenthesis.
///
/// Example:
/// ```simplescript
/// call(
///     1,
///     2,
///     (
///         keyword <- 3;
///     ),
/// )
/// ```
#[derive(Clone, PartialEq, Debug)]
pub enum Statement<S: ops::Deref<Target = str> = String> {
    Expression(Ast<S>),
    Set(MaybeTyped<S>, PartialAst<S>),
    Block(Block<S>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum PartialAst<T, S: ops::Deref<Target = str> = String> {
    Identifier(S),
    Literal(Literal<S>),
    OperatorString(Vec<WithOperator<T, S>>),
    Call(Vec<T>),
    Statements(Vec<Statement<S>>),
}

impl<T, S: ops::Deref<Target = str>> PartialAst<T, S> {
    pub fn transform<U, F: FnMut(&T) -> U>(&self, mut conversion: F) -> PartialAst<U, S> where S: Clone {
        match *self {
            PartialAst::Identifier(ref ident) => PartialAst::Identifier(ident.clone()),
            PartialAst::Literal(ref lit) => PartialAst::Literal(lit.clone()),
            PartialAst::OperatorString(ref ops) => PartialAst::OperatorString(ops.iter().map(|i| WithOperator { operators_before: i.operators_before.clone(), value: conversion(&i.value) }).collect()),
            PartialAst::Call(ref parts) => PartialAst::Call(parts.iter().map(conversion).collect()),
            PartialAst::Statements(ref statements) => PartialAst::Statements(statements.clone()),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Ast<S: ops::Deref<Target = str> = String>(pub PartialAst<Ast<S>, S>);

impl<S: ops::Deref<Target = str>> From<Ast<S>> for PartialAst<Ast<S>, S> {
    fn from(this: Ast<S>) -> Self {
        this.0
    }
}

impl<S: ops::Deref<Target = str>> ops::Deref for Ast<S> {
    type Target = PartialAst<Ast<S>, S>;

    fn deref(&self) -> &PartialAst<Ast<S>, S> {
        &self.0
    }
}

impl<S: ops::Deref<Target = str>> ops::DerefMut for Ast<S> {
    fn deref_mut(&mut self) -> &mut PartialAst<Ast<S>, S> {
        &mut self.0
    }
}

impl<S: ops::Deref<Target = str>> PartialAst<Ast<S>, S> {
    fn eval_impl<T, F: FnMut(PartialAst<T, S>) -> T>(&self, conversion: &mut F) -> T where S: Clone {
        let partial = self.transform(|part| part.eval(&mut*conversion));
        conversion(partial)
    }

    pub fn eval<T, F: FnMut(PartialAst<T, S>) -> T>(&self, mut conversion: F) -> T where S: Clone {
        self.eval_impl(&mut conversion)
    }
}

pub type Block<S> = PartialBlock<Ast<S>, S>;
