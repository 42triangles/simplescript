use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::ptr;
use std::rc::Rc;

use smallvec::SmallVec;

pub type StrVec<'a> = SmallVec<[&'a str; 1]>;
pub type ExprVec<'a> = SmallVec<[Expr<'a>; 1]>;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Call<'a>(pub ExprVec<'a>);

impl<'a> Call<'a> {
    pub fn new(function: Expr<'a>, mut args: ExprVec<'a>) -> Self {
        args.insert(0, function);
        Call(args)
    }

    pub fn function(&self) -> &Expr<'a> {
        &self.0[0]
    }

    pub fn args(&self) -> &[Expr<'a>] {
        &self.0[1..]
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Operation<'a> {
    pub symbol: &'a str,
    pub args: Vec<Expr<'a>>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Expr<'a> {
    Operation(Operation<'a>),
    Call(Box<Call<'a>>),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct BlockArgs<'a> {
    pub identifiers: StrVec<'a>,
    pub bound_names: StrVec<'a>,
    pub arguments: ExprVec<'a>,
    pub block: Option<Code<'a>>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Block<'a> {
    pub keyword: &'a str,
    pub args: BlockArgs<'a>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Statement<'a> {
    Block(Block<'a>),
    Expr(Expr<'a>),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct Code<'a>(pub Vec<Statement<'a>>);

#[derive(Clone, Copy, Debug)]
pub struct DataPtr(*mut ());

type InternalImplFn<'a, A, V, E, C> =
    unsafe fn(DataPtr, A, Context<'a, V, E, C>) -> Result<ContextUpdate<'a, V, E, C>, E>;

pub type ImplFn<'a, A, V, E, C> =
    fn(DataPtr, A, Context<'a, V, E, C>) -> Result<ContextUpdate<'a, V, E, C>, E>;

pub struct Implementation<'a, A, V, E = (), C: PartialOrd = usize> {
    data: *mut (),
    func: InternalImplFn<'a, A, V, E, C>,
    drop: unsafe fn(*mut ()),
}

trait ImplementationImpl<'a, A, V, E, C: PartialOrd> {
    unsafe fn call(
        this: DataPtr,
        arg: A,
        ctx: Context<'a, V, E, C>,
    ) -> Result<ContextUpdate<'a, V, E, C>, E>;

    unsafe fn drop(this: *mut ());
}

impl<
        'a,
        A,
        F: Fn(A, Context<'a, V, E, C>) -> Result<ContextUpdate<'a, V, E, C>, E>,
        V,
        E,
        C: PartialOrd,
    > ImplementationImpl<'a, A, V, E, C> for F
{
    unsafe fn call(
        this: DataPtr,
        arg: A,
        ctx: Context<'a, V, E, C>,
    ) -> Result<ContextUpdate<'a, V, E, C>, E> {
        let this = &mut *(this.0 as *mut Self);
        this(arg, ctx)
    }

    unsafe fn drop(this: *mut ()) {
        ptr::drop_in_place(this as *mut Self)
    }
}

impl<'a, A, V, E, C: PartialOrd> Implementation<'a, A, V, E, C> {
    pub fn boxed<F: Fn(A, Context<'a, V, E, C>) -> Result<ContextUpdate<'a, V, E, C>, E>>(
        func: F,
    ) -> Self {
        Implementation {
            data: Box::into_raw(Box::new(func)) as *mut (),
            func: <F as ImplementationImpl<'a, A, V, E, C>>::call,
            drop: <F as ImplementationImpl<'a, A, V, E, C>>::drop,
        }
    }

    pub fn unboxed(func: ImplFn<'a, A, V, E, C>) -> Self {
        fn drop(_: *mut ()) {}

        Implementation {
            data: ptr::null_mut(),
            func,
            drop,
        }
    }

    pub fn call(&self, arg: A, ctx: Context<'a, V, E, C>) -> Result<ContextUpdate<'a, V, E, C>, E> {
        unsafe { (self.func)(DataPtr(self.data), arg, ctx) }
    }
}

impl<'a, A, V, E, C: PartialOrd> fmt::Debug for Implementation<'a, A, V, E, C> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{implementation}}")
    }
}

impl<'a, A, V, E, C: PartialOrd> Drop for Implementation<'a, A, V, E, C> {
    fn drop(&mut self) {
        unsafe { (self.drop)(self.data) }
    }
}

pub type BlockImplementation<'a, V, E = (), C = usize> = Implementation<'a, BlockArgs<'a>, V, E, C>;
pub type OperatorImplementation<'a, V, E = (), C = usize> =
    Implementation<'a, Vec<Expr<'a>>, V, E, C>;
pub type CallImplementation<'a, V, E = (), C = usize> = Implementation<'a, Call<'a>, V, E, C>;

#[derive(Debug)]
pub struct BlockDefinition<'a, V, E = (), C: PartialOrd = usize> {
    pub keyword: &'a str,
    pub separator: Option<String>,
    pub implementation: BlockImplementation<'a, V, E, C>,
}

#[derive(Debug)]
pub struct OperatorDefinition<'a, V, E = (), C: PartialOrd = usize> {
    pub symbol: &'a str,
    pub argument_count: usize,
    pub implementation: OperatorImplementation<'a, V, E, C>,
    pub associativity: C,
}

macro_rules! opdef_new {
    ($($name:ident -> $argument_count:expr),*) => {
        impl<'a, V, E, C: PartialOrd> OperatorDefinition<'a, V, E, C> {
            $(
                pub fn $name(symbol: &'a str, implementation: OperatorImplementation<'a, V, E, C>, associativity: C) -> Self {
                    OperatorDefinition {
                        symbol,
                        argument_count: $argument_count,
                        implementation,
                        associativity,
                    }
                }
            )*
        }
    }
}

opdef_new!(unary -> 1, binary -> 2, terciary -> 3);

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Namespace<V> {
    pub local: HashMap<String, V>,
    pub parent: Option<Rc<RefCell<Namespace<V>>>>,
}

impl<V> Namespace<V> {
    pub fn new() -> Self {
        Namespace {
            local: HashMap::new(),
            parent: None,
        }
    }

    pub fn lookup<T, F: FnOnce(&V) -> T>(&self, name: &str, f: F) -> Option<T> {
        match self.local.get(name) {
            Some(value) => Some(f(value)),
            None => self
                .parent
                .as_ref()
                .and_then(|p| p.borrow().lookup(name, f)),
        }
    }

    pub fn lookup_mut<T, F: FnOnce(&mut V) -> T>(&mut self, name: &str, f: F) -> Option<T> {
        match self.local.get_mut(name) {
            Some(value) => Some(f(value)),
            None => self
                .parent
                .as_ref()
                .and_then(|p| p.borrow_mut().lookup_mut(name, f)),
        }
    }

    fn set_sub(&mut self, name: &str, value: V, toplevel: &mut HashMap<String, V>) -> bool {
        match self.local.get_mut(name) {
            Some(mut_ref) => {
                *mut_ref = value;
                false
            }
            None => match self.parent.as_ref() {
                Some(parent) => parent.borrow_mut().set_sub(name, value, toplevel),
                None => {
                    toplevel.insert(name.to_owned(), value);
                    true
                }
            },
        }
    }

    pub fn set(&mut self, name: &str, value: V) -> bool {
        match self.local.get_mut(name) {
            Some(mut_ref) => {
                *mut_ref = value;
                false
            }
            None => match self.parent.as_ref() {
                Some(parent) => parent.borrow_mut().set_sub(name, value, &mut self.local),
                None => {
                    self.local.insert(name.to_owned(), value);
                    true
                }
            },
        }
    }
}

impl<V> Default for Namespace<V> {
    fn default() -> Self {
        Self::new()
    }
}

pub trait Evaluatable<'a, V, E, C: PartialOrd = usize> {
    type Err;

    fn eval(self, ctx: &mut Context<'a, V, E, C>) -> Result<(), Self::Err>;
}

pub enum UnknownOr<E> {
    Unknown,
    EvaluationError(E),
}

macro_rules! eval_blockop {
    ($($ty:ident, $collection:ident, $ident:ident);*) => {
        $(
            impl<'a, V, E, C: PartialOrd> Evaluatable<'a, V, E, C> for $ty<'a> {
                type Err = UnknownOr<E>;

                fn eval(self, ctx: &mut Context<'a, V, E, C>) -> Result<(), Self::Err> {
                    let ctx_clone = ctx.clone();

                    for i in &ctx.$collection {
                        if i.$ident == self.$ident {
                            return match i.implementation.call(self.args, ctx_clone) {
                                Ok(ctx_change) => {
                                    ctx_change.apply(ctx);
                                    Ok(())
                                },
                                Err(err) => Err(UnknownOr::EvaluationError(err)),
                            };
                        }
                    }

                    Err(UnknownOr::Unknown)
                }
            }
        )*
    }
}

eval_blockop!(Block, blocks, keyword; Operation, operators, symbol);

impl<'a, V, E, C: PartialOrd> Evaluatable<'a, V, E, C> for Call<'a> {
    type Err = E;

    fn eval(self, ctx: &mut Context<'a, V, E, C>) -> Result<(), E> {
        let call = ctx.call.clone();
        let clone = ctx.clone();

        call.call(self, clone)
            .map(|ctx_change| ctx_change.apply(ctx))
    }
}

impl<'a, V, E, C: PartialOrd> Evaluatable<'a, V, E, C> for Expr<'a> {
    type Err = UnknownOr<E>;

    fn eval(self, ctx: &mut Context<'a, V, E, C>) -> Result<(), Self::Err> {
        match self {
            Expr::Operation(op) => op.eval(ctx),
            Expr::Call(op) => op.eval(ctx).map_err(UnknownOr::EvaluationError),
        }
    }
}

trait SETag<E> {
    fn unknown() -> StatementError<E>;
}

struct BlockTag;

impl<E> SETag<E> for BlockTag {
    fn unknown() -> StatementError<E> {
        StatementError::UnkownBlock
    }
}

struct ExprTag;

impl<E> SETag<E> for ExprTag {
    fn unknown() -> StatementError<E> {
        StatementError::UnknownOperator
    }
}

pub enum StatementError<E> {
    UnkownBlock,
    UnknownOperator,
    EvaluationError(E),
}

impl<E> StatementError<E> {
    fn convert<T: SETag<E>>(x: UnknownOr<E>) -> Self {
        match x {
            UnknownOr::Unknown => T::unknown(),
            UnknownOr::EvaluationError(e) => StatementError::EvaluationError(e),
        }
    }

    fn block(x: UnknownOr<E>) -> Self {
        Self::convert::<BlockTag>(x)
    }

    fn expr(x: UnknownOr<E>) -> Self {
        Self::convert::<ExprTag>(x)
    }
}

impl<'a, V, E, C: PartialOrd> Evaluatable<'a, V, E, C> for Statement<'a> {
    type Err = StatementError<E>;

    fn eval(self, ctx: &mut Context<'a, V, E, C>) -> Result<(), Self::Err> {
        match self {
            Statement::Block(block) => block.eval(ctx).map_err(StatementError::block),
            Statement::Expr(expr) => expr.eval(ctx).map_err(StatementError::expr),
        }
    }
}

pub struct CodeError<E> {
    pub error: StatementError<E>,
    pub position: usize,
}

impl<'a, V, E, C: PartialOrd> Evaluatable<'a, V, E, C> for Code<'a> {
    type Err = CodeError<E>;

    fn eval(self, ctx: &mut Context<'a, V, E, C>) -> Result<(), Self::Err> {
        for (position, i) in self.0.into_iter().enumerate() {
            i.eval(ctx).map_err(|error| CodeError { error, position })?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct Context<'a, V, E = (), C: PartialOrd = usize> {
    pub blocks: Vec<Rc<BlockDefinition<'a, V, E, C>>>,
    pub operators: Vec<Rc<OperatorDefinition<'a, V, E, C>>>,
    pub namespace: Rc<RefCell<Namespace<V>>>,
    pub call: Rc<CallImplementation<'a, V, E, C>>,
}

impl<'a, V, E, C: PartialOrd> Context<'a, V, E, C> {
    pub fn new(call: Rc<CallImplementation<'a, V, E, C>>) -> Self {
        Context {
            blocks: Vec::new(),
            operators: Vec::new(),
            namespace: Rc::new(RefCell::new(Namespace::new())),
            call,
        }
    }

    pub fn eval<T: Evaluatable<'a, V, E, C>>(&mut self, x: T) -> Result<(), T::Err> {
        x.eval(self)
    }
}

impl<'a, V, E, C: PartialOrd> Clone for Context<'a, V, E, C> {
    fn clone(&self) -> Self {
        Context {
            blocks: self.blocks.clone(),
            operators: self.operators.clone(),
            namespace: self.namespace.clone(),
            call: self.call.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VecDiff<T> {
    pub remove: RefCell<Vec<usize>>,
    pub add: Vec<T>,
}

impl<T> VecDiff<T> {
    pub fn empty() -> Self {
        VecDiff {
            remove: RefCell::new(Vec::new()),
            add: Vec::new(),
        }
    }

    fn removal_step(&self, vec: &mut Vec<T>) {
        let mut remove = self.remove.borrow_mut();
        remove.sort();
        for &i in remove.iter().rev() {
            vec.swap_remove(i);
        }
    }

    pub fn apply(mut self, vec: &mut Vec<T>) {
        self.removal_step(vec);
        vec.append(&mut self.add);
    }

    pub fn apply_cloned(&self, vec: &mut Vec<T>)
    where
        T: Clone,
    {
        self.removal_step(vec);
        vec.extend_from_slice(&self.add);
    }
}

impl<T> Default for VecDiff<T> {
    fn default() -> Self {
        Self::empty()
    }
}

#[derive(Debug)]
pub struct ContextUpdate<'a, V, E, C: PartialOrd = usize> {
    pub block_diff: VecDiff<Rc<BlockDefinition<'a, V, E, C>>>,
    pub operator_diff: VecDiff<Rc<OperatorDefinition<'a, V, E, C>>>,
}

impl<'a, V, E, C: PartialOrd> ContextUpdate<'a, V, E, C> {
    pub fn empty() -> Self {
        ContextUpdate {
            block_diff: VecDiff::empty(),
            operator_diff: VecDiff::empty(),
        }
    }

    pub fn apply(&self, ctx: &mut Context<'a, V, E, C>) {
        self.block_diff.apply_cloned(&mut ctx.blocks);
        self.operator_diff.apply_cloned(&mut ctx.operators);
    }
}

impl<'a, V, E, C: PartialOrd> Clone for ContextUpdate<'a, V, E, C> {
    fn clone(&self) -> Self {
        ContextUpdate {
            block_diff: self.block_diff.clone(),
            operator_diff: self.operator_diff.clone(),
        }
    }
}

impl<'a, V, E, C: PartialOrd> Default for ContextUpdate<'a, V, E, C> {
    fn default() -> Self {
        Self::empty()
    }
}
