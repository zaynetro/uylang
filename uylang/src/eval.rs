use std::{cell::RefCell, collections::HashMap, rc::Rc};

use anyhow::{anyhow, Result};

use crate::{
    ir::tree::{Block, Expr, FnDef, IfBlock, Literal, MatchPattern, Module, ObjectEntry, Stmt},
    parser::ast::{BinaryOp, UnaryOp},
    std::NativeFn,
};

pub fn eval(module: &Module) -> Result<EvaluatedModule> {
    let mut r = Runner::default();
    let exports = r.eval(module)?;
    Ok(exports)
}

#[derive(Default)]
pub struct Runner {}

// Let's think about our eval loop.
// UY language is interpreted. We first need to evaluate the module
// which defines all methods and runs the statements.
// Once we have an evaluated module it should be possible to call exported functions.
// How do we evaluate calling a function?
// What are the values that variables could hold?
// Ref:
//   - https://262.ecma-international.org/#sec-ecmascript-language-types
//   - https://262.ecma-international.org/#sec-fundamental-objects
//
// - Undefined
// - Null
// - Boolean
// - String
// - Symbol
// - Number
// - BigInt
// - Object <-- Function is an Object
//
// So in JS world we would get an Object that will have a Function prototype.
// What do I want here? Also I do need to support constructors or smth.
// Forget about classes and constructors. We will support only Objects (structs) and functions.
// Now I am trying to figure out what could an expression return. In addition to literals and could return a pointer to a function definition.
// Well it could also return a lambda function.

impl Runner {
    // NOTE: Rhai returns a Target that is converted to Dynamic with Target::take_or_clone
    //       Ref: https://github.com/rhaiscript/rhai/blob/main/src/eval/target.rs#L86
    fn eval_expr(&mut self, scope: &Scope, e: &Expr) -> Result<ValueRef> {
        let res = match e {
            Expr::Literal(v) => Value::from(v.clone()).into(),
            Expr::Jsx(jsx) => {
                // Ref: https://github.com/preactjs/preact/blob/10.17.1/src/create-element.js#L55

                let mut props = HashMap::with_capacity(jsx.props.len() + 1);
                for (key, e) in &jsx.props {
                    props.insert(key.clone(), self.eval_expr(scope, e)?);
                }

                let mut children = Vec::with_capacity(jsx.children.len());
                for e in &jsx.children {
                    children.push(self.eval_expr(scope, e)?);
                }
                props.insert("children".into(), Value::List(Rc::new(children)).into());

                let obj: HashMap<String, ValueRef> = HashMap::from([
                    ("type".into(), Value::Str(jsx.name.clone()).into()),
                    ("props".into(), Value::Object(Rc::new(props)).into()),
                ]);
                Value::Object(Rc::new(obj)).into()
            }
            Expr::VarRef(name) => scope
                .find(&name)
                .ok_or_else(|| anyhow!("Ref '{name}' to exist"))?,
            Expr::Grouped(e) => self.eval_expr(scope, e)?,
            Expr::List(list) => Value::List(Rc::new(
                list.into_iter()
                    .map(|el| self.eval_expr(scope, el))
                    .into_iter()
                    .collect::<Result<_>>()?,
            ))
            .into(),
            Expr::Object(entries) => {
                let mut obj = HashMap::with_capacity(entries.len());
                for entry in entries {
                    match entry {
                        ObjectEntry::KeyValue(k, e) => {
                            obj.insert(k.clone(), self.eval_expr(scope, e)?);
                        }
                        ObjectEntry::Rest(name) => {
                            let v = scope
                                .find(&name)
                                .ok_or_else(|| anyhow!("Ref '{name}' to exist"))?;

                            match &*v.0.borrow() {
                                Value::List(list) => {
                                    for (i, v) in list.iter().enumerate() {
                                        obj.insert(format!("{i}"), v.clone());
                                    }
                                }
                                Value::Object(sub) => {
                                    for (k, v) in sub.iter() {
                                        obj.insert(k.clone(), v.clone());
                                    }
                                }
                                _ => {}
                            };
                        }
                    }
                }
                Value::Object(Rc::new(obj)).into()
            }
            Expr::Field(e, field) => {
                tracing::trace!(e = ?e, field, "Evaluating Expr::Field");
                let value = self.eval_expr(scope, e)?;
                let res = match &*value.0.borrow() {
                    Value::Num(_) => {
                        tracing::warn!("Number has no methods");
                        Value::Null.into()
                    }
                    Value::Str(s) => match field.as_str() {
                        "length" => Value::Num(s.len() as f64).into(),
                        _ => {
                            tracing::warn!("Unsupported field={field} on String");
                            Value::Null.into()
                        }
                    },
                    Value::List(list) => {
                        let found =
                            crate::std::list::field(&field, list, &value).ok_or_else(|| {
                                anyhow!("Function/property '{}' to exist on List", field)
                            })?;
                        found
                    }
                    Value::Object(ref obj) => {
                        if let Some(v) = obj.get(field) {
                            v.clone()
                        } else {
                            // TODO: try returning the method
                            tracing::warn!("Object={:?} has no field={}", obj, field);
                            Value::Null.into()
                        }
                    }
                    _ => {
                        tracing::warn!("Can't access field={} on {:?}", field, value);
                        Value::Null.into()
                    }
                };
                res
            }
            Expr::Index(left, right) => {
                let index = self.eval_expr(scope, right)?;
                let val = self.eval_expr(scope, left)?;

                let res = match &*index.0.borrow() {
                    Value::Num(n) => match &*val.0.borrow() {
                        Value::List(list) => {
                            let i = n.floor() as usize;
                            list.get(i).cloned().unwrap_or_else(|| Value::Null.into())
                        }
                        v => {
                            tracing::warn!("Trying to access a non-list by index: value={:?}", v);
                            Value::Null.into()
                        }
                    },
                    // TODO: support strings on objects also
                    i => {
                        tracing::warn!("Trying to access by non-number: index={:?}", i);
                        Value::Null.into()
                    }
                };
                res
            }
            Expr::Unary(u) => {
                let e = self.eval_expr(scope, &u.expr)?;
                e.unary_op(&u.op)
            }
            Expr::Binary(b) => {
                let left = self.eval_expr(scope, &b.left)?;
                let right = self.eval_expr(scope, &b.right)?;
                left.binary_op(&right, &b.op).into()
            }
            Expr::Block(b) => {
                let child = Scope::child(scope);
                self.eval_block(&child, b)?
            }
            Expr::If(IfBlock {
                cond,
                block,
                r#else,
            }) => {
                let cond = self.eval_expr(scope, cond)?.0.borrow().as_bool();

                if cond {
                    self.eval_block(scope, block)?
                } else if let Some(e) = r#else {
                    self.eval_expr(scope, e)?
                } else {
                    Value::Null.into()
                }
            }
            Expr::FnCall(f) => {
                let res = self.eval_expr(scope, &f.fn_name)?;
                let res = match &*res.0.borrow() {
                    Value::Fn(fdef) => self.eval_fn(scope, &f.args, fdef)?,
                    v => {
                        return Err(anyhow!("Can't call an non-function: {:?}", v));
                    }
                };
                res
            }
            Expr::Lambda(def) => Value::Fn(Rc::new(FnValue::User(def.clone()))).into(),
        };

        Ok(res)
    }

    pub(crate) fn eval_block(&mut self, scope: &Scope, b: &Block) -> Result<ValueRef> {
        for (fn_name, f) in &b.fns {
            scope.add(
                fn_name.clone(),
                Value::Fn(Rc::new(FnValue::User(f.clone()))).into(),
                false,
            );
        }

        for st in &b.statements {
            self.eval_stmt(scope, st)?;
        }

        let res = match &b.out {
            Some(e) => self.eval_expr(scope, e)?,
            None => Value::Null.into(),
        };
        Ok(res)
    }

    fn eval_stmt(&mut self, scope: &Scope, st: &Stmt) -> Result<()> {
        match st {
            Stmt::LineComment(_) => {}
            Stmt::VarDef(var) => {
                let value = self.eval_expr(scope, &var.def)?;
                self.assign_pattern(scope, &var.pattern, value, var.exported)?;
            }
            Stmt::Expr(e) => {
                self.eval_expr(scope, e)?;
            }
        }

        Ok(())
    }

    fn assign_pattern(
        &mut self,
        scope: &Scope,
        pattern: &MatchPattern,
        value: ValueRef,
        exported: bool,
    ) -> Result<()> {
        match pattern {
            MatchPattern::Empty => {}
            MatchPattern::Ident(var_name) => {
                scope.add(var_name.clone(), value, exported);
            }
            MatchPattern::Slice { items, rest } => {
                match &*value.0.borrow() {
                    Value::Str(s) => {
                        let mut iter = items.iter().peekable();
                        let mut chars = s.chars();

                        loop {
                            // We need this extra check so that we don't advance chars iterator
                            if let None = iter.peek() {
                                break;
                            }

                            match (iter.next(), chars.next()) {
                                (Some(p), None) => {
                                    // String value is shorter than our pattern --> assign nulls
                                    self.assign_pattern(scope, p, Value::Null.into(), exported)?;
                                }
                                (Some(p), Some(v)) => {
                                    let s = Value::Str(v.into());
                                    self.assign_pattern(scope, p, s.into(), exported)?;
                                }
                                (None, _) => break,
                            }
                        }

                        // Assign the rest of the string
                        if let Some(r) = rest {
                            let v = Value::Str(chars.collect());
                            scope.add(r.clone(), v.into(), exported);
                        }
                    }
                    Value::List(list) => {
                        let mut iter = items.iter().peekable();
                        let mut elements = list.iter();

                        loop {
                            // We need this extra check so that we don't advance elements iterator
                            if let None = iter.peek() {
                                break;
                            }

                            match (iter.next(), elements.next()) {
                                (Some(p), None) => {
                                    // Array value is shorter than our pattern --> assign nulls
                                    self.assign_pattern(scope, p, Value::Null.into(), exported)?;
                                }
                                (Some(p), Some(v)) => {
                                    self.assign_pattern(scope, p, v.clone(), exported)?;
                                }
                                (None, _) => break,
                            }
                        }

                        // Assign the rest of the array
                        if let Some(r) = rest {
                            let v = Value::List(Rc::new(elements.cloned().collect()));
                            scope.add(r.clone(), v.into(), exported);
                        }
                    }
                    _ => {
                        return Err(anyhow!("{:?} is not iterable", value));
                    }
                }
            }
        }

        Ok(())
    }

    pub fn eval_fn(&mut self, scope: &Scope, e_args: &[Expr], f: &FnValue) -> Result<ValueRef> {
        let res = match f {
            FnValue::Native(f) => {
                let mut args = f.args.clone();
                for e in e_args {
                    args.push(self.eval_expr(scope, e)?);
                }
                (f.body)(&scope, &args)?
            }
            FnValue::User(fdef) => {
                let FnDef { args, body, .. } = fdef;
                let child = Scope::child(scope);
                for (name, e) in args.into_iter().zip(e_args.into_iter()) {
                    let v = self.eval_expr(scope, &e)?;
                    child.add(name.clone(), v, false);
                }

                self.eval_block(&child, body)?
            }
        };
        Ok(res)
    }

    pub fn eval(&mut self, p: &Module) -> Result<EvaluatedModule> {
        let mut scope = Scope::default();

        for import in &p.imports {
            match import.from.as_str() {
                "preact/hooks" => {
                    for ref_ in &import.refs {
                        let found = crate::std::preact_hooks::find_fn(&ref_)
                            .ok_or(anyhow!("Function {} to exist in preact/hooks", ref_))?;
                        scope.add(
                            ref_.clone(),
                            Value::Fn(Rc::new(FnValue::Native(found))).into(),
                            false,
                        );
                    }
                }
                _ => {
                    panic!("Importing from other files is not supported yet");
                }
            }
        }

        for (fn_name, f) in &p.fns {
            let exported = f.exported;
            scope.add(
                fn_name.clone(),
                Value::Fn(Rc::new(FnValue::User(f.clone()))).into(),
                exported,
            );
        }

        for st in &p.statements {
            self.eval_stmt(&mut scope, st)?;
        }

        // Keep only exported vars
        let vars = scope.vars.take();

        Ok(EvaluatedModule { vars })
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ValueRef(pub Rc<RefCell<Value>>);

impl ValueRef {
    pub fn binary_op(self, other: &Self, op: &BinaryOp) -> ValueRef {
        match op {
            // Algebraic
            BinaryOp::Add => {
                let res = self.0.borrow().num() + other.0.borrow().num();
                Value::Num(res).into()
            }
            BinaryOp::Sub => {
                let res = self.0.borrow().num() - other.0.borrow().num();
                Value::Num(res).into()
            }
            BinaryOp::Mul => {
                let res = self.0.borrow().num() * other.0.borrow().num();
                Value::Num(res).into()
            }
            BinaryOp::Div => {
                let res = self.0.borrow().num() / other.0.borrow().num();
                Value::Num(res).into()
            }
            BinaryOp::Mod => {
                let res = self.0.borrow().num() % other.0.borrow().num();
                Value::Num(res).into()
            }

            // Boolean
            BinaryOp::Eq => Value::Bool(*self.0.borrow() == *other.0.borrow()).into(),
            BinaryOp::NotEq => Value::Bool(*self.0.borrow() != *other.0.borrow()).into(),
            BinaryOp::Lt => Value::Bool(self.less_than(other)).into(),
            BinaryOp::Lte => {
                Value::Bool(self.less_than(other) || *self.0.borrow() == *other.0.borrow()).into()
            }
            BinaryOp::Gt => {
                Value::Bool(!self.less_than(other) && *self.0.borrow() != *other.0.borrow()).into()
            }
            BinaryOp::Gte => Value::Bool(!self.less_than(other)).into(),
            BinaryOp::And => {
                let res = self.0.borrow().as_bool() && other.0.borrow().as_bool();
                Value::Bool(res).into()
            }
            BinaryOp::Or => {
                let res = self.0.borrow().as_bool() || other.0.borrow().as_bool();
                Value::Bool(res).into()
            }

            // Assignment
            BinaryOp::Assign => {
                *self.0.borrow_mut() = (*other.0.borrow()).clone();
                self
            }

            // Assignment algebraic
            BinaryOp::AssignAdd => {
                let res = self.0.borrow().num() + other.0.borrow().num();
                *self.0.borrow_mut() = Value::Num(res);
                self
            }
            BinaryOp::AssignSub => {
                let res = self.0.borrow().num() - other.0.borrow().num();
                *self.0.borrow_mut() = Value::Num(res);
                self
            }
            BinaryOp::AssignMul => {
                let res = self.0.borrow().num() * other.0.borrow().num();
                *self.0.borrow_mut() = Value::Num(res);
                self
            }
            BinaryOp::AssignDiv => {
                let res = self.0.borrow().num() / other.0.borrow().num();
                *self.0.borrow_mut() = Value::Num(res);
                self
            }
            BinaryOp::AssignMod => {
                let res = self.0.borrow().num() % other.0.borrow().num();
                *self.0.borrow_mut() = Value::Num(res);
                self
            }
        }
    }

    pub fn unary_op(self, op: &UnaryOp) -> ValueRef {
        match op {
            UnaryOp::Neg => match &*self.0.borrow() {
                Value::Num(n) => Value::Num(-n).into(),
                v => panic!("Cannot negate {:?}", v),
            },
            UnaryOp::Not => Value::Bool(!self.0.borrow().as_bool()).into(),
        }
    }

    fn less_than(&self, other: &Self) -> bool {
        match (&*self.0.borrow(), &*other.0.borrow()) {
            (Value::Bool(ref s), Value::Bool(ref o)) => s < o,
            (Value::Num(ref s), Value::Num(ref o)) => s < o,
            (Value::Str(ref s), Value::Str(ref o)) => s < o,
            // TODO: need to be consistent here. If `self < other` than opposite should be true `other > self`
            (_, _) => false,
        }
    }
}

impl From<Value> for ValueRef {
    fn from(value: Value) -> Self {
        Self(Rc::new(RefCell::new(value)))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Null,
    Bool(bool),
    Num(f64),
    Str(String),
    List(Rc<Vec<ValueRef>>),
    Object(Rc<HashMap<String, ValueRef>>),
    Fn(Rc<FnValue>),
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Bool(b) => *b,
            Value::Num(n) => *n != 0.,
            Value::Str(s) => s.len() > 0,
            Value::List(l) => l.len() > 0,
            Value::Object(o) => o.len() > 0,
            Value::Fn(_) => true,
        }
    }

    pub fn num(&self) -> f64 {
        match self {
            Value::Num(n) => *n,
            _ => 0.,
        }
    }
}

impl From<Literal> for Value {
    fn from(l: Literal) -> Self {
        match l {
            Literal::Null => Self::Null,
            Literal::Bool(b) => Self::Bool(b),
            Literal::Num(n) => Self::Num(n),
            Literal::Str(s) => Self::Str(s),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum FnValue {
    Native(NativeFn),
    User(FnDef),
}

pub struct EvaluatedModule {
    vars: HashMap<String, ScopeVar>,
}

impl EvaluatedModule {
    pub fn exported_vars(&self) -> HashMap<&str, &ValueRef> {
        self.vars
            .iter()
            .filter(|(_, var)| var.exported)
            .map(|(name, var)| (name.as_ref(), &var.value))
            .collect()
    }

    pub fn var(&self, name: &str) -> Option<Value> {
        self.vars.get(name).map(|v| (*v.value.0.borrow()).clone())
    }

    pub fn var_ref(&self, name: &str) -> Option<&ValueRef> {
        self.vars.get(name).map(|v| &v.value)
    }

    pub fn build_scope(&self) -> Scope {
        let scope = Scope::default();
        for (name, var) in &self.vars {
            scope.add(name.to_string(), var.value.clone(), var.exported);
        }
        scope
    }

    pub(crate) fn do_call_fn(scope: &Scope, f: &FnValue, args: &[ValueRef]) -> Result<ValueRef> {
        let mut runner = Runner::default();

        match f {
            FnValue::Native(f) => (f.body)(scope, args),
            FnValue::User(f) => {
                let child = Scope::child(&scope);
                for (name, arg) in f.args.iter().zip(args.iter()) {
                    child.add(name.clone(), arg.clone(), false);
                }

                runner.eval_block(&child, &f.body)
            }
        }
    }

    pub fn call_fn(&self, scope: &Scope, name: &str, args: &[ValueRef]) -> Result<ValueRef> {
        if let Some(v) = self.vars.get(name) {
            match *v.value.0.borrow() {
                Value::Fn(ref f) => Self::do_call_fn(scope, f, args),
                _ => Err(anyhow!("Reference '{name}' is not a function")),
            }
        } else {
            Err(anyhow!("Function '{name}' not found"))
        }
    }
}

#[derive(Default, Clone)]
pub struct Scope {
    vars: Rc<RefCell<HashMap<String, ScopeVar>>>,
    parent: Option<Rc<Scope>>,
}

#[derive(Debug, PartialEq)]
// NOTE: Rhai scope: https://github.com/rhaiscript/rhai/blob/main/src/types/scope.rs#L70
//       It uses a Vec to store values and accesses them either by name or by index.
//       Then after evaluating a block it rolls back to previous index.
struct ScopeVar {
    value: ValueRef,
    exported: bool,
}

impl Scope {
    pub fn add(&self, name: String, value: ValueRef, exported: bool) {
        tracing::trace!(name, ?value, "Adding to scope");
        self.vars.borrow_mut().insert(
            name,
            ScopeVar {
                value: value.into(),
                exported,
            },
        );
    }

    pub fn find(&self, name: &str) -> Option<ValueRef> {
        RefCell::borrow(&self.vars)
            .get(name)
            .map(|v| &v.value)
            .cloned()
            .or_else(|| {
                if let Some(ref p) = self.parent {
                    p.find(name)
                } else {
                    None
                }
            })
    }

    pub fn child(scope: &Scope) -> Scope {
        tracing::trace!("Creating child scope");
        let parent = Self {
            vars: scope.vars.clone(),
            parent: scope.parent.clone(),
        };

        let mut c = Self::default();
        c.parent = Some(Rc::new(parent));
        c
    }
}

impl Drop for Scope {
    fn drop(&mut self) {
        tracing::trace!("Exiting child scope");
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use pretty_assertions::assert_eq;

    use crate::{
        eval::{eval, Value},
        ir::tree::{
            BinaryExpr, Block, Expr, FnCall, FnDef, FnMeta, Literal, MatchPattern, Module, Stmt,
            VarDef,
        },
        parser::ast::BinaryOp,
    };

    #[test]
    fn test_eval() {
        let module = Module {
            imports: vec![],
            fns: vec![(
                "one".into(),
                FnDef {
                    exported: true,
                    args: vec![],
                    body: Block {
                        fns: vec![],
                        statements: vec![],
                        out: Some(Box::new(Expr::Literal(Literal::Num(1.0)))),
                    },
                    meta: FnMeta::default(),
                },
            )],
            statements: vec![Stmt::VarDef(VarDef {
                exported: true,
                pattern: MatchPattern::Ident("two".into()),
                def: Expr::Literal(Literal::Num(2.0)),
            })],
        };

        let evaluated = eval(&module).unwrap();
        assert_eq!(evaluated.var("two"), Some(Value::Num(2.)),);

        // assert_eq!(
        //     Value::Num(1.0),
        //     evaluated.call_fn(&Scope::default(), "one", vec![]).unwrap()
        // );

        let module = Module {
            imports: vec![],
            fns: vec![],
            statements: vec![Stmt::VarDef(VarDef {
                exported: true,
                pattern: MatchPattern::Ident("one".into()),
                def: Expr::FnCall(FnCall {
                    fn_name: Box::new(Expr::Field(
                        Box::new(Expr::List(vec![
                            Expr::Literal(Literal::Num(1.)),
                            Expr::Literal(Literal::Num(2.)),
                        ])),
                        "map".into(),
                    )),
                    args: vec![Expr::Lambda(FnDef {
                        exported: false,
                        args: vec!["el".into()],
                        body: Block {
                            fns: vec![],
                            statements: vec![],
                            out: Some(Box::new(Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::VarRef("el".into())),
                                op: BinaryOp::Mul,
                                right: Box::new(Expr::Literal(Literal::Num(2.))),
                            }))),
                        },
                        meta: FnMeta::default(),
                    })],
                }),
            })],
        };

        let evaluated = eval(&module).unwrap();
        assert_eq!(
            Some(Value::List(Rc::new(vec![
                Value::Num(2.).into(),
                Value::Num(4.).into()
            ]))),
            evaluated.var("one"),
        );
    }
}
