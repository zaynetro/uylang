//! Standard library. This module provides globally available methods.

use std::rc::Rc;

use anyhow::{anyhow, Result};

use crate::eval::{FnValue, Scope, Value, ValueRef};

pub mod preact_hooks {
    use std::{collections::HashMap, rc::Rc};

    use crate::eval::{FnValue, Value};

    use super::NativeFn;

    pub fn find_fn(name: &str) -> Option<NativeFn> {
        match name {
            "useState" => Some(use_state()),
            "useRef" => Some(use_ref()),
            _ => None,
        }
    }

    pub fn use_state() -> NativeFn {
        NativeFn {
            args: vec![],
            body: Box::new(|_scope, args| {
                let value = args.get(0).unwrap();
                let action = Value::Fn(Rc::new(FnValue::Native(no_op())));
                let res = vec![value.clone(), action.into()];
                Ok(Value::List(Rc::new(res)).into())
            }),
        }
    }

    pub fn use_ref() -> NativeFn {
        NativeFn {
            args: vec![],
            body: Box::new(|_scope, _args| {
                let value = HashMap::from([("current".into(), Value::Null.into())]);
                Ok(Value::Object(Rc::new(value)).into())
            }),
        }
    }

    fn no_op() -> NativeFn {
        NativeFn {
            args: vec![],
            body: Box::new(|_scope, _args| Ok(Value::Null.into())),
        }
    }
}

pub mod list {
    use std::rc::Rc;

    use crate::eval::{EvaluatedModule, FnValue, Value, ValueRef};

    use super::{expect_fn, expect_list, NativeFn};

    pub fn field(name: &str, list: &[ValueRef], value: &ValueRef) -> Option<ValueRef> {
        if let Some(prop) = find_property(name, list) {
            Some(prop.into())
        } else if let Some(mut found) = find_fn(name) {
            // Apply list as the first argument
            // NOTE: rhai calls found.args (curry): https://github.com/rhaiscript/rhai/blob/main/src/types/fn_ptr.rs#L25
            found.args.push(value.clone());
            Some(Value::Fn(Rc::new(FnValue::Native(found))).into())
        } else {
            None
        }
    }

    fn find_property(name: &str, list: &[ValueRef]) -> Option<Value> {
        match name {
            "length" => Some(Value::Num(list.len() as f64)),
            _ => None,
        }
    }

    fn find_fn(name: &str) -> Option<NativeFn> {
        match name {
            "map" => Some(map()),
            "some" => Some(some()),
            "filter" => Some(filter()),
            _ => None,
        }
    }

    pub fn map() -> NativeFn {
        NativeFn {
            args: vec![],
            body: Box::new(|scope, args| {
                let list = expect_list(args, 0, ".map")?;
                let f = expect_fn(args, 1, ".map")?;
                let mut res = vec![];

                for (i, el) in list.iter().enumerate() {
                    let cb_args = &[el.clone(), Value::Num(i as f64).into()];
                    let v = EvaluatedModule::do_call_fn(scope, &f, cb_args)?;
                    res.push(v);
                }

                Ok(Value::List(Rc::new(res)).into())
            }),
        }
    }

    pub fn some() -> NativeFn {
        NativeFn {
            args: vec![],
            body: Box::new(|scope, args| {
                let list = expect_list(args, 0, ".some")?;
                let predicate = expect_fn(args, 1, ".some")?;

                for el in list.iter() {
                    let v = EvaluatedModule::do_call_fn(scope, &predicate, &[el.clone()])?;
                    if v.0.borrow().as_bool() {
                        return Ok(Value::Bool(true).into());
                    }
                }

                Ok(Value::Bool(false).into())
            }),
        }
    }

    pub fn filter() -> NativeFn {
        NativeFn {
            args: vec![],
            body: Box::new(|scope, args| {
                let list = expect_list(args, 0, ".filter")?;
                let predicate = expect_fn(args, 1, ".filter")?;
                let mut res = vec![];

                for (i, el) in list.iter().enumerate() {
                    let cb_args = &[el.clone(), Value::Num(i as f64).into()];
                    let v = EvaluatedModule::do_call_fn(scope, &predicate, cb_args)?;
                    if v.0.borrow().as_bool() {
                        res.push(el.clone());
                    }
                }

                Ok(Value::List(Rc::new(res)).into())
            }),
        }
    }
}

pub struct NativeFn {
    pub args: Vec<ValueRef>,
    pub body: Box<dyn Fn(&Scope, &[ValueRef]) -> Result<ValueRef>>,
}

impl std::fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFn")
            .field("args", &self.args)
            .field("body", &"native-code")
            .finish()
    }
}

impl PartialEq for NativeFn {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

fn expect_list(args: &[ValueRef], index: usize, fn_name: &str) -> Result<Rc<Vec<ValueRef>>> {
    let val = args
        .get(index)
        .ok_or_else(|| anyhow!("Can't get {} argument", index + 1))?;
    match *val.0.borrow() {
        Value::List(ref l) => Ok(l.clone()),
        ref v => Err(anyhow!(
            "{} argument to '{fn_name}' must be list (actual={:?}).",
            get_label_for_index(index),
            v
        )),
    }
}

fn expect_fn(args: &[ValueRef], index: usize, fn_name: &str) -> Result<Rc<FnValue>> {
    let val = args
        .get(index)
        .ok_or_else(|| anyhow!("Can't get {} argument", index + 1))?;
    match &*val.0.borrow() {
        Value::Fn(f) => Ok(f.clone()),
        ref v => Err(anyhow!(
            "{} argument to '{fn_name}' must be function (actual={:?}).",
            get_label_for_index(index),
            v
        )),
    }
}

fn get_label_for_index(index: usize) -> &'static str {
    match index {
        0 => "first",
        1 => "second",
        3 => "third",
        _ => "an",
    }
}
