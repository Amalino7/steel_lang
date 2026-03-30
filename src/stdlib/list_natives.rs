use crate::stdlib::NativeDef;
use crate::vm::value::{List, Value};

pub(super) fn natives() -> Vec<NativeDef> {
    vec![
        NativeDef {
            name: "List.len",
            type_: None,
            func: |args, _| {
                let list = as_list(&args[0]);
                Ok(Value::Number(list.vec.len() as f64))
            },
        },
        NativeDef {
            name: "List.is_empty",
            type_: None,
            func: |args, _| {
                let list = as_list(&args[0]);
                Ok(Value::Boolean(list.vec.is_empty()))
            },
        },
        NativeDef {
            name: "List.push",
            type_: None,
            func: |args, _| match args[0] {
                Value::List(mut list) => unsafe {
                    list.deref_mut().vec.push(args[1]);
                    Ok(Value::Nil)
                },
                _ => unreachable!("expected list"),
            },
        },
        NativeDef {
            name: "List.pop",
            type_: None,
            func: |args, _| match args[0] {
                Value::List(mut list) => unsafe {
                    Ok(list.deref_mut().vec.pop().unwrap_or(Value::Nil))
                },
                _ => unreachable!("expected list"),
            },
        },
        NativeDef {
            name: "List.slice",
            type_: None,
            func: |args, gc| {
                let list = as_list(&args[0]);
                let start = as_number(&args[1]) as usize;
                let end = as_number(&args[2]) as usize;
                let end = end.min(list.vec.len());
                let start = start.min(end);
                let elements = list.vec[start..end].to_vec();
                Ok(Value::List(gc.alloc(List { vec: elements })))
            },
        },
        NativeDef {
            name: "List.concat",
            type_: None,
            func: |args, gc| {
                let left = as_list(&args[0]);
                let right = as_list(&args[1]);
                let mut elements = Vec::with_capacity(left.vec.len() + right.vec.len());
                elements.extend_from_slice(&left.vec);
                elements.extend_from_slice(&right.vec);
                Ok(Value::List(gc.alloc(List { vec: elements })))
            },
        },
        NativeDef {
            name: "List.join",
            type_: None,
            func: |args, gc| {
                let list = as_list(&args[0]);
                let sep = as_str(&args[1]);
                let parts: Vec<String> = list
                    .vec
                    .iter()
                    .map(|v| match v {
                        Value::String(s) => s.as_str().to_string(),
                        _ => unreachable!("expected list of strings"),
                    })
                    .collect();
                let result = parts.join(sep.as_str());
                Ok(Value::String(gc.alloc(result)))
            },
        },
    ]
}

fn as_list(val: &Value) -> &List {
    match val {
        Value::List(list) => list,
        _ => unreachable!("expected list"),
    }
}

fn as_number(val: &Value) -> f64 {
    match val {
        Value::Number(n) => *n,
        _ => unreachable!("expected number"),
    }
}

fn as_str(val: &Value) -> String {
    match val {
        Value::String(s) => s.as_str().to_string(),
        _ => unreachable!("expected string"),
    }
}
