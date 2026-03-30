use crate::stdlib::NativeDef;
use crate::vm::value::{List, Value};

pub(super) fn natives() -> Vec<NativeDef> {
    vec![
        NativeDef {
            name: "string.length",
            type_: None,
            func: |args, _| Ok(Value::Number(as_str(&args[0]).len() as f64)),
        },
        NativeDef {
            name: "string.split",
            type_: None,
            func: |args, gc| {
                let s = as_str(&args[0]);
                let sep = as_str(&args[1]);
                let parts: Vec<Value> = s
                    .split(sep.as_str())
                    .map(|part| Value::String(gc.alloc(part.to_string())))
                    .collect();
                Ok(Value::List(gc.alloc(List { vec: parts })))
            },
        },
        NativeDef {
            name: "string.trim",
            type_: None,
            func: |args, gc| {
                let result = as_str(&args[0]).trim().to_string();
                Ok(Value::String(gc.alloc(result)))
            },
        },
        NativeDef {
            name: "string.trim_start",
            type_: None,
            func: |args, gc| {
                let result = as_str(&args[0]).trim_start().to_string();
                Ok(Value::String(gc.alloc(result)))
            },
        },
        NativeDef {
            name: "string.trim_end",
            type_: None,
            func: |args, gc| {
                let result = as_str(&args[0]).trim_end().to_string();
                Ok(Value::String(gc.alloc(result)))
            },
        },
        NativeDef {
            name: "string.to_upper",
            type_: None,
            func: |args, gc| {
                let result = as_str(&args[0]).to_uppercase();
                Ok(Value::String(gc.alloc(result)))
            },
        },
        NativeDef {
            name: "string.to_lower",
            type_: None,
            func: |args, gc| {
                let result = as_str(&args[0]).to_lowercase();
                Ok(Value::String(gc.alloc(result)))
            },
        },
        NativeDef {
            name: "string.starts_with",
            type_: None,
            func: |args, _| {
                let s = as_str(&args[0]);
                let prefix = as_str(&args[1]);
                Ok(Value::Boolean(s.starts_with(prefix.as_str())))
            },
        },
        NativeDef {
            name: "string.ends_with",
            type_: None,
            func: |args, _| {
                let s = as_str(&args[0]);
                let suffix = as_str(&args[1]);
                Ok(Value::Boolean(s.ends_with(suffix.as_str())))
            },
        },
        NativeDef {
            name: "string.contains",
            type_: None,
            func: |args, _| {
                let s = as_str(&args[0]);
                let substr = as_str(&args[1]);
                Ok(Value::Boolean(s.contains(substr.as_str())))
            },
        },
        NativeDef {
            name: "string.replace",
            type_: None,
            func: |args, gc| {
                let s = as_str(&args[0]);
                let from = as_str(&args[1]);
                let to = as_str(&args[2]);
                let result = s.replace(from.as_str(), to.as_str());
                Ok(Value::String(gc.alloc(result)))
            },
        },
        NativeDef {
            name: "string.substring",
            type_: None,
            func: |args, gc| {
                let s = as_str(&args[0]);
                let start = as_number(&args[1]) as usize;
                let end = as_number(&args[2]) as usize;
                let end = end.min(s.len());
                let start = start.min(end);
                let result = s[start..end].to_string();
                Ok(Value::String(gc.alloc(result)))
            },
        },
        NativeDef {
            name: "string.index_of",
            type_: None,
            func: |args, _| {
                let s = as_str(&args[0]);
                let substr = as_str(&args[1]);
                match s.find(substr.as_str()) {
                    Some(idx) => Ok(Value::Number(idx as f64)),
                    None => Ok(Value::Nil),
                }
            },
        },
        NativeDef {
            name: "string.chars",
            type_: None,
            func: |args, gc| {
                let s = as_str(&args[0]);
                let chars: Vec<Value> = s
                    .chars()
                    .map(|c| Value::String(gc.alloc(c.to_string())))
                    .collect();
                Ok(Value::List(gc.alloc(List { vec: chars })))
            },
        },
        NativeDef {
            name: "string.parse_number",
            type_: None,
            func: |args, _| {
                let s = as_str(&args[0]);
                match s.parse::<f64>() {
                    Ok(n) => Ok(Value::Number(n)),
                    Err(_) => Ok(Value::Nil),
                }
            },
        },
    ]
}

fn as_str(val: &Value) -> String {
    match val {
        Value::String(s) => s.as_str().to_string(),
        _ => unreachable!("expected string"),
    }
}

fn as_number(val: &Value) -> f64 {
    match val {
        Value::Number(n) => *n,
        _ => unreachable!("expected number"),
    }
}
