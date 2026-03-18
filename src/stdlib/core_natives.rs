use crate::stdlib::NativeDef;
use crate::typechecker::core::types::Type;
use crate::vm::value::{List, Value};

pub(super) fn natives() -> Vec<NativeDef> {
    vec![
        // Vararg — type must be specified here since Steel syntax doesn't support varargs.
        NativeDef {
            name: "println",
            type_: Some(Type::new_vararg(vec![Type::Any], Type::Void)),
            func: |args, _| {
                for arg in args {
                    print!("{}", arg);
                }
                println!();
                Ok(Value::Nil)
            },
        },
        NativeDef {
            name: "print",
            type_: Some(Type::new_vararg(vec![Type::Any], Type::Void)),
            func: |args, _| {
                for arg in args {
                    print!("{}", arg);
                }
                Ok(Value::Nil)
            },
        },
        // Non-vararg — types declared via `extern func` in core.steel
        NativeDef {
            name: "panic",
            type_: None,
            func: |args, _| Err(args[0].to_string()),
        },
        NativeDef {
            name: "assert",
            type_: None,
            func: |args, _| {
                if args[0] != args[1] {
                    panic!("Assertion failed: {} != {}", args[0], args[1]);
                }
                Ok(Value::Nil)
            },
        },
        NativeDef {
            name: "clock",
            type_: None,
            func: |_, _| {
                use std::time::{SystemTime, UNIX_EPOCH};
                let start = SystemTime::now();
                let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap();
                Ok(Value::Number(since_the_epoch.as_secs_f64()))
            },
        },
        NativeDef {
            name: "to_str",
            type_: None,
            func: |args, gc| {
                let s = args[0].to_string();
                Ok(Value::String(gc.alloc(s)))
            },
        },
        NativeDef {
            name: "is_nan",
            type_: None,
            func: |args, _| {
                let n = as_number(&args[0]);
                Ok(Value::Boolean(n.is_nan()))
            },
        },
        NativeDef {
            name: "readline",
            type_: None,
            func: |_, gc| {
                let mut buf = String::new();
                std::io::stdin()
                    .read_line(&mut buf)
                    .map_err(|e| e.to_string())?;
                // Strip trailing newline
                if buf.ends_with('\n') {
                    buf.pop();
                    if buf.ends_with('\r') {
                        buf.pop();
                    }
                }
                Ok(Value::String(gc.alloc(buf)))
            },
        },
        NativeDef {
            name: "range",
            type_: None,
            func: |args, gc| {
                let start = as_number(&args[0]) as i64;
                let end = as_number(&args[1]) as i64;
                let mut elements = Vec::with_capacity((end - start).max(0) as usize);
                let mut i = start;
                while i < end {
                    elements.push(Value::Number(i as f64));
                    i += 1;
                }
                Ok(Value::List(gc.alloc(List { vec: elements })))
            },
        },
    ]
}

fn as_number(val: &Value) -> f64 {
    match val {
        Value::Number(n) => *n,
        _ => unreachable!("expected number"),
    }
}
