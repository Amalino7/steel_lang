use crate::stdlib::NativeDef;
use crate::vm::value::Value;

pub(super) fn natives() -> Vec<NativeDef> {
    vec![
        NativeDef {
            name: "number.floor",
            type_: None,
            func: |args, _| Ok(Value::Number(as_number(&args[0]).floor())),
        },
        NativeDef {
            name: "number.ceil",
            type_: None,
            func: |args, _| Ok(Value::Number(as_number(&args[0]).ceil())),
        },
        NativeDef {
            name: "number.round",
            type_: None,
            func: |args, _| Ok(Value::Number(as_number(&args[0]).round())),
        },
        NativeDef {
            name: "number.trunc",
            type_: None,
            func: |args, _| Ok(Value::Number(as_number(&args[0]).trunc())),
        },
        NativeDef {
            name: "number.sqrt",
            type_: None,
            func: |args, _| Ok(Value::Number(as_number(&args[0]).sqrt())),
        },
        NativeDef {
            name: "number.log",
            type_: None,
            func: |args, _| Ok(Value::Number(as_number(&args[0]).ln())),
        },
        NativeDef {
            name: "number.log2",
            type_: None,
            func: |args, _| Ok(Value::Number(as_number(&args[0]).log2())),
        },
        NativeDef {
            name: "number.log10",
            type_: None,
            func: |args, _| Ok(Value::Number(as_number(&args[0]).log10())),
        },
        NativeDef {
            name: "number.sin",
            type_: None,
            func: |args, _| Ok(Value::Number(as_number(&args[0]).sin())),
        },
        NativeDef {
            name: "number.cos",
            type_: None,
            func: |args, _| Ok(Value::Number(as_number(&args[0]).cos())),
        },
        NativeDef {
            name: "number.tan",
            type_: None,
            func: |args, _| Ok(Value::Number(as_number(&args[0]).tan())),
        },
        NativeDef {
            name: "number.pow",
            type_: None,
            func: |args, _| {
                Ok(Value::Number(
                    as_number(&args[0]).powf(as_number(&args[1])),
                ))
            },
        },
        NativeDef {
            name: "number.min",
            type_: None,
            func: |args, _| {
                Ok(Value::Number(
                    as_number(&args[0]).min(as_number(&args[1])),
                ))
            },
        },
        NativeDef {
            name: "number.max",
            type_: None,
            func: |args, _| {
                Ok(Value::Number(
                    as_number(&args[0]).max(as_number(&args[1])),
                ))
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
