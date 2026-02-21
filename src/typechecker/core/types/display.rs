use crate::typechecker::core::types::Type;
use std::fmt::{Display, Formatter};

fn print_generics(args: &[Type], f: &mut Formatter<'_>) -> std::fmt::Result {
    if args.is_empty() {
        Ok(())
    } else {
        write!(f, "<")?;
        for (i, arg) in args.iter().enumerate() {
            write!(f, "{}", arg)?;
            if i != args.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ">")
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Error => write!(f, "Error"),
            Type::Infer(n) => write!(f, "?{}", n),
            Type::Metatype(name, generic_args) => {
                write!(f, "Type {}", name)?;
                print_generics(generic_args, f)
            }
            Type::GenericParam(name) => write!(f, "{}", name),
            Type::Number => write!(f, "number"),
            Type::Boolean => write!(f, "boolean"),
            Type::String => write!(f, "string"),
            Type::Void => write!(f, "void"),
            Type::Never => write!(f, "never"),
            Type::List(args) => {
                write!(f, "List")?;
                print_generics(args, f)
            }
            Type::Map(args) => {
                write!(f, "Map")?;
                print_generics(args, f)
            }
            Type::Function(function_type) => {
                write!(
                    f,
                    "func({}) -> {}",
                    function_type
                        .params
                        .iter()
                        .map(|t| format!("{}", t.1))
                        .collect::<Vec<_>>()
                        .join(", "),
                    function_type.return_type
                )
            }
            Type::Unknown => write!(f, "unknown"),
            Type::Any => write!(f, "any"),
            Type::Struct(name, generic_args) => {
                write!(f, "struct {}", name,)?;
                print_generics(generic_args, f)
            }
            Type::Interface(name) => write!(f, "interface {}", name),
            Type::Enum(name, generic_args) => {
                write!(f, "enum {}", name,)?;
                print_generics(generic_args, f)
            }
            Type::Optional(inner) => write!(f, "{}?", inner),
            Type::Nil => write!(f, "nil"),
            Type::Tuple(types) => {
                write!(
                    f,
                    "tuple({})",
                    types
                        .types
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}
