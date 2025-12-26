use crate::typechecker::type_ast::{BinaryOp, ExprKind, LogicalOp, TypedExpr, UnaryOp};
use crate::typechecker::types::Type;
use crate::typechecker::{Symbol, TypeChecker};

type Refinement = (Symbol, Type);

pub(crate) struct BranchRefinements {
    pub(crate) true_path: Vec<Refinement>,
    pub(crate) false_path: Vec<Refinement>,
}

impl<'src> TypeChecker<'src> {
    pub(crate) fn analyze_condition(&mut self, expr: &TypedExpr) -> BranchRefinements {
        match &expr.kind {
            // x == nil
            ExprKind::Binary {
                left,
                operator,
                right,
            } if operator == &BinaryOp::EqualEqual => {
                let other = if right.ty == Type::Nil {
                    left
                } else if left.ty == Type::Nil {
                    right
                } else {
                    return BranchRefinements {
                        true_path: vec![],
                        false_path: vec![],
                    };
                };

                if let ExprKind::GetVar(_, name) = &other.kind {
                    if let Type::Optional(inner) = &other.ty {
                        return BranchRefinements {
                            true_path: vec![],
                            false_path: vec![(name.clone(), *inner.clone())],
                        };
                    }
                }
                BranchRefinements {
                    true_path: vec![],
                    false_path: vec![],
                }
            }
            ExprKind::Is {
                target,
                variant_idx,
                narrowed,
            } => {
                if let ExprKind::GetVar(_, name) = &target.kind
                    && let Type::Enum(enum_name) = &target.ty
                {
                    let enum_def = self.sys.get_enum(enum_name).unwrap();
                    let false_path = if enum_def.variants.len() == 2 {
                        let other = enum_def
                            .variants
                            .iter()
                            .find(|&e| (*e.1).0 != *variant_idx as usize)
                            .unwrap();
                        vec![(name.clone(), (*other.1).1.clone())] // TODO fix this mess
                    } else {
                        vec![]
                    };

                    return BranchRefinements {
                        true_path: vec![(name.clone(), narrowed.clone())],
                        false_path,
                    };
                }
                BranchRefinements {
                    true_path: vec![],
                    false_path: vec![],
                }
            }
            // Case: !Condition
            ExprKind::Unary { operator, operand } if operator == &UnaryOp::Not => {
                let inner = self.analyze_condition(operand);
                // Swap the branches
                BranchRefinements {
                    true_path: inner.false_path,
                    false_path: inner.true_path,
                }
            }
            ExprKind::Logical {
                left,
                operator,
                right,
                ..
            } if operator == &LogicalOp::And => {
                let refine_left = self.analyze_condition(left);
                let refine_right = self.analyze_condition(right);
                BranchRefinements {
                    true_path: [refine_left.true_path, refine_right.true_path].concat(),
                    false_path: vec![],
                }
            }
            _ => BranchRefinements {
                true_path: vec![],
                false_path: vec![],
            },
        }
    }
}
