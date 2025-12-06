#[derive(Clone, Debug, PartialEq)]
pub enum ResolvedVar {
    Local(u8),
    Global(u16),
    Closure(u8),
}
