use crate::ir;

pub trait IrTransform<'output> {
    type InputAst<'a>;

    // TODO: Assumes the lifetimes are different (e.g. allocated using two different arenas), but not sure if that is good for perf.
    fn transform<'a>(&self, program: &'a Self::InputAst<'a>) -> ir::Program<'output>;
}
