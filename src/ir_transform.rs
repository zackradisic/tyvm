pub trait IrTransform<'input, 'output> {
    type InputAst<'alloc>;
    type OutputAst<'allo>;
}
