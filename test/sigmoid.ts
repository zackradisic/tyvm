import { Print, Mul, Exp, Add, Div, AssertEq } from "./std";

type E = 2.718281828459045;

type Sigmoid<A extends number> = Div<
    1,
    Add<1, Exp<E, Mul<A, -1>>>
>;

type a = 5;

type Main<Args extends string[]> = AssertEq<
  Print<Sigmoid<a>>,
  0.9933071490757153
>;
