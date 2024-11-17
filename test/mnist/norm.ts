// Helper Types
export type Print<T> = T;
export type Add<A extends number, B extends number> = number;
export type Mul<A extends number, B extends number> = number;
export type Div<A extends number, B extends number> = number;
export type Exp<A extends number, B extends number> = number;

type Sqrt<N extends number> = Exp<N, Div<1, 2>>;

type SquareSumArray<Arr extends number[]> = SquareSumArrayImpl<Arr, 0, 0>;

type SquareSumArrayImpl<Arr extends number[], I extends number, Sum extends number> = I extends Arr['length']
    ? Sum
    : SquareSumArrayImpl<Arr, Add<I, 1>, Add<Sum, Exp<Arr[I], 2>>>;

type L2Norm<Vec extends number[]> = Sqrt<SquareSumArray<Vec>>;

type Normalize<Arr extends number[]> = NormalizeImpl<Arr, L2Norm<Arr>, 0, []>;

type NormalizeImpl<Arr extends number[], Norm extends number, I extends number, Result extends number[]> = I extends Arr['length']
    ? Result
    : NormalizeImpl<Arr, Norm, Add<I, 1>, [...Result, Div<Arr[I], Norm>]>;

type test = Normalize<[1, 2, 3]>;
type Main<Args extends string[]> = Print<test>;
