import { Print, Add, Sub, Lte, Eq, Panic, AssertEq } from "./std";

type FillArray<
  Count extends number,
  Value,
  Array extends any[]
> = FillArrayImpl<Count, 0, Value, Array>;

type FillArrayImpl<
  Count extends number,
  I extends number,
  Value,
  Array extends any[]
> = I extends Count
  ? Array
  : FillArrayImpl<Count, Add<I, 1>, Value, [Value, ...Array]>;

export type Main<Args extends string[]> = AssertEq<
  Print<FillArray<4, "LOL", []>>,
  ["LOL", "LOL", "LOL", "LOL"]
>;
