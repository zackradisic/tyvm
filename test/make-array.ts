import { Print, Add, Sub, Lte, Eq, Panic, AssertEq } from "./std";

type MakeArray<N extends number> = N extends 1 ? [N] : MakeArrayImpl<N, 1, []>;

type MakeArrayImpl<
  N extends number,
  I extends number,
  Acc extends number[]
> = N extends I ? Acc : MakeArrayImpl<N, Add<I, 1>, [...Acc, I]>;

type img<_> = MakeArrayImpl<5, 0, []>;

type Main<Args extends string[]> = Print<img<420>["length"]>;
