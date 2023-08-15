import { Print, Add, Sub, Lte, Eq } from "std";

type Fib<X extends number> = Lte<X, 1> extends true ? X : FibHelper<X, 2, 1, 0>;

type FibHelper<
  X extends number,
  I extends number,
  Prev extends number,
  PrevPrev extends number
> = Eq<X, I> extends true
  ? { result: Add<Prev, PrevPrev> }
  : { result: FibHelper<X, Add<I, 1>, Add<Prev, PrevPrev>, Prev> };

type Main = Fib<420>;
