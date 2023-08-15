import { Print, Add, Sub, Lte, Eq } from "./std";

type FibHelper<
  X extends number,
  I extends number,
  Prev extends number,
  PrevPrev extends number
> = Eq<X, I> extends true
  ? Add<Prev, PrevPrev>
  : FibHelper<X, Add<I, 1>, Add<Prev, PrevPrev>, Prev>;

type Fib<X extends number> = Lte<X, 1> extends true ? X : FibHelper<X, 2, 1, 0>;

type Main = Print<Fib<50>>;
