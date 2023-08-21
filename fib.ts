import { Print, Add, Sub, Lte, Eq } from "./std";

// type FibHelper<
//   X extends number,
//   I extends number,
//   Prev extends number,
//   PrevPrev extends number
// > = Eq<X, I> extends true
//   ? Add<Prev, PrevPrev>
//   : FibHelper<X, Add<I, 1>, Add<Prev, PrevPrev>, Prev>;
// type Fib<X extends number> = Lte<X, 1> extends true ? X : FibHelper<X, 2, 1, 0>;

type Fib<X extends number> = Lte<X, 1> extends true ? X : FibIter<X, 2, 1, 0>;
type FibIter<
  X extends number,
  I extends number,
  Prev extends number,
  PrevPrev extends number
> = X extends I
  ? Add<Prev, PrevPrev>
  : FibIter<X, Add<I, 1>, Add<Prev, PrevPrev>, Prev>;

type Main = WriteFile<
  "./fib-result.ts",
  ToTypescriptSource<"FibonacciResult", Fib<amount>>
>;

type amount = 40;
// type myobj = {
//   hi: string;
//   hello: string;
//   amount: 40;
// };
