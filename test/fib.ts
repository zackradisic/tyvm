import { Print, Add, Sub, Lte, Eq, ParseInt, Panic } from "./std";

type Fib<X extends number> = Lte<X, 1> extends true ? X : FibIter<X, 2, 1, 0>;
type FibIter<
  X extends number,
  I extends number,
  Prev extends number,
  PrevPrev extends number
> = X extends I
  ? Add<Prev, PrevPrev>
  : FibIter<X, Add<I, 1>, Add<Prev, PrevPrev>, Prev>;

export type Main<Argv extends string[]> = ParseInt<
  Argv[0]
> extends infer amount extends number
  ? WriteFile<
      "./test/fib-result.ts",
      ToTypescriptSource<"FibonacciResult", Fib<amount>>
    >
  : Panic<"invalid arguments">;
