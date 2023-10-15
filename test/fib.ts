import { Print, Add, Sub, Lte, Eq, ParseInt, Panic } from "./std";

/**
 * Compute the Nth fibonacci number
 **/
type Fib<N extends number> = Lte<N, 1> extends true ? N : FibIter<N, 2, 1, 0>;
/**
 * Comuptes the Nth fibonacci using iterative recursion
 **/
type FibIter<
  N extends number,
  I extends number,
  NminusOne extends number,
  NminusTwo extends number
> = N extends I
  ? Add<NminusOne, NminusTwo>
  : FibIter<N, Add<I, 1>, Add<NminusOne, NminusTwo>, NminusOne>;

export type Main<Argv extends string[]> =
  Print<`Value is ${Fib<10>}${"wow"} yay`>;

// export type Main<Argv extends string[]> = ParseInt<
//   Argv[0]
// > extends infer amount extends number
//   ? WriteFile<
//       "./test/fib-result.ts",
//       ToTypescriptSource<"FibonacciResult", Fib<amount>>
//     >
//   : Panic<"invalid arguments">;
