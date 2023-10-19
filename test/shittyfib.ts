import {
  // prints a value to console
  Print,
  // addition
  Add,
  // subtraction
  Sub,
  // <=
  Lte,
} from "./std";

// Compute the Xth fibonnaci number
type Fib<X extends number> = Lte<X, 1> extends true
  ? X
  : Add<Fib<Sub<X, 1>>, Fib<Sub<X, 2>>>;

type Main<Args extends string[]> = Print<Fib<35>>;
