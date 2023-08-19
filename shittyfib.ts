import { Print, Add, Sub, Lte, Eq } from "./std";

type Fib<X extends number> = Lte<X, 1> extends true
  ? X
  : Add<Fib<Sub<X, 1>>, Fib<Sub<X, 2>>>;

type Main = Print<Fib<35>>;
