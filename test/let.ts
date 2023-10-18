import { Print, Add, Sub, Mul, Lte, Eq, Panic, AssertEq } from "./std";

type Foo<T extends { hi: any }> = T["hi"] extends infer Hi extends number
  ? Hi
  : "uh oh";

export type Main<Arg extends string[]> = AssertEq<Print<Foo<{ hi: 420 }>>, 420>;
