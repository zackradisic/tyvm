import { Print, Add, Sub, Lte, Eq, Panic, AssertEq } from "./std";

type Extends<A, B> = A extends B ? "extends" : "not extends";

export type Main<Args extends string[]> = AssertEq<
  Print<
    Extends<
      { bar: string },
      { foo: string } | { bar: string } | { baz: number }
    >
  >,
  "extends"
>;
