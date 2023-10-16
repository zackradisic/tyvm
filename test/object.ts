import { Print, AssertEq } from "./std";

type Foo<X extends number> = X extends 0 ? `${X}: bad` : `${X}: good`;
type Bar<X extends { hi: string }> = { hi: X["hi"]; nice: "wow" };
type Baz<X extends object> = X;

export type Main<Args extends string[]> = AssertEq<
  Print<Baz<Bar<{ hi: Foo<420> }>>>,
  { hi: "420: good"; nice: "wow" }
>;
