type Foo<X extends number> = X extends 0 ? `${X}: bad` : `${X}: good`;
type Bar<X extends { hi: string }> = { hi: X["hi"]; nice: "wow" };
type Baz<X extends object> = X;

export type Main<Args extends string[]> = Print<Baz<Bar<{ hi: Foo<420> }>>>;
