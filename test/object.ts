type Foo<X extends number> = X extends 0 ? `${X}: bad` : `${X}: good`;
type Bar<X extends { hi: string }> = { hi: X["hi"]; nice: "wow" };

export type Main<Args extends string[]> = Print<Bar<{ hi: Foo<420> }>>;
