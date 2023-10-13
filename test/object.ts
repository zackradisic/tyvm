type Foo<X extends number> = X extends 0 ? `${X}: bad` : `${X}: good`;

export type Main<Args extends string[]> = Print<{ hi: Foo<420> }>;
