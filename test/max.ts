export type Max<A extends number, B extends number> = number;
export type Print<A extends number> = number;

export type Main<Arg extends string[]> = Print<Max<34.5, 2>>;
