import { Print, Add, Sub, Lte, Eq, Panic, AssertEq } from "./std";
export type Exp<A extends number, B extends number> = number;

export type Main<Arg extends string[]> = Print<Exp<5, 2>>;
