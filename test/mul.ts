import { Print, Add, Sub, Mul, Lte, Eq, Panic, AssertEq } from "./std";

export type Main<Arg extends string[]> = AssertEq<Print<Mul<34.5, 2>>, 69>;
