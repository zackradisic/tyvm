import { Print, Add, Sub, Lte, Eq, Panic, AssertEq } from "./std";

export type Main<Argv extends string[]> = AssertEq<Print<Add<1, 2>>, 3>;
