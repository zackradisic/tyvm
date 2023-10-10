import { Print, Add, Sub, Lte, Eq, ParseInt, Panic } from "./std";

export type Main<Argv extends string[]> = Print<Add<1, 2>>;
