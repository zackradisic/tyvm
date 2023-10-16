import { Print, Add, Sub, Mul, Lte, Eq, Panic } from "./std";

export type Main<Arg extends string[]> = Print<Mul<34.5, 2>>;
