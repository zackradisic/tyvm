import { Print, Add, Sub, Lte, Eq, Panic, AssertEq } from "./std";

type Extends<A, B> = A extends B ? "extends" : "not extends";

export type Main<Args extends string[]> = { foo: "hi" } | { foo: "nice" };
