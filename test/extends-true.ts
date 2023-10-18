import { AssertEq, Print, Add, Sub, Lte, Eq } from "./std";

export type Main<_Args extends string[]> = AssertEq<
  Print<Eq<Add<1, 2>, 3> extends true ? "Nice" : "Not nice">,
  "Nice"
>;
