import { Print, Add, Sub, Lte, Eq, Panic, AssertEq } from "./std";

export type Main<Argv extends string[]> = AssertEq<
  Print<`Value is ${Add<400, 20>} ${"wow"} yay`>,
  "Value is 420 wow yay"
>;
