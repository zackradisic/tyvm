import { Print, Add, Sub, Lte, Eq, Panic, AssertEq } from "./std";

type Arr1 = [0, 1, 2, 3];
type Arr2 = [7, 8, 9];

export type Main<Args extends string[]> = AssertEq<
  Print<[...Arr1, 4, 5, 6, ...Arr2]>,
  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
>;
