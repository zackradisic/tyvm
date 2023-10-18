import { Print, Add, Sub, Lte, Eq, Panic, AssertEq } from "./std";

type HeadTail<Arr extends string[], Acc extends string[]> = Arr extends []
  ? Acc
  : Arr extends [infer First extends string, ...infer Rest extends string[]]
  ? HeadTail<Rest, [`${First} LOL`, ...Acc]>
  : Acc;

type lol = HeadTail<["hi", "hello", "nice"], []>;

export type Main<_Argv extends string[]> = AssertEq<
  Print<HeadTail<["hi", "hello", "nice"], []>>,
  ["nice LOL", "hello LOL", "hi LOL"]
>;
