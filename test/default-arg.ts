import { Print, AssertEq } from "./std";

type MyObj = {
  foo: number;
  bar: string;
};

type MyFn<B extends MyObj, A extends MyObj = { foo: 420; bar: "lol" }> = {
  foo: B["foo"];
  bar: A["bar"];
};

export type Main<Arg extends string[]> = AssertEq<
  MyFn<{ foo: 69; bar: "lmao" }>,
  { foo: 69; bar: "lol" }
>;
