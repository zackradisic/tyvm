import { Print, AssertEq } from "./std";

type MyObj = {
  foo: number;
  bar: string;
};

type globalString = "lol";

type MyFn<
  B extends MyObj,
  A extends MyObj = { foo: 420; bar: globalString }
> = {
  foo: B["foo"];
  bar: A["bar"];
};

export type Main<Arg extends string[]> = AssertEq<
  MyFn<{ foo: 69; bar: "lmao" }>,
  { foo: 69; bar: "lol" }
>;
