import { Print, AssertEq, Add, Sub, Lte, Eq, Update, Panic } from "./std";

type State = {
  foo: string;
  bar: number;
};

export type Main<Args extends string[]> = AssertEq<
  Print<Update<State, { baz: number }>>,
  { foo: string; bar: number; baz: number }
>;
