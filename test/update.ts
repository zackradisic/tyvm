import { Print, Add, Sub, Lte, Eq, Update, Panic } from "./std";

type State = {
  foo: string;
  bar: number;
};

export type Main<Args extends string[]> = Print<Update<State, { baz: number }>>;
