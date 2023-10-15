// import { Print, Add, Sub, Lte, Eq } from "std";
import { Print, Add, Lte, Eq } from "../std";

type TestLet<Arg> = Arg extends infer val extends 0
  ? Add<val, 1> extends infer val extends 1
    ? Add<val, 1>
    : "we broke math"
  : "arg is not 0";

type Main<Argv extends string[]> = Print<TestLet<0>>;

type RecursiveType<T> = T extends string ? RecursiveType<string> : never;

// Usage of RecursiveType will cause an error because it leads to infinite recursion
type Example = RecursiveType<string>;

type ExtendsCheck<A, B> = A extends B ? "yes" : "no";

type t1 = ExtendsCheck<[], string[]>;
type t2 = ExtendsCheck<[], []>;
type t3 = ExtendsCheck<[], [string]>;

type t4 = ExtendsCheck<string | boolean, string | boolean | number>;
type t5 = ExtendsCheck<
  "NICE" | boolean | { lmao: boolean },
  string | boolean | number | { nice: string }
>;

type t6 = [1, 2, 3, 4]["length"];

type t7 = ExtendsCheck<number, object>;

type t8 = ExtendsCheck<{}, {}>;

// type t9 = ExtendsCheck<null, undefined>;

type Extends<A, B> = A extends B ? "extends" : "not extends";

type a = { foo: 1 | 2 };
type b = { foo: 1 } | { foo: 2 };

// extends
type result = Extends<a, b>;
