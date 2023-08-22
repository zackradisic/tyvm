// import { Print, Add, Sub, Lte, Eq } from "std";
import { Print, Add, Lte, Eq } from "../std";

// type Main<Argv extends string[]> = Print<
//   Argv[0] extends infer val ? val : "yo wtf"
// >;

type TestLet<Arg> = Arg extends infer val extends 0
  ? Add<val, 1> extends infer val extends 1
    ? Add<val, 1>
    : "we broke math"
  : "arg is not 0";

// type TestLet<Arg> = Arg extends 0
//   ? Add<val, 1> extends infer val extends 1
//     ? "FUCK"
//     : "we broke math"
//   : "arg is not 0";

type Main<Argv extends string[]> = Print<TestLet<0>>;
