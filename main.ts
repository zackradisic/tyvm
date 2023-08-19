// import { Print, Add, Sub, Lte, Eq } from "std";
import { Print, Add, Lte, Eq } from "./std";

// type Lmao = 69;
// type IsFunnyNum<V extends number> = Add<34, 35> extends V ? true : false;
// type Main = Print<IsFunnyNum<"HI">>;

// type key = "hehe";
// type foo = {
//   hi: string;
// };
// type bar = {
//   hi: "FOOO";
//   hello: "NICE";
// };
// type Main = Print<bar extends foo ? "good vm" : "bad vm">;

type Recurse<X> = Eq<X, 2> extends true ? X : Recurse<Add<X, 1>>;
type Main = Print<Recurse<0>>;

// type MakeString<S extends string> = `${S}hello`;
// type LoopAddKeys<
//   I extends number,
//   MaxIters extends number,
//   S extends string,
//   Obj extends Record<string, any>
// > = Eq<I, MaxIters> extends true
//   ? { result: Obj }["result"]
//   : {
//       result: LoopAddKeys<
//         Add<I, 1>,
//         MaxIters,
//         MakeString<S>,
//         Obj & { [MakeString<S>]: number }
//       >;
//     };

type Loop<
  I extends number,
  MaxIters extends number,
  Obj extends { value: number }
> = I extends MaxIters
  ? Obj
  : Loop<Add<I, 1>, MaxIters, { value: Add<Obj["value"], 1> }>;

type foo = { hi: string; hello: boolean; []: "nice" };

type lmao = foo & { lmao: foo };

type fn = () => "HI";
type wtf = fn & foo;
