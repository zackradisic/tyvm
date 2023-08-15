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

// type MakeString<S extends string> = `${S}hello`;
// type Eq<A extends number, B extends number> = true | false;
// type Add<A extends number, B extends number> = number;
// type Loop<
//   I extends number,
//   MaxIters extends number,
//   S extends string
// > = I extends MaxIters
//   ? S & { myVal: "hey" }
//   : Loop<Add<I, 1>, MaxIters, MakeString<S>>;

// type nice = Loop<0, 5, "NICE">;
