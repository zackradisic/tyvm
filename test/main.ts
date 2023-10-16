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
type b = { foo: 1 } | { foo: 2 } | { foo: 1 };

namespace objectUnionNormalizatoin {
  type a = { foo: 1 | 2 };
  type b = { foo: 1 } | { foo: 2 };

  // a | b
  type c = { foo: 1 | 2 } | ({ foo: 1 } | { foo: 2 });

  type d = { foo: string } | { foo: number };

  type e = { foo: string; bar: number } | { foo: string; bar: number };

  type foo = Exclude<{ foo: "bar" }, { foo: "baz" }>;
}

namespace objectUnionExtends {
  type a = { foo: 1 | 2 };
  type b = { foo: 1 } | { foo: 2 } | { bar: "test" };

  // Usually the algorithm goes:
  // ```
  // For each field in a
  // Check if the key is present in b
  // Check if the value in a subtypes the value in b
  // ```
  // But this is a counterexample to that:
  // 1 | 2 DOES NOT extend 1, and
  // 1 | 2 DOES NOT extend 2
  //
  // Which makes me think that the algorithm should be:
  // ```
  // For each field in a
  // Check if the key is present in b
  // If the value in a is NOT a union:
  //   Check if the value in a subtypes the value in b
  // If the value in a IS a union:
  //   Check that each variant subtypes at least one in b
  // ```
  type r1 = Extends<a, b>;

  // But unfortunately that algorithm does not work,
  // a counter example:
  type c = { foo: 1 | 2; bar: "hi" | "hello" };
  type d = { foo: 1 } | { foo: 2 } | { bar: "hi" } | { bar: "hello" };
  type r2 = Extends<c, d>;

  type e = { foo: 1 | 2 } | { bar: "hi" | "hello" };

  type r3 = Extends<c, e>;

  type f =
    | { foo: 1 }
    | { foo: 2 }
    | { bar: "hi" }
    | { bar: "hello" }
    | { lmao: "hi" };
  type r4 = Extends<{ foo: 1 | 2; bar: "hi" | "hello"; lmao: "hi" }, f>;

  type g = { foo: 1 | 2; bar: "hi" | "hello"; lmao: "hi" };
  type r5 = Extends<{ foo: 1 | 2; bar: "hi" | "hello"; lmao: "hi" }, g>;

  type h =
    | { foo: 1 }
    | { foo: 2; nice: "hi" }
    | { bar: "hi" }
    | { bar: "hello" }
    | { lmao: "hi" };

  type r6 = Extends<{ foo: 1 | 2; bar: "hi" | "hello"; lmao: "hi" }, g>;

  // type Shape = |
  // { type: 'circle', radius: number } |
  // { type: 'square', side: number } |
  // { type: 'rectangle', side: string, height: number }

  type Shape = { type: "circle" } | { type: "square" } | { type: "rectangle" };

  type r7 = Extends<{ type: "circle" | "square" }, Shape>;
  type r8 = Extends<
    { type: "circle" | "square" },
    { type: "circle" | "square" | "rectangle" }
  >;

  type r9 = Extends<{ type: "circle" | "square" }, Shape>;
  type r10 = Extends<
    { type: 0 | 1 | 2; foo: string },
    { type: 0 } | { type: 1 } | { type: 2 }
  >;
}

// extends
type result = Extends<a, b>;

type a1 = string | number;
type a2 = string | number | boolean;

type a3 = a1 | a2;

type circle = { type: "circle"; radius: number };
type rectangle = { type: "rectangle"; w: number; h: number };
type _shape =
  | { type: "circle"; radius: number }
  | { type: "rectangle"; w: number; h: number };
type shape = circle | rectangle;

// circle = { type * 'circle' }
// rectangle = { type * 'rectangle' }
// shape = circle + rectangle
//       = { type * 'circle' } + { type * 'rectangle' }
//       = { type * ('circle' + 'rectangle') }

// circle = { type * 'circle' } * { radius * number }
// rectangle = { type * 'rectangle' } * { w * number } * { h * number }
// shape = circle + rectangle
//       = { type * 'circle' } * { radius * number } + { type * 'rectangle' } * { w * number } * { h * number }
//
// A = type, B = 'circle', C = 'rectangle', D = radius, E = w, F = h, G = number
//       = A * B * D * G + A * C * E * G * F * G
//       = ABDG + ACEGFG
//       = A(BD + CEGFG)
//
//       = { type } * ('circle' * radius * number + 'rectangle' * w * number * h * number)
type shape1 = {
  type: "circle" | "rectangle";
  radius: number;
  w: number;
  h: number;
};
type result2 = Extends<shape1, shape>;

type shape2 = {
  type: "circle" | "rectangle";
  radius: 120;
  w: number;
  h: number;
};
type result3 = Extends<shape2, shape>;

type shape3 = {
  type: "circle" | "rectangle";
  radius: 120;
  w: number;
  h: number;
  thisDoesntBreakIt: boolean;
};
type result4 = Extends<shape3, shape>;

// brekas it cause missing `h`
type shape4 = { type: "circle" | "rectangle"; radius: 120; w: number };
type result5 = Extends<shape4, shape>;

type result6 = Extends<{ type: "circle"; w: number }, shape>;

type square = { side: number };
type triangle = { side1: number; side2: number; side3: number };
type squaretriangle = square | triangle;
type squaretriangle2 =
  | { side: number }
  | { side1: number; side2: number; side3: number };

type result7 = Extends<{ side1: number }, squaretriangle2>;

// HERE IS THE KEY INSIGHT, IT ONLY WORKS ON DISCRIMINATED UNIONS!
// Discriminated union is a union of more than 1 object that share same key, and that key's value must be a literal.
// `heehaw` is not a discriminated union, so doesn't work
type heehaw = { type: string; foo: "lol" } | { type: number; bar: "baz" };
type result8 = Extends<
  { type: string | number; foo: "lol"; bar: "baz" },
  heehaw
>;

// Interestingly, a discriminated union can only have one discriminant. So none of these work:
type nicenice = { foo: "lol"; bar: "hehe" } | { foo: "wow"; bar: "nice" };
type result9 = Extends<{ foo: "lol" | "wow"; bar: "hehe" | "nice" }, nicenice>;
type result10 = Extends<{ foo: "lol" | "wow"; bar: "hehe" | "nice" }, nicenice>;

type discriminatlsdkjflksd = { foo: Array<"hi"> } | { foo: Array<string> };

type discriminatedUnionEdgeCase =
  | { foo: "lol"; hi: number }
  | { foo: "bar" }
  | { foo: string };

// normalization works as expected, they all get flattened to `{ foo: string }`
function someFunc(x: discriminatedUnionEdgeCase) {
  if (x.foo === "lol") {
    // @ts-expect-error
    x.hi;
  }
}

type discriminatedUnionEdgeCase2 =
  | { foo: "lol" }
  | { foo: "bar" }
  | { hi: string };

// `x` has no keys
function someFunc2(x: discriminatedUnionEdgeCase2) {
  // this is `never
  type lmao = keyof typeof x;
}

type discriminatedUnionEdgeCase3 =
  | { foo: "lol"; charAt(pos: number): string }
  | { foo: "bar"; charAt(pos: number): string }
  | string;

// `x` has no keys
function someFunc3(x: discriminatedUnionEdgeCase2) {
  // this is `never`
  type lmao = keyof typeof x;
}

type discriminatedUnionEdgeCase4 =
  | { foo: "lol"; hello: string }
  | { foo: "lol"; hello: number }
  // ^ The above two get normalized as expected
  | { foo: "bar" };

function someFunc4(x: discriminatedUnionEdgeCase4) {}
