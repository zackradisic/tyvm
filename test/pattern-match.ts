type arrayPatternMatch<T> = T extends [infer First, ...infer Rest]
  ? First
  : never;

// type isLength4<T extends Array<any>> = T['length'] extends 4 ? "hi" : never

// type arrayPatternMatch<T> = T extends [
//   infer First extends isL
//   ...infer Rest
// ]
//   ? First
//   : never;
