type Print<T> = never;

type Add<A extends number, B extends number> = number;
type Sub<A extends number, B extends number> = number;
type Lte<A extends number, B extends number> = boolean;
type Eq<A extends number, B extends number> = boolean;
type WriteFile<Path extends string, Content extends string> = never;
type ToTypescriptSource<Name extends string, T extends any> = never;
type Panic<Msg extends string> = never;
