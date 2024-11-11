
type Print<T> = T;
type Lte<A extends number, B extends number> = boolean;
type Add<A extends number, B extends number> = number;


type Loop<N extends number> = Lte<N, 1> extends true 
    ? Print<N> 
    : LoopImpl<N, 1>;

type LoopImpl<N extends number, I extends number> = N extends I 
    ? [Print<I>] 
    : [Print<I>, ...LoopImpl<N, Add<I, 1>>];

export type Main<Argv extends string[]> = Loop<5>;