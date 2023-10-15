export type Print<T> = never;

export type Add<A extends number, B extends number> = number;
export type Sub<A extends number, B extends number> = number;
export type Mul<A extends number, B extends number> = number;
export type Div<A extends number, B extends number> = number;
export type Lte<A extends number, B extends number> = boolean;
export type Eq<A extends number, B extends number> = boolean;
export type WriteFile<Path extends string, Content extends string> = never;
export type ToTypescriptSource<Name extends string, T extends any> = never;
export type Panic<Msg extends string> = never;
export type RequestAnimFrame<
  State extends { drawCommands: Array<DrawCommand> }
> = void;
// export type Update<F, A extends F, B extends F> = Omit<A, keyof B> & B;
// export type Update<A extends any, B extends A> = Omit<A, keyof B> & B;
export type Update<A extends object, B extends object> = Omit<A, keyof B> & B;
export type DrawCommand =
  | {
      type: "DrawImage";
      img: string;
      x: number;
      y: number;
      width: number;
      height: number;
    }
  | {
      type: "ClearCanvas";
      x: number;
      y: number;
      width: number;
      height: number;
    };
