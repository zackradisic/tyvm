declare const __vmSym: unique symbol;
declare const __fnSym: unique symbol;

export type VMRef = number & { __vmSym: typeof __vmSym };
export type FnRef = number & { __fnSym: typeof __fnSym };

export type Exports = {
  init(sourcePtr: number, len: number): VMRef;
  get_global_function(vm: VMRef): FnRef;
  get_function(vm: VMRef, namePtr: number, len: number): FnRef;
  run(vm: VMRef, fn: FnRef): void;
  alloc(size: number): number;
  dealloc(ptr: number, len: number): void;
  is_game(vm: VMRef): boolean;
  jump(vm: VMRef): void;
};

export type DrawCommandKindImage = 0;
export type DrawCommandKindClearCanvas = 1;
export type DrawCommand =
  | {
      type: DrawCommandKindImage;
      img: string;
      x: number;
      y: number;
      width: number;
      height: number;
    }
  | {
      type: DrawCommandKindClearCanvas;
      x: number;
      y: number;
      width: number;
      height: number;
    };

const drawCmd: DrawCommand = {
  type: 0,
  img: "/bird.png",
  x: 500,
  y: 500,
  width: 60,
  height: 45,
};
