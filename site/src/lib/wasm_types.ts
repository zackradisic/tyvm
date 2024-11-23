declare const __vmSym: unique symbol;
declare const __fnSym: unique symbol;

export type VMRef = number & { __vmSym: typeof __vmSym };
export type FnRef = number & { __fnSym: typeof __fnSym };
export type KeydownEvent = { code: string };
export type MouseEvent = {
  top: number;
  right: number;
  bottom: number;
  left: number;

  client_x: number;
  client_y: number;
};

export type Exports = {
  init(sourcePtr: number, len: number): VMRef;
  get_global_function(vm: VMRef): FnRef;
  get_function(vm: VMRef, namePtr: number, len: number): FnRef;
  run(vm: VMRef, fn: FnRef): void;
  alloc(size: number): number;
  dealloc(ptr: number, len: number): void;
  is_game(vm: VMRef): boolean;
  keydown(vm: VMRef, eventPtr: number, len: number): boolean;
  on_mouse_down(vm: VMRef, eventPtr: number, len: number): boolean;
  on_mouse_up(vm: VMRef, eventPtr: number, len: number): boolean;
  on_mouse_move(vm: VMRef, eventPtr: number, len: number): boolean;
};

export type DrawCommandKindImage = 0;
export type DrawCommandKindClearCanvas = 1;
export type DrawCommandKindFill = 2;
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
      color: string;
    }
  | {
      type: DrawCommandKindFill;
      fillStyle: string;
      x: number;
      y: number;
      w: number;
      h: number;
    };

const drawCmd: DrawCommand = {
  type: 0,
  img: "/bird.png",
  x: 500,
  y: 500,
  width: 60,
  height: 45,
};
