import {
  Add,
  Sub,
  Div,
  RequestAnimFrame,
  Mul,
  Gte,
  Mod,
  Lt,
  Lte,
  Eq,
  Update,
  SetArray,
  DrawCommandKindImage,
  DrawCommandKindClearCanvas,
  Rand,
  Floor,
  Or,
  And,
  Panic,
  Fill,
  Print,
  NewArray,
} from "./std";

type gridSize = 28;
type cellSize = 20;

type DrawCommand =
  | {
      type: 0;
      img: string;
      x: number;
      y: number;
      width: number;
      height: number;
    }
  | {
      type: 1;
      color: string;
    }
  | {
      type: 2;
      fillStyle: string;
      x: number;
      y: number;
      w: number;
      h: number;
    };

export type State = {
  grid: number[][];
  isDrawing: boolean;
  drawCommands: DrawCommand[];
  reset: boolean;
};

type ToggleCell<
  Grid extends number[][],
  Row extends number,
  Col extends number,
> = SetArray<Grid, SetArray<Grid[Row], 1, Col>, Row>;

type DrawGrid<
  Grid extends number[][],
  Row extends number,
  Col extends number,
  Acc extends DrawCommand[],
> = Row extends gridSize
  ? Acc
  : Col extends gridSize
  ? DrawGrid<Grid, Add<Row, 1>, 0, Acc>
  : Grid[Row] extends infer col extends number[]
  ? DrawGrid<
      Grid,
      Row,
      Add<Col, 1>,
      Grid[Row][Col] extends 1
        ? // ? [
          //     ...Acc,
          //     {
          //       type: 2;
          //       fillStyle: Grid[Row][Col] extends 1 ? "#000" : "#fff";
          //       x: Mul<Col, cellSize>;
          //       y: Mul<Row, cellSize>;
          //       w: cellSize;
          //       h: cellSize;
          //     },
          //   ]
          // : Acc
          Acc
        : Acc
    >
  : Acc;

type Draw<S extends State> = Update<
  S,
  {
    drawCommands: DrawGrid<S["grid"], 0, 0, [{ type: 1; color: "#fff" }]>;
    grid: Modify<S["grid"]>;
    isDrawing: false;
  }
>;

type MouseEvent = {
  top: number;
  right: number;
  bottom: number;
  left: number;

  client_x: number;
  client_y: number;
};

export type OnMouseDown<E extends MouseEvent, S extends State> = [
  Update<
    S,
    {
      grid: ToggleCell<
        S["isDrawing"] extends false
          ? NewArray<NewArray<0, gridSize>, gridSize>
          : S["grid"],
        // y = e.client_y - e.left
        // row = floor(y / cellSize)
        Floor<Div<Sub<E["client_y"], E["top"]>, cellSize>>,
        // x = e.client_x - e.top
        // col = floor(x / cellSize)
        Floor<Div<Sub<E["client_x"], E["left"]>, cellSize>>
      >;
      isDrawing: true;
    }
  >,
  false,
];

export type OnMouseMove<E extends MouseEvent, S extends State> = [
  S["isDrawing"] extends false
    ? S
    : Update<
        S,
        {
          grid: ToggleCell<
            S["grid"],
            // y = e.client_y - e.left
            // row = floor(y / cellSize)
            Floor<Div<Sub<E["client_y"], E["top"]>, cellSize>>,
            // x = e.client_x - e.top
            // col = floor(x / cellSize)
            Floor<Div<Sub<E["client_x"], E["left"]>, cellSize>>
          >;
        }
      >,
  false,
];

export type OnMouseUp<E extends MouseEvent, S extends State> = [
  Update<
    S,
    {
      isDrawing: false;
    }
  >,
  false,
];

export type Main<
  _Argv extends string[],
  S extends State = {
    grid: NewArray<NewArray<0, gridSize>, gridSize>;
    isDrawing: false;
    drawCommands: [];
    reset: false;
  },
> = RequestAnimFrame<Draw<S>>;
