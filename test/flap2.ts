import {
  Add,
  Sub,
  Div,
  RequestAnimFrame,
  Mul,
  Gte,
  Lt,
  Eq,
  Update,
  DrawCommandKindImage,
  DrawCommandKindClearCanvas,
  Rand,
  Floor,
} from "./std";

type Gravity = 1;
type Jump = -15;
type canvasHeight = 600;
type canvasWidth = 800;
type birdWidth = 60;
type birdHeight = 40;
type gameoverHeight = 160;
type gameoverWidth = 300;
type pipeWidth = 60;
type pipeHeight = 250;
type pipeGap = 200;
type pipesAmount = Floor<Div<canvasWidth, Add<pipeWidth, pipeGap>>>;

type Pipe = { x: number; y: number };

type MakeDefaultPipes<
  Amount extends number,
  Acc extends Pipe[],
  X extends number
> = Lt<Acc["length"], Amount> extends true
  ? MakeDefaultPipes<
      Amount,
      [...Acc, { x: X; y: Sub<Floor<Rand<0, pipeHeight>>, pipeHeight> }],
      Add<X, pipeGap>
    >
  : Acc;

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
      x: number;
      y: number;
      width: number;
      height: number;
    };

type GameState = {
  birdY: number;
  velocity: number;
  pipes: Array<Pipe>;
  drawCommands: Array<DrawCommand>;
  jumpInput: boolean;
  isCollided: boolean;
};

type PossiblyResetGame<S extends GameState> = S["isCollided"] extends true
  ? S["jumpInput"] extends true
    ? Update<
        S,
        {
          birdY: Div<canvasHeight, 2>;
          velocity: 0;
          isCollided: false;
          pipes: [{ x: canvasWidth; y: 0 }];
        }
      >
    : S
  : S;

type ApplyJump<S extends GameState> = Update<
  S,
  {
    velocity: S["jumpInput"] extends true ? Jump : S["velocity"];
    jumpInput: false;
  }
>;

type ApplyGravity<S extends GameState> = Update<
  S,
  {
    birdY: Add<S["birdY"], Mul<S["velocity"], 0.5>>;
    velocity: Add<S["velocity"], Mul<Gravity, 0.5>>;
  }
>;

type CheckCollision<S extends GameState> = Gte<
  Add<S["birdY"], birdHeight>,
  canvasHeight
> extends true
  ? Update<
      S,
      {
        isCollided: true;
      }
    >
  : S;

type NextPipeIdx<I extends number, Len extends number> = Eq<
  Add<I, 1>,
  Len
> extends true
  ? 0
  : Add<I, 1>;

type MovePipesImpl<
  Pipes extends Array<Pipe>,
  I extends number,
  Acc extends Array<Pipe>
> = Lt<I, Pipes["length"]> extends true
  ? Pipes[I] extends infer P extends Pipe
    ? Lt<Add<Sub<P["x"], 1>, pipeWidth>, 0> extends true
      ? Pipes[NextPipeIdx<I, Pipes["length"]>] extends infer Next extends Pipe
        ? MovePipesImpl<
            Pipes,
            Add<I, 1>,
            [
              ...Acc,
              {
                x: Add<Add<Next["x"], pipeWidth>, pipeGap>;
                y: Sub<Floor<Rand<0, pipeHeight>>, pipeHeight>;
              }
            ]
          >
        : any
      : MovePipesImpl<
          Pipes,
          Add<I, 1>,
          [...Acc, { x: Sub<P["x"], 1>; y: P["y"] }]
        >
    : any
  : Acc;

type UpdatePipes<S extends GameState> = Update<
  S,
  { pipes: MovePipesImpl<S["pipes"], 0, []> }
>;

type DrawPipes<
  I extends number,
  S extends Pipe[],
  Acc extends DrawCommand[]
> = Lt<I, S["length"]> extends true
  ? S[I] extends infer P extends Pipe
    ? [
        ...Acc,
        {
          type: 0;
          img: "/pipeNorth.png";
          x: P["x"];
          y: P["y"];
          width: pipeWidth;
          height: pipeHeight;
        },
        {
          type: 0;
          img: "/pipeSouth.png";
          x: P["x"];
          y: Add<Add<P["y"], pipeHeight>, pipeGap>;
          width: pipeWidth;
          height: pipeHeight;
        }
      ]
    : Acc
  : Acc;

type Draw<S extends GameState> = Update<
  S,
  {
    drawCommands: S["isCollided"] extends true
      ? [
          {
            type: 0;
            img: "/background.png";
            x: 0;
            y: 0;
            width: canvasWidth;
            height: canvasHeight;
          },
          {
            type: 0;
            img: "/game-over.png";
            x: Sub<Div<canvasWidth, 2>, Div<gameoverWidth, 2>>;
            y: Sub<Div<canvasHeight, 2>, Div<gameoverHeight, 2>>;
            width: gameoverWidth;
            height: gameoverHeight;
          },
          ...DrawPipes<0, S["pipes"], []>
        ]
      : [
          {
            type: 0;
            img: "/background.png";
            x: 0;
            y: 0;
            width: canvasWidth;
            height: canvasHeight;
          },
          {
            type: 0;
            img: "/bird.png";
            x: 50;
            y: S["birdY"];
            width: birdWidth;
            height: birdHeight;
          },
          ...DrawPipes<0, S["pipes"], []>
        ];
  }
>;

type InitialState = {
  birdY: Div<canvasHeight, 2>;
  velocity: 0;
  pipes: MakeDefaultPipes<pipesAmount, [], canvasWidth>;
  drawCommands: [];
  jumpInput: false;
  isCollided: false;
};

export type Main<_Argv extends string[]> = RequestAnimFrame<
  Draw<
    CheckCollision<
      UpdatePipes<ApplyGravity<ApplyJump<PossiblyResetGame<InitialState>>>>
    >
  >
>;
