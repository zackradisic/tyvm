import {
  Add,
  Sub,
  Div,
  RequestAnimFrame,
  Mul,
  Gte,
  Update,
  DrawCommandKindImage,
  DrawCommandKindClearCanvas,
} from "./std";

type Gravity = 1;
type Jump = -15;
type canvasHeight = 600;
type canvasWidth = 800;
type birdWidth = 60;
type birdHeight = 40;
type gameoverHeight = 160;
type gameoverWidth = 300;

type Pipe = { x: number; y: number };

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

// type UpdatePipes<S extends GameState> = Eq<>
// type UpdatePipesImpl<P extends Array<Pipe>, Acc extends Array<Pipe>> = P extends infer [

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
        ];
  }
>;

type InitialState = {
  birdY: Div<canvasHeight, 2>;
  velocity: 0;
  pipes: [{ x: canvasWidth; y: 0 }];
  drawCommands: [];
  jumpInput: false;
  isCollided: false;
};

export type Main<_Argv extends string[]> = RequestAnimFrame<
  Draw<CheckCollision<ApplyGravity<ApplyJump<PossiblyResetGame<InitialState>>>>>
>;
