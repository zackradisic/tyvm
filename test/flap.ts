import {
  Add,
  Sub,
  Div,
  RequestAnimFrame,
  Mul,
  Update,
  DrawCommandKindImage,
  DrawCommandKindClearCanvas,
} from "./std";

type Gravity = 1;
type Jump = -15;

type GameState = {
  birdY: number;
  velocity: number;
  pipes: Array<Pipe>;
  drawCommands: Array<DrawCommand>;
  jumpInput: boolean;
  isCollided: boolean;
};

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

type ApplyGravity<S extends GameState> = Update<
  S,
  {
    birdY: Add<S["birdY"], Mul<S["velocity"], 0.5>>;
    velocity: Add<S["velocity"], Mul<Gravity, 0.5>>;
  }
>;

type CheckCollision<S extends GameState> = S;

type DrawBird<S extends GameState> = Update<
  S,
  {
    drawCommands: [
      ...S["drawCommands"],
      {
        type: DrawCommandKindImage;
        img: "bird";
        x: 50;
        y: S["birdY"];
        width: 40;
        height: 40;
      }
    ];
  }
>;

type ApplyJump<S extends GameState> = Update<
  S,
  {
    velocity: S["jumpInput"] extends true ? Jump : S["velocity"];
    jumpInput: false;
  }
>;

type UpdatePipes<S extends GameState> = Update<S, {}>;

type canvasHeight = 600;

export type InitialState = {
  birdY: Div<canvasHeight, 2>;
  velocity: 0;
  pipes: [];
  drawCommands: [];
  jumpInput: false;
  isCollided: false;
};

export type Main<_Args extends string[]> = RequestAnimFrame<
  CheckCollision<UpdatePipes<DrawBird<ApplyJump<ApplyGravity<InitialState>>>>>
>;
