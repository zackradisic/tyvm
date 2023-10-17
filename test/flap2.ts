type Gravity = 1;
type Jump = -15;
type canvasHeight = 600;

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

type ApplyGravity<S extends GameState> = Update<
  S,
  {
    birdY: Add<S["birdY"], Mul<S["velocity"], 0.5>>;
    velocity: Add<S["velocity"], Mul<Gravity, 0.5>>;
  }
>;

type DrawBird<S extends GameState> = Update<
  S,
  {
    drawCommands: [
      ...S["drawCommands"],
      {
        type: 0;
        img: "/bird.png";
        x: 50;
        y: S["birdY"];
        width: 40;
        height: 40;
      }
    ];
  }
>;

type InitialState = {
  birdY: Div<canvasHeight, 2>;
  velocity: 0;
  pipes: [];
  drawCommands: [];
  jumpInput: false;
  isCollided: false;
};

export type Main<Argv extends string[]> = RequestAnimFrame<
  DrawBird<ApplyGravity<InitialState>>
>;
