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
  DrawCommandKindImage,
  DrawCommandKindClearCanvas,
  Rand,
  Floor,
  Or,
  And,
} from "./std";

type Gravity = 1;
type Jump = -12.5;
type canvasHeight = 600;
type canvasWidth = 800;
type birdX = 50;
type birdWidth = 60;
type birdHeight = 40;
type gameoverHeight = 160;
type gameoverWidth = 300;
type pipeWidth = 60;
type pipeHeight = 250;
type pipeGap = 200;
type pipesAmount = Mul<Floor<Div<canvasWidth, Add<pipeWidth, pipeGap>>>, 2>;

type Pipe = { x: number; y: number };

type MakeDefaultPipes<
  Amount extends number,
  Acc extends Pipe[],
  X extends number,
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
  reset: boolean;
};

type PossiblyResetGame<S extends GameState> = S["reset"] extends true
  ? Update<
      S,
      {
        birdY: Div<canvasHeight, 2>;
        velocity: 0;
        isCollided: false;
        pipes: MakeDefaultPipes<pipesAmount, [], canvasWidth>;
        reset: false;
      }
    >
  : S;

type ApplyJump<S extends GameState> = S["isCollided"] extends true
  ? S
  : Update<
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

type CollidesPipeX<BirdX extends number, PipeX extends number> = Or<
  // pipe.x <= bird.x < pipe.x + pipe.width
  And<Gte<BirdX, PipeX>, Lt<BirdX, Add<PipeX, pipeWidth>>>,
  // pipe.x <= bird.x + bird.width < pipe.x + pipe.width
  And<
    Gte<Add<BirdX, birdWidth>, PipeX>,
    Lt<Add<BirdX, birdWidth>, Add<PipeX, pipeWidth>>
  >
>;

type CollidesPipeY<
  BirdY extends number,
  PipeY extends number,
  PipeHeight extends number,
> = Or<
  // pipe.y <= bird.y < pipe.y + pipe.height
  And<Gte<BirdY, PipeY>, Lt<BirdY, Add<PipeY, PipeHeight>>>,
  // pipe.y <= bird.y + bird.height < pipe.y + pipe.height
  And<
    Gte<Add<BirdY, birdHeight>, PipeY>,
    Lt<Add<BirdY, birdHeight>, Add<PipeY, PipeHeight>>
  >
>;

type CollidesPipe<
  BirdY extends number,
  BirdX extends number,
  PipeX extends number,
  PipeY extends number,
  PipeHeight extends number,
> = And<CollidesPipeX<BirdX, PipeX>, CollidesPipeY<BirdY, PipeY, PipeHeight>>;

type CheckPipeCollisions<
  BirdX extends number,
  BirdY extends number,
  I extends number,
  Pipes extends Array<Pipe>,
> = Pipes[I] extends infer P extends Pipe
  ? Or<
      CollidesPipe<BirdY, BirdX, P["x"], P["y"], pipeHeight>,
      CollidesPipe<
        BirdY,
        BirdX,
        P["x"],
        Add<Add<P["y"], pipeHeight>, pipeGap>,
        Sub<canvasHeight, Add<Add<P["y"], pipeHeight>, pipeGap>>
      >
    > extends true
    ? true
    : CheckPipeCollisions<BirdX, BirdY, Add<I, 1>, Pipes>
  : false;

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
  : CheckPipeCollisions<birdX, S["birdY"], 0, S["pipes"]> extends true
  ? Update<S, { isCollided: true }>
  : S;

type FarthestPipe<
  I extends number,
  Pipes extends Array<Pipe>,
  MaxIdx extends number,
  MaxX extends number,
> = Pipes[I] extends infer P extends Pipe
  ? FarthestPipe<
      Add<I, 1>,
      Pipes,
      Gte<P["x"], MaxX> extends true ? I : MaxIdx,
      Gte<P["x"], MaxX> extends true ? P["x"] : MaxX
    >
  : Pipes[MaxIdx];

type MovePipesImpl<
  Pipes extends Array<Pipe>,
  I extends number,
  Acc extends Array<Pipe>,
> = Lt<I, Pipes["length"]> extends true
  ? Pipes[I] extends infer P extends Pipe
    ? Lte<Add<Sub<P["x"], 1>, pipeWidth>, 0> extends true // pipe is off the screen
      ? FarthestPipe<0, Pipes, 0, 0> extends infer Next extends Pipe
        ? MovePipesImpl<
            Pipes,
            Add<I, 1>,
            [
              ...Acc,
              {
                x: Add<Add<Next["x"], pipeWidth>, pipeGap>;
                y: Sub<Floor<Rand<0, pipeHeight>>, pipeHeight>;
              },
            ]
          >
        : false
      : MovePipesImpl<
          Pipes,
          Add<I, 1>,
          [...Acc, { x: Sub<P["x"], 1>; y: P["y"] }]
        >
    : false
  : Acc;

type UpdatePipes<S extends GameState> = Update<
  S,
  { pipes: MovePipesImpl<S["pipes"], 0, []> }
>;

type DrawPipes<
  I extends number,
  S extends Pipe[],
  Acc extends DrawCommand[],
> = Lt<I, S["length"]> extends true
  ? S[I] extends infer P extends Pipe
    ? DrawPipes<
        Add<I, 1>,
        S,
        [
          ...Acc,
          {
            img: "/pipeSouth.png";
            type: 0;
            x: P["x"];
            y: P["y"];
            width: pipeWidth;
            height: pipeHeight;
          },
          {
            img: "/pipeNorth.png";
            type: 0;
            x: P["x"];
            y: Add<Add<P["y"], pipeHeight>, pipeGap>;
            width: pipeWidth;
            height: Sub<canvasHeight, Add<Add<P["y"], pipeHeight>, pipeGap>>;
          },
        ]
      >
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
            img: "/bird.png";
            x: birdX;
            y: S["birdY"];
            width: birdWidth;
            height: birdHeight;
          },
          ...DrawPipes<0, S["pipes"], []>,
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
            x: birdX;
            y: S["birdY"];
            width: birdWidth;
            height: birdHeight;
          },
          ...DrawPipes<0, S["pipes"], []>,
        ];
  }
>;

type InitialState = {
  birdY: Div<canvasHeight, 2>;
  velocity: 0;
  pipes: MakeDefaultPipes<pipesAmount, [], canvasWidth>;
  drawCommands: [];
  jumpInput: false;
  reset: false;
  isCollided: false;
};

export type Main<_Argv extends string[]> = RequestAnimFrame<
  Draw<
    CheckCollision<
      UpdatePipes<ApplyGravity<ApplyJump<PossiblyResetGame<InitialState>>>>
    >
  >
>;
