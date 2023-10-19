import Image from "next/image";
import { useEffect, useRef } from "react";
import { initWasm } from "q/lib/load_wasm";
import React from "react";

export default function Home() {
  const canvasRef = React.useRef<HTMLCanvasElement>();
  const [gameLoaded, setGameLoaded] = React.useState(false);

  async function runWasm() {
    await initWasm(canvasRef);
    setGameLoaded(true);
  }

  return (
    <main className="flex min-h-screen flex-col items-center font-inter text-offwhite bg-bg p-16">
      <div className="flex flex-row items-baseline">
        <a
          href="https://github.com/100xsoftware"
          className="font-semibold italic font-iosevka text-4xl"
        >
          100<span className="text-primary">x</span>
        </a>
        <p className="ml-2">presents</p>
      </div>
      <h1 className="text-4xl font-regular mt-14 max-w-2xl text-center">
        Flappy bird written in type-level Typescript
      </h1>

      <button
        className="mt-20 bg-offwhite text-darkbg font-semibold rounded-md p-2"
        onClick={() => runWasm()}
        disabled={gameLoaded}
      >
        {gameLoaded ? "Game loaded" : "Load game"}
      </button>

      <canvas
        className="mt-12 rounded-sm"
        width={600}
        height={400}
        ref={(ref) => {
          canvasRef.current = ref || undefined;
        }}
      />

      <div>
        <h6 className="font-bold mt-12">Controls</h6>
        <ul>
          <li>
            <span className="tracking-widest font-semibold text-primary italic">
              Space
            </span>{" "}
            to jump{" "}
          </li>
          <li>
            <span className="tracking-widest font-semibold text-primary italic">
              Enter
            </span>{" "}
            to reset game
          </li>
        </ul>

        <h6 className="mt-4">
          How did is it done? Read{" "}
          <a
            className="text-primary"
            href="https://zackoverflow.dev/writing/flappy-bird-in-type-level-typescript"
          >
            here
          </a>
        </h6>

        <h6 className="font-bold mt-4">
          Source code{" "}
          <a
            className="text-primary"
            href="https://github.com/zackradisic/tyvm/blob/master/site/public/flap.ts"
          >
            here
          </a>
        </h6>
      </div>
    </main>
  );
}
