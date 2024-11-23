import { initWasm } from "q/lib/load_wasm";
import React, { useState, useRef, useEffect } from "react";
import { match, P } from "ts-pattern";

export type WasmLoadState =
  | {
      type: "idle";
    }
  | { type: "loading" }
  | { type: "loaded" }
  | { type: "error" };

const GRID_SIZE = 28;
const CELL_SIZE = 20; // Size of each cell in pixels

export default function Mnist() {
  const canvasRef = React.useRef<HTMLCanvasElement>();
  const [gameLoaded, setGameLoaded] = React.useState<WasmLoadState>({
    type: "idle",
  });

  async function runWasm() {
    try {
      setGameLoaded({ type: "loading" });
      await initWasm(canvasRef, "/mnist.ts");
      setGameLoaded({ type: "loaded" });
    } catch (err) {
      console.error("Error", err);
      setGameLoaded({ type: "error" });
    }
  }

  return (
    <div>
      <button
        className="mt-20 bg-offwhite text-darkbg font-semibold rounded-md p-2"
        onClick={() => runWasm()}
        disabled={match(gameLoaded.type)
          .with(P.union("loading", "loaded", "error"), () => true)
          .otherwise(() => false)}
      >
        {match(gameLoaded.type)
          .with("idle", () => "Load game")
          .with("loaded", () => "Game loaded")
          .with("loading", () => "Loading game..")
          .with("error", () => "Error loading game!")
          .exhaustive()}
      </button>
      <canvas
        ref={(ref) => {
          canvasRef.current = ref || undefined;
        }}
        width={GRID_SIZE * CELL_SIZE}
        height={GRID_SIZE * CELL_SIZE}
        style={{ border: "1px solid black" }}
      />
    </div>
  );
}
