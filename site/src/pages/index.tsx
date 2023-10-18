import Image from "next/image";
import { Caveat, Inter } from "next/font/google";
import { useEffect, useRef } from "react";
import { initWasm } from "q/lib/load_wasm";
import React from "react";

const inter = Inter({ subsets: ["latin"] });

export default function Home() {
  const canvasRef = React.createRef<HTMLCanvasElement>();

  async function runWasm() {
    console.warn("WTF");
    await initWasm(canvasRef);
  }

  return (
    <main
      className={`flex min-h-screen flex-col items-center justify-between p-24 ${inter.className}`}
    >
      <button onClick={() => runWasm()}>
        Flappy bird in type-level Typescript
      </button>

      <canvas width={800} height={600} ref={canvasRef} />
    </main>
  );
}
