/* eslint-disable camelcase */
import {
  WASI,
  File,
  OpenFile,
  Fd,
  PreopenDirectory,
} from "@bjorn3/browser_wasi_shim";

import * as Tyvm from "./wasm_types";
import { lowerI64Imports } from "@wasmer/wasm-transformer";

type DrawState = {
  images: Record<string, HTMLImageElement>;
};

type VMState = {
  vmRef: Tyvm.VMRef;
  vmFns: Tyvm.Exports;
  memory: WebAssembly.Memory;
  globalFn: Tyvm.FnRef;
};

function vmStateReadString(vmState: VMState, ptr: number, len: number): string {
  // Get the buffer from the WebAssembly memory
  const buffer = new Uint8Array(vmState.memory.buffer, ptr, len);

  // Decode the buffer into a string
  const text = new TextDecoder().decode(buffer);

  return text;
}

function vmStateReadDrawCommands(
  vmState: VMState,
  ptr: number,
  len: number
): Tyvm.DrawCommand[] {
  const str = vmStateReadString(vmState, ptr, len);
  return JSON.parse(str);
}

function drawCommandsExecuteMany(
  ctx: CanvasRenderingContext2D,
  drawState: DrawState,
  drawCommands: Tyvm.DrawCommand[]
) {
  for (const cmd of drawCommands) {
    drawCommandsExecute(ctx, drawState, cmd);
  }
}

function drawCommandsExecute(
  ctx: CanvasRenderingContext2D,
  drawState: DrawState,
  drawCommand: Tyvm.DrawCommand
) {
  switch (drawCommand.type) {
    // Draw image
    case 0: {
      let image = drawState.images[drawCommand.img];
      if (image === undefined) {
        const img = new Image();
        img.src = drawCommand.img;
        image = img;
        drawState.images[drawCommand.img] = img;
      }

      ctx.drawImage(
        image,
        drawCommand.x,
        drawCommand.y,
        drawCommand.width,
        drawCommand.height
      );
      break;
    }
    // Clear
    case 1: {
      // ctx.clearRect()
      break;
    }
  }
}

const instantiateWasm = async (
  wasi: WASI,
  vmState: { state: VMState | undefined },
  canvasRef: React.RefObject<HTMLCanvasElement | undefined>
) => {
  const bird = new Image();
  bird.src = "/bird.png";
  const drawState: DrawState = {
    images: {
      "/bird.png": bird,
    },
  };
  let ctx: CanvasRenderingContext2D | undefined = undefined;
  const wasm = await WebAssembly.compileStreaming(fetch("/tyvm.wasm"));
  const instance = await WebAssembly.instantiate(wasm, {
    wasi_snapshot_preview1: wasi.wasiImport,
    env: {
      request_anim_frame(
        serializedDrawCommandsPtr: number,
        serializedDrawCommandsLen: number,
        cap: number
      ) {
        function runFrame() {
          if (!canvasRef.current) return;
          if (!ctx) {
            ctx = canvasRef.current.getContext("2d")!;
          }
          const vm = vmState.state;
          if (!vm) return;

          // vm.vmFns.run(vm.vmRef, vm.globalFn);

          const drawCommands = vmStateReadDrawCommands(
            vmState.state!,
            serializedDrawCommandsPtr,
            serializedDrawCommandsLen
          );

          drawCommandsExecuteMany(ctx, drawState, drawCommands);

          // vm.vmFns.dealloc(serializedDrawCommandsPtr, cap);

          // requestAnimationFrame(runFrame);
        }
        // requestAnimationFrame(runFrame);
        runFrame();
      },
    },
  });
  return instance;
};

export const initWasm = async (
  canvasRef: React.RefObject<HTMLCanvasElement | undefined>
) => {
  let fds = [
    new OpenFile(new File([])), // stdin
    // new OpenFile(new File([])), // stdout
    // new OpenFile(new File([])), // stderr
    new Stdio("stdout"), // stdout
    new Stdio("stderr"), // stderr
  ];

  // @ts-expect-error
  let vmState: { state: VMState | undefined } = {};

  const wasi = new WASI([], [], fds);

  const instance = await instantiateWasm(wasi, vmState, canvasRef);
  const memory = instance.exports.memory as WebAssembly.Memory;
  const vmFns = instance.exports as Tyvm.Exports;
  const { init, run, get_function, get_global_function, alloc, dealloc } =
    vmFns;

  // const programSource = fib
  const programSource = await fetch("/flap.ts").then((res) => res.text());

  const progBinary = new TextEncoder().encode(programSource);
  const ptr = alloc(progBinary.byteLength);
  const memBuf = new Uint8Array(memory.buffer, ptr, progBinary.length);
  memBuf.set(progBinary);

  wasi.initialize({
    exports: {
      _initialize: () => {
        const vmRef = init(ptr, progBinary.length);
        const globalFn = vmFns.get_global_function(vmRef);
        const isGame = vmFns.is_game(vmRef);
        vmState.state = {
          vmRef,
          vmFns,
          memory,
          globalFn,
        };

        console.log("[_initialize] state", vmState);

        // const globalFunctionRef = get_global_function(vmState.state!.vmRef);
        try {
          if (isGame) {
            window.addEventListener("keydown", (e) => {
              console.log("THE EVENT", e);
              if (e.code === "Space") {
                e.preventDefault();
                vmFns.jump(vmRef);
              }
            });
            const runGame = () => {
              run(vmState.state!.vmRef, globalFn);
              requestAnimationFrame(runGame);
            };
            requestAnimationFrame(runGame);
          } else {
            run(vmState.state!.vmRef, globalFn);
          }
        } catch (err) {
          console.error(err);
        }

        // console.log("STDOUT", fds[1].buffer);
        // console.log("STDERR", fds[2].buffer);
      },
      memory: instance.exports.memory as WebAssembly.Memory,
    },
  });
};

const fib = `
import { Print, Add, Sub, Lte, Eq, Panic, AssertEq } from "./std";

/**
 * Compute the Nth fibonacci number
 **/
type Fib<N extends number> = Lte<N, 1> extends true ? N : FibIter<N, 2, 1, 0>;

/**
 * Comuptes the Nth fibonacci using iterative recursion
 **/
type FibIter<
  N extends number,
  I extends number,
  NminusOne extends number,
  NminusTwo extends number
> = N extends I
  ? Add<NminusOne, NminusTwo>
  : FibIter<N, Add<I, 1>, Add<NminusOne, NminusTwo>, NminusOne>;

export type Main<Argv extends string[]> = AssertEq<Print<Fib<10>>, 55>;
`;

class Stdio extends Fd {
  buffer: string = "";
  name: string;

  constructor(name: string) {
    super();
    this.name = name;
  }

  fd_write(view8: any, iovs: any) {
    let nwritten = 0;

    // Iterate through each iovec structure
    for (const iov of iovs) {
      const dataPtr = iov.buf; // Assuming buf is a pointer to the actual buffer
      const dataLen = iov.buf_len;

      // Extract the buffer data into a new Uint8Array
      const dataSlice = view8.subarray(dataPtr, dataPtr + dataLen);

      // Decode the buffer to string
      const data = new TextDecoder().decode(dataSlice);

      // Add to internal buffer
      this.buffer += data;

      // Update nwritten count
      nwritten += dataLen;

      // Check for newline and flush
      const newLineIndex = this.buffer.indexOf("\n");
      if (newLineIndex !== -1) {
        console.log(`[${this.name}]`, this.buffer.substring(0, newLineIndex));
        this.buffer = this.buffer.substring(newLineIndex + 1);
      }
    }

    return {
      ret: 0, // Assuming 0 is the success code
      nwritten: nwritten,
    };
  }
}
