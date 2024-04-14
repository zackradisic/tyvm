import {
  WASI,
  File,
  OpenFile,
  Fd,
  PreopenDirectory,
} from "@bjorn3/browser_wasi_shim";
import * as Tyvm from "../site/src/lib/wasm_types";
import fs from "node:fs";

const PLATFORM: "bun/node" | "browser" = process.env.PLATFORM ?? "browser";

class Stdio extends Fd {
  buffer: string = "";
  name: string;
  timeout: ReturnType<typeof setTimeout> | undefined = undefined;

  constructor(name: string) {
    super();
    this.name = name;
  }

  flush() {
    const string = `[${this.name}] ${this.buffer.substring(0, this.buffer.length)}`;
    console.log(string);
  }

  fd_write(view8: Uint8Array) {
    let nwritten = 0;
    const dataSlice = view8;

    // Decode the buffer to string
    const data = new TextDecoder().decode(dataSlice);

    // Add to internal buffer
    this.buffer += data;

    // Update nwritten count
    nwritten += dataSlice.byteLength;

    // Check for newline and flush
    // const newLineIndex = this.buffer.indexOf("\n");
    // if (newLineIndex !== -1) {
    // console.log(`[${this.name}]`, this.buffer.substring(0, newLineIndex));
    //   this.buffer = this.buffer.substring(newLineIndex + 1);
    // } else if (this.buff

    if (this.timeout !== undefined) clearTimeout(this.timeout);
    const stdio = this;
    this.timeout = setTimeout(() => {
      stdio.flush();
    }, 1000);

    return {
      ret: 0, // Assuming 0 is the success code
      nwritten: nwritten,
    };
  }
}

async function run() {
  let fds = [
    new OpenFile(new File([])), // stdin
    // new OpenFile(new File([])), // stdout
    // new OpenFile(new File([])), // stderr
    new Stdio("stdout"), // stdout
    new Stdio("stderr"), // stderr
  ];

  const wasi = new WASI([], [], fds);

  // Import the WASM module
  const wasmModule = await WebAssembly.compile(
    PLATFORM === "bun/node"
      ? fs.readFileSync("./zig-out/bin/tyvm.wasm").buffer
      : await fetch("./zig-out/bin/tyvm.wasm").then((r) => r.arrayBuffer())

    // await fetch("./zig-out/bin/tyvm.wasm").then((r) => r.arrayBuffer())

    // await Bun.file(
    //   "/Users/zackradisic/Code/tyvm/zig-out/bin/tyvm.wasm"
    // ).arrayBuffer()
  );

  // Create an instance of the module
  const instance = await WebAssembly.instantiate(wasmModule, {
    wasi_snapshot_preview1: wasi.wasiImport,
    env: {
      request_anim_frame(
        serializedDrawCommandsPtr: number,
        serializedDrawCommandsLen: number,
        cap: number
      ) {
        console.log(
          "request_anim_frame",
          serializedDrawCommandsPtr,
          serializedDrawCommandsLen,
          cap
        );
      },
    },
  });
  const vmFns = instance.exports as Tyvm.Exports;
  const { init, run, get_function, get_global_function, alloc, dealloc } =
    vmFns;

  const programSource =
    PLATFORM === "bun/node"
      ? await fetch("./test/array.ts").then((r) => r.text())
      : fs.readFileSync("./test/array.ts").toString("utf-8");
  const progBinary = new TextEncoder().encode(programSource);
  const ptr = alloc(progBinary.byteLength);
  const memBuf = new Uint8Array(
    instance.exports.memory.buffer,
    ptr,
    progBinary.length
  );
  memBuf.set(progBinary);

  wasi.initialize({
    exports: {
      _initialize() {
        const vmRef = init(ptr, progBinary.length);
        const globalFn = vmFns.get_global_function(vmRef);
        run(vmRef, globalFn);
      },
      memory: instance.exports.memory as WebAssembly.Memory,
    },
  });

  // Now you can use the exported functions from your WASM module
  // const result = instance.exports.yourFunctionName();
  // console.log(result);
}

run();
