RELEASE=0
ifeq ($(RELEASE), 1)
    CARGO_FLAGS=--release
	ZIG_FLAGS=-Doptimize=ReleaseSafe
else
    CARGO_FLAGS=
	ZIG_FLAGS=
endif

target/release/libtyvm_compiler.a: src/*.rs
	cargo build $(CARGO_FLAGS)

# target/wasm32-wasi/release/libtyvm_compiler.a: src/*.rs
# 	RUSTFLAGS='-C target-feature=+bulk-memory' cargo build $(CARGO_FLAGS) --target=wasm32-wasi

target/wasm32-wasi/release/libtyvm_compiler.a: src/*.rs
	cargo build $(CARGO_FLAGS) --target=wasm32-wasi

compiler: target/release/libtyvm_compiler.a
compiler-wasm: target/wasm32-wasi/release/libtyvm_compiler.a

zig-out/bin/tyvm: vm/*.zig compiler
	zig build $(ZIG_FLAGS)

zig-out/bin/tyvm.wasm: vm/*.zig compiler-wasm
	zig build -Dtarget=wasm32-wasi $(ZIG_FLAGS)

site/public/tyvm.wasm: zig-out/bin/tyvm.wasm
	cp zig-out/bin/tyvm.wasm site/public/tyvm.wasm

vm: zig-out/bin/tyvm
vm-wasm: zig-out/bin/tyvm.wasm site/public/tyvm.wasm

.PHONY: clean
clean:
	rm -rf target zig-out zig-cache

run-wasm: vm-wasm
	wasmtime ./zig-out/bin/tyvm.wasm --dir=.

run: vm
	./zig-out/bin/tyvm 
