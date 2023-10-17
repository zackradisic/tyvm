const builtin = @import("builtin");
const native = @import("./raf/raf_native.zig");
const wasm = @import("./raf/raf_wasm.zig");

pub const request_anim_frame = if (builtin.cpu.arch == .wasm32) wasm.request_anim_frame else native.request_anim_frame;