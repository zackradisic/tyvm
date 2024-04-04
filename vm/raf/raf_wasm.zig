const std = @import("std");

pub extern fn request_anim_frame(serialized_ptr: [*]const u8, serialized_len: usize, cap: usize) void;
