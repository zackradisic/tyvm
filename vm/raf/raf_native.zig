pub fn request_anim_frame(serialized_ptr: [*]const u8, serialized_len: usize, cap: usize) void {
    _ = cap;
    _ = serialized_len;
    _ = serialized_ptr;
    @panic("Only implemented for Wasm target.");
}
