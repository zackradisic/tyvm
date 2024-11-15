const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = std.json.Token;
pub fn ArrayList(comptime T: type) type {
    return std.ArrayListAligned(T, 8);
}

/// Copied from std.json. But changed to return []align(8) const u8 instead of []const u8
/// Also will skip calling `jsonParse` on the value if it has the `__generateJsonParse` function defined on it.
/// This is done to avoid infinite recursion.
///
/// --- original comment ---
/// This is an internal function called recursively
/// during the implementation of `parseFromTokenSourceLeaky` and similar.
/// It is exposed primarily to enable custom `jsonParse()` methods to call back into the `parseFrom*` system,
/// such as if you're implementing a custom container of type `T`;
/// you can call `innerParse(T, ...)` for each of the container's items.
/// Note that `null` fields are not allowed on the `options` when calling this function.
/// (The `options` you get in your `jsonParse` method has no `null` fields.)
pub fn innerParse(
    comptime T: type,
    allocator: Allocator,
    source: anytype,
    options: std.json.ParseOptions,
) std.json.ParseError(@TypeOf(source.*))!T {
    switch (@typeInfo(T)) {
        .Bool => {
            return switch (try source.next()) {
                .true => true,
                .false => false,
                else => error.UnexpectedToken,
            };
        },
        .Float, .ComptimeFloat => {
            const token = try source.nextAllocMax(allocator, .alloc_if_needed, options.max_value_len.?);
            defer freeAllocated(allocator, token);
            const slice = switch (token) {
                inline .number, .allocated_number, .string, .allocated_string => |slice| slice,
                else => return error.UnexpectedToken,
            };
            return try std.fmt.parseFloat(T, slice);
        },
        .Int, .ComptimeInt => {
            const token = try source.nextAllocMax(allocator, .alloc_if_needed, options.max_value_len.?);
            defer freeAllocated(allocator, token);
            const slice = switch (token) {
                inline .number, .allocated_number, .string, .allocated_string => |slice| slice,
                else => return error.UnexpectedToken,
            };
            return sliceToInt(T, slice);
        },
        .Optional => |optionalInfo| {
            switch (try source.peekNextTokenType()) {
                .null => {
                    _ = try source.next();
                    return null;
                },
                else => {
                    return try innerParse(optionalInfo.child, allocator, source, options);
                },
            }
        },
        .Enum => {
            if (std.meta.hasFn(T, "jsonParse") and !std.meta.hasFn(T, "__generateJsonParse")) {
                return T.jsonParse(allocator, source, options);
            }

            const token = try source.nextAllocMax(allocator, .alloc_if_needed, options.max_value_len.?);
            defer freeAllocated(allocator, token);
            const slice = switch (token) {
                inline .number, .allocated_number, .string, .allocated_string => |slice| slice,
                else => return error.UnexpectedToken,
            };
            return sliceToEnum(T, slice);
        },
        .Union => |unionInfo| {
            if (std.meta.hasFn(T, "jsonParse") and !std.meta.hasFn(T, "__generateJsonParse")) {
                return T.jsonParse(allocator, source, options);
            }

            if (unionInfo.tag_type == null) @compileError("Unable to parse into untagged union '" ++ @typeName(T) ++ "'");

            if (.object_begin != try source.next()) return error.UnexpectedToken;

            var result: ?T = null;
            var name_token: ?Token = try source.nextAllocMax(allocator, .alloc_if_needed, options.max_value_len.?);
            const field_name = switch (name_token.?) {
                inline .string, .allocated_string => |slice| slice,
                else => {
                    return error.UnexpectedToken;
                },
            };

            inline for (unionInfo.fields) |u_field| {
                if (std.mem.eql(u8, u_field.name, field_name)) {
                    // Free the name token now in case we're using an allocator that optimizes freeing the last allocated object.
                    // (Recursing into innerParse() might trigger more allocations.)
                    freeAllocated(allocator, name_token.?);
                    name_token = null;
                    if (u_field.type == void) {
                        // void isn't really a json type, but we can support void payload union tags with {} as a value.
                        if (.object_begin != try source.next()) return error.UnexpectedToken;
                        if (.object_end != try source.next()) return error.UnexpectedToken;
                        result = @unionInit(T, u_field.name, {});
                    } else {
                        // Recurse.
                        result = @unionInit(T, u_field.name, try innerParse(u_field.type, allocator, source, options));
                    }
                    break;
                }
            } else {
                // Didn't match anything.
                return error.UnknownField;
            }

            if (.object_end != try source.next()) return error.UnexpectedToken;

            return result.?;
        },

        .Struct => |structInfo| {
            if (structInfo.is_tuple) {
                if (.array_begin != try source.next()) return error.UnexpectedToken;

                var r: T = undefined;
                inline for (0..structInfo.fields.len) |i| {
                    r[i] = try innerParse(structInfo.fields[i].type, allocator, source, options);
                }

                if (.array_end != try source.next()) return error.UnexpectedToken;

                return r;
            }

            if (std.meta.hasFn(T, "jsonParse") and !std.meta.hasFn(T, "__generateJsonParse")) {
                return T.jsonParse(allocator, source, options);
            }

            if (.object_begin != try source.next()) return error.UnexpectedToken;

            var r: T = undefined;
            var fields_seen = [_]bool{false} ** structInfo.fields.len;

            while (true) {
                var name_token: ?Token = try source.nextAllocMax(allocator, .alloc_if_needed, options.max_value_len.?);
                const field_name = switch (name_token.?) {
                    inline .string, .allocated_string => |slice| slice,
                    .object_end => { // No more fields.
                        break;
                    },
                    else => {
                        return error.UnexpectedToken;
                    },
                };

                inline for (structInfo.fields, 0..) |field, i| {
                    if (field.is_comptime) @compileError("comptime fields are not supported: " ++ @typeName(T) ++ "." ++ field.name);
                    if (std.mem.eql(u8, field.name, field_name)) {
                        // Free the name token now in case we're using an allocator that optimizes freeing the last allocated object.
                        // (Recursing into innerParse() might trigger more allocations.)
                        freeAllocated(allocator, name_token.?);
                        name_token = null;
                        if (fields_seen[i]) {
                            switch (options.duplicate_field_behavior) {
                                .use_first => {
                                    // Parse and ignore the redundant value.
                                    // We don't want to skip the value, because we want type checking.
                                    _ = try innerParse(field.type, allocator, source, options);
                                    break;
                                },
                                .@"error" => return error.DuplicateField,
                                .use_last => {},
                            }
                        }
                        @field(r, field.name) = try innerParse(field.type, allocator, source, options);
                        fields_seen[i] = true;
                        break;
                    }
                } else {
                    // Didn't match anything.
                    freeAllocated(allocator, name_token.?);
                    if (options.ignore_unknown_fields) {
                        try source.skipValue();
                    } else {
                        return error.UnknownField;
                    }
                }
            }
            try fillDefaultStructValues(T, &r, &fields_seen);
            return r;
        },

        .Array => |arrayInfo| {
            switch (try source.peekNextTokenType()) {
                .array_begin => {
                    // Typical array.
                    return internalParseArray(T, arrayInfo.child, arrayInfo.len, allocator, source, options);
                },
                .string => {
                    if (arrayInfo.child != u8) return error.UnexpectedToken;
                    // Fixed-length string.

                    var r: T = undefined;
                    var i: usize = 0;
                    while (true) {
                        switch (try source.next()) {
                            .string => |slice| {
                                if (i + slice.len != r.len) return error.LengthMismatch;
                                @memcpy(r[i..][0..slice.len], slice);
                                break;
                            },
                            .partial_string => |slice| {
                                if (i + slice.len > r.len) return error.LengthMismatch;
                                @memcpy(r[i..][0..slice.len], slice);
                                i += slice.len;
                            },
                            .partial_string_escaped_1 => |arr| {
                                if (i + arr.len > r.len) return error.LengthMismatch;
                                @memcpy(r[i..][0..arr.len], arr[0..]);
                                i += arr.len;
                            },
                            .partial_string_escaped_2 => |arr| {
                                if (i + arr.len > r.len) return error.LengthMismatch;
                                @memcpy(r[i..][0..arr.len], arr[0..]);
                                i += arr.len;
                            },
                            .partial_string_escaped_3 => |arr| {
                                if (i + arr.len > r.len) return error.LengthMismatch;
                                @memcpy(r[i..][0..arr.len], arr[0..]);
                                i += arr.len;
                            },
                            .partial_string_escaped_4 => |arr| {
                                if (i + arr.len > r.len) return error.LengthMismatch;
                                @memcpy(r[i..][0..arr.len], arr[0..]);
                                i += arr.len;
                            },
                            else => unreachable,
                        }
                    }

                    return r;
                },

                else => return error.UnexpectedToken,
            }
        },

        .Vector => |vecInfo| {
            switch (try source.peekNextTokenType()) {
                .array_begin => {
                    return internalParseArray(T, vecInfo.child, vecInfo.len, allocator, source, options);
                },
                else => return error.UnexpectedToken,
            }
        },

        .Pointer => |ptrInfo| {
            switch (ptrInfo.size) {
                .One => {
                    const r: *ptrInfo.child = try allocator.create(ptrInfo.child);
                    r.* = try innerParse(ptrInfo.child, allocator, source, options);
                    return r;
                },
                .Slice => {
                    switch (try source.peekNextTokenType()) {
                        .array_begin => {
                            _ = try source.next();

                            // Typical array.
                            var arraylist = ArrayList(ptrInfo.child).init(allocator);
                            while (true) {
                                switch (try source.peekNextTokenType()) {
                                    .array_end => {
                                        _ = try source.next();
                                        break;
                                    },
                                    else => {},
                                }

                                try arraylist.ensureUnusedCapacity(1);
                                arraylist.appendAssumeCapacity(try innerParse(ptrInfo.child, allocator, source, options));
                            }

                            if (ptrInfo.sentinel) |some| {
                                const sentinel_value = @as(*align(1) const ptrInfo.child, @ptrCast(some)).*;
                                return try arraylist.toOwnedSliceSentinel(sentinel_value);
                            }

                            return try arraylist.toOwnedSlice();
                        },
                        .string => {
                            if (ptrInfo.child != u8) return error.UnexpectedToken;

                            // Dynamic length string.
                            if (ptrInfo.sentinel) |sentinel_ptr| {
                                // Use our own array list so we can append the sentinel.
                                var value_list = ArrayList(u8).init(allocator);
                                _ = try source.allocNextIntoArrayList(&value_list, .alloc_always);
                                return try value_list.toOwnedSliceSentinel(@as(*const u8, @ptrCast(sentinel_ptr)).*);
                            }
                            if (ptrInfo.is_const) {
                                switch (try source.nextAllocMax(allocator, options.allocate.?, options.max_value_len.?)) {
                                    inline .string, .allocated_string => |slice| return slice,
                                    else => unreachable,
                                }
                            } else {
                                // Have to allocate to get a mutable copy.
                                switch (try source.nextAllocMax(allocator, .alloc_always, options.max_value_len.?)) {
                                    .allocated_string => |slice| return slice,
                                    else => unreachable,
                                }
                            }
                        },
                        else => return error.UnexpectedToken,
                    }
                },
                else => @compileError("Unable to parse into type '" ++ @typeName(T) ++ "'"),
            }
        },
        else => @compileError("Unable to parse into type '" ++ @typeName(T) ++ "'"),
    }
    unreachable;
}

pub inline fn assert(ok: bool) void {
    if (!ok) unreachable;
}

fn internalParseArray(
    comptime T: type,
    comptime Child: type,
    comptime len: comptime_int,
    allocator: Allocator,
    source: anytype,
    options: std.json.ParseOptions,
) !T {
    assert(.array_begin == try source.next());

    var r: T = undefined;
    var i: usize = 0;
    while (i < len) : (i += 1) {
        r[i] = try innerParse(Child, allocator, source, options);
    }

    if (.array_end != try source.next()) return error.UnexpectedToken;

    return r;
}

fn freeAllocated(allocator: Allocator, token: std.json.Token) void {
    switch (token) {
        .allocated_number, .allocated_string => |slice| {
            allocator.free(slice);
        },
        else => {},
    }
}

fn sliceToInt(comptime T: type, slice: []const u8) !T {
    if (isNumberFormattedLikeAnInteger(slice))
        return std.fmt.parseInt(T, slice, 10);
    // Try to coerce a float to an integer.
    const float = try std.fmt.parseFloat(f128, slice);
    if (@round(float) != float) return error.InvalidNumber;
    if (float > std.math.maxInt(T) or float < std.math.minInt(T)) return error.Overflow;
    return @as(T, @intCast(@as(i128, @intFromFloat(float))));
}

fn sliceToEnum(comptime T: type, slice: []const u8) !T {
    // Check for a named value.
    if (std.meta.stringToEnum(T, slice)) |value| return value;
    // Check for a numeric value.
    if (!isNumberFormattedLikeAnInteger(slice)) return error.InvalidEnumTag;
    const n = std.fmt.parseInt(@typeInfo(T).Enum.tag_type, slice, 10) catch return error.InvalidEnumTag;
    return std.meta.intToEnum(T, n);
}

pub fn isNumberFormattedLikeAnInteger(value: []const u8) bool {
    if (std.mem.eql(u8, value, "-0")) return false;
    return std.mem.indexOfAny(u8, value, ".eE") == null;
}

fn fillDefaultStructValues(comptime T: type, r: *T, fields_seen: *[@typeInfo(T).Struct.fields.len]bool) !void {
    inline for (@typeInfo(T).Struct.fields, 0..) |field, i| {
        if (!fields_seen[i]) {
            if (field.default_value) |default_ptr| {
                const default = @as(*align(1) const field.type, @ptrCast(default_ptr)).*;
                @field(r, field.name) = default;
            } else {
                return error.MissingField;
            }
        }
    }
}
