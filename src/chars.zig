const std = @import("std");

pub const Char = union(enum) {
    one: [1]u8,
    two: [2]u8,
    three: [3]u8,
    four: [4]u8,

    const Self = @This();

    pub fn init(slice: []const u8) !Self {
        if (slice.len == 0) return error.EmptyChar;

        if (slice[0] == '\\') {
            return try Self.fromEscape(slice);
        }

        return try Self.fromUtf8(slice);
    }

    pub fn fromUtf8(slice: []const u8) !Self {
        const first = slice[0];
        const len: usize = switch (first) {
            0x00...0x7F => 1, // ASCII
            0xC0...0xDF => 2,
            0xE0...0xEF => 3,
            0xF0...0xF7 => 4,
            else => return error.InvalidUtf8Header,
        };
        if (slice.len < len) return error.IncompleteUtf8;

        var buf: [4]u8 = undefined;
        for (slice[0..len], 0..) |b, i| buf[i] = b;

        return switch (len) {
            1 => Self{ .one = [1]u8{buf[0]} },
            2 => Self{ .two = buf[0..2].* },
            3 => Self{ .three = buf[0..3].* },
            4 => Self{ .four = buf },
            else => unreachable,
        };
    }

    pub fn fromEscape(slice: []const u8) !Self {
        if (slice.len < 2) return error.InvalidEscape;

        const kind = slice[1];

        return switch (kind) {
            'n' => Self{ .one = [_]u8{'\n'} },
            'r' => Self{ .one = [_]u8{'\r'} },
            't' => Self{ .one = [_]u8{'\t'} },
            '0' => Self{ .one = [_]u8{0} },
            '\\' => Self{ .one = [_]u8{'\\'} },
            '\'' => Self{ .one = [_]u8{'\''} },
            '"' => Self{ .one = [_]u8{'"'} },
            'u' => try Self.fromUnicodeEscape(slice),
            else => return error.UnknownEscape,
        };
    }

    pub fn fromUnicodeEscape(slice: []const u8) !Self {
        // Must be at least "\\u{X}"
        if (slice.len < 4 or slice[2] != '{') return error.InvalidUnicodeEscape;

        var i: usize = 3; // after '\', 'u', '{'
        var value: u21 = 0;
        var digits: usize = 0;

        while (i < slice.len) {
            const c = slice[i];
            if (c == '}') break;
            if (digits >= 6) return error.UnicodeEscapeTooLong;

            const d = std.fmt.charToDigit(c, 16) catch return error.InvalidUnicodeDigit;
            value = value * 16 + d;
            digits += 1;
            i += 1;
        }

        if (digits == 0) return error.UnicodeEscapeEmpty;
        if (i >= slice.len or slice[i] != '}') return error.UnclosedUnicodeEscape;

        // Validate scalar
        if (!isValidUnicodeScalar(value)) return error.InvalidUnicodeScalar;

        // Encode to UTF-8
        var buf: [4]u8 = undefined;
        const len = try std.unicode.utf8Encode(value, &buf);
        return switch (len) {
            1 => Self{ .one = buf[0..1].* },
            2 => Self{ .two = buf[0..2].* },
            3 => Self{ .three = buf[0..3].* },
            4 => Self{ .four = buf[0..4].* },
            else => unreachable,
        };
    }

    pub fn asBytes(self: *const Self) []const u8 {
        return switch (self.*) {
            .one => |*b| b[0..],
            .two => |*b| b[0..],
            .three => |*b| b[0..],
            .four => |*b| b[0..],
        };
    }

    pub fn eql(self: *const Self, other: *const Self) bool {
        return std.mem.eql(u8, self.asBytes(), other.asBytes());
    }

    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;

        const slice = self.asBytes();

        // Handle common escapes
        if (slice.len == 1) {
            const c = slice[0];
            switch (c) {
                '\\' => try writer.writeAll("\\\\"),
                '\'' => try writer.writeAll("\\'"),
                '\"' => try writer.writeAll("\\\""),
                '\n' => try writer.writeAll("\\n"),
                '\r' => try writer.writeAll("\\r"),
                '\t' => try writer.writeAll("\\t"),
                0x00 => try writer.writeAll("\\0"),
                else => {
                    if (std.ascii.isPrint(c))
                        try writer.writeByte(c)
                    else {
                        try writer.writeAll("\\x");
                        try std.fmt.formatInt(c, 16, .lower, .{ .width = 2, .fill = '0' }, writer);
                    }
                },
            }
        } else {
            // Assume valid UTF-8 (you can validate if needed)
            try writer.writeAll(slice);
        }
    }

    pub fn isValidUnicodeScalar(u: u32) bool {
        return (u <= 0x10FFFF) and !(u >= 0xD800 and u <= 0xDFFF);
    }
};
