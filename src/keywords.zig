const std = @import("std");

pub const keywords = [_][]const u8{
    "typedef", "enum",     "struct", "union", "let",  "const", "fn",  "sizeof",
    "break",   "continue", "return", "if",    "else", "while", "for", "switch",
    "case",    "default",  "in",
};

pub fn sanitize(comptime kws: []const u8) [:0]const u8 {
    const len = kws.len;
    var buf: [len + 1:0]u8 = undefined;

    comptime for (kws, 0..) |c, i| {
        buf[i] = c;
    };

    buf[len] = '_';
    return buf[0.. :0];
}

pub fn KeywordEnum(comptime Names: []const []const u8) type {
    var fields: [Names.len]std.builtin.Type.EnumField = undefined;

    for (Names, 0..) |name, i| {
        fields[i] = .{
            .name = sanitize(name),
            .value = i,
        };
    }

    return @Type(.{
        .@"enum" = .{
            .tag_type = u8,
            .fields = &fields,
            .decls = &.{},
            .is_exhaustive = true,
        },
    });
}

pub fn KeywordMap(comptime Names: []const []const u8, comptime Keywords: type) type {
    return struct {
        pub const Map = std.StringHashMap(Keywords);

        pub fn build(allocator: std.mem.Allocator, interner: anytype) !Map {
            var map = Map.init(allocator);

            inline for (Names, 0..) |name, i| {
                const interned = try interner.intern(name);
                try map.put(interned, @enumFromInt(i));
            }

            return map;
        }
    };
}

pub fn format_keywords(comptime Self: type, comptime Names: []const []const u8) type {
    return struct {
        pub fn format_(self: *const Self, writer: anytype) anyerror!void {
            const index = @intFromEnum(self.*);
            if (index >= Names.len) return error.InvalidEnumValue;
            try writer.writeAll(Names[index]);
        }
    };
}
