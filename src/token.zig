const std = @import("std");
const lib = @import("root.zig");
const Char = lib.Char;

mod: Mod = .none,
data: union(Kind) {
    eof,
    colon,
    lparen,
    rparen,
    lbrace,
    rbrace,
    lbracket,
    rbracket,
    comma,
    dot,
    dot_dot,
    dot_dot_eq,
    question,
    semicolon,
    bang,

    keyword: Kw,
    int: u64,
    float: f64,
    str: []const u8,
    name: []const u8,
    char: Char,

    mul,
    div,
    mod,
    ampersand,
    lshift,
    rshift,

    add,
    sub,
    xor,
    pipe,

    eq,
    neq,
    lt,
    gt,
    lteq,
    gteq,
    ampersand_ampersand,
    pipe_pipe,

    assign,
    add_assign,
    sub_assign,
    pipe_assign,
    ampersand_assign,
    xor_assign,
    lshift_assign,
    rshift_assign,
    mul_assign,
    div_assign,
    mod_assign,

    inc,
    dec,
},

pub const Kind = enum(u8) {
    eof = 0,
    colon,
    lparen,
    rparen,
    lbrace,
    rbrace,
    lbracket,
    rbracket,
    comma,
    dot,
    dot_dot,
    dot_dot_eq,
    question,
    semicolon,
    bang,

    keyword,
    int,
    float,
    str,
    name,
    char,

    mul,
    div,
    mod,
    ampersand,
    lshift,
    rshift,

    add,
    sub,
    xor,
    pipe,

    eq,
    neq,
    lt,
    gt,
    lteq,
    gteq,
    ampersand_ampersand,
    pipe_pipe,

    assign,
    add_assign,
    sub_assign,
    pipe_assign,
    ampersand_assign,
    xor_assign,
    lshift_assign,
    rshift_assign,
    mul_assign,
    div_assign,
    mod_assign,

    inc,
    dec,

    pub fn isMul(self: @This()) bool {
        return @intFromEnum(self) >= @intFromEnum(@This().mul) and @intFromEnum(self) <= @intFromEnum(@This().rshift);
    }

    pub fn isAdd(self: @This()) bool {
        return @intFromEnum(self) >= @intFromEnum(@This().add) and @intFromEnum(self) <= @intFromEnum(@This().pipe);
    }

    pub fn isCmp(self: @This()) bool {
        return @intFromEnum(self) >= @intFromEnum(@This().eq) and @intFromEnum(self) <= @intFromEnum(@This().gteq);
    }

    pub fn isAssign(self: @This()) bool {
        return @intFromEnum(self) >= @intFromEnum(@This().assign) and @intFromEnum(self) <= @intFromEnum(@This().mod_assign);
    }

    pub fn toString(self: *const @This()) []const u8 {
        return switch (self.*) {
            .eof => "<EOF>",
            .colon => ":",
            .lparen => "(",
            .rparen => ")",
            .lbrace => "{",
            .rbrace => "}",
            .lbracket => "[",
            .rbracket => "]",
            .comma => ",",
            .dot => ".",
            .dot_dot => "..",
            .dot_dot_eq => "..=",
            .question => "?",
            .semicolon => ";",
            .bang => "!",

            .keyword => "keyword",
            .int => "int",
            .float => "float",
            .str => "str",
            .name => "name",
            .char => "char",

            .mul => "+",
            .div => "/",
            .mod => "%",
            .ampersand => "&",
            .lshift => "<<",
            .rshift => ">>",

            .add => "+",
            .sub => "-",
            .xor => "^",
            .pipe => "|",

            .eq => "==",
            .neq => "!=",
            .lt => "<",
            .gt => ">",
            .lteq => "<=",
            .gteq => ">=",
            .ampersand_ampersand => "&&",
            .pipe_pipe => "||",

            .assign => "=",
            .add_assign => "+=",
            .sub_assign => "-=",
            .pipe_assign => "|=",

            .ampersand_assign => "&=",
            .xor_assign => "^=",

            .lshift_assign => "<<=",
            .rshift_assign => ">>=",

            .mul_assign => "*=",
            .div_assign => "/=",
            .mod_assign => "%=",

            .inc => "++",
            .dec => "--",
        };
    }
};

pub const Mod = enum {
    none,
    hex,
    bin,
    oct,

    pub fn base(self: *const @This()) u64 {
        return switch (self.*) {
            .none => 10,
            .hex => 16,
            .bin => 2,
            .oct => 8,
        };
    }

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .none => try std.fmt.format(writer, "none", .{}),
            .hex => try std.fmt.format(writer, "hex", .{}),
            .bin => try std.fmt.format(writer, "bin", .{}),
            .oct => try std.fmt.format(writer, "oct", .{}),
        }
    }
};

pub const Kw = lib.kw.KeywordEnum(&lib.kw.keywords);

const Self = @This();

pub fn init_with_data(comptime k: Kind, args: anytype) Self {
    var self: Self = undefined;

    switch (k) {
        .int => {
            const T = @TypeOf(args);
            const info = @typeInfo(T);
            if (info == .@"struct" and info.@"struct".is_tuple and info.@"struct".fields.len == 2) {
                self.mod = args[0];
                self.data = .{ .int = args[1] };
            } else {
                self.data = @unionInit(@TypeOf(self.data), @tagName(k), args);
            }
        },
        .float => {
            self.data = .{ .float = args };
        },
        .str => {
            self.data = .{ .str = args };
        },
        .name => {
            self.data = .{ .name = args };
        },
        .keyword => {
            self.data = .{ .keyword = args };
        },
        .char => {
            self.data = .{ .char = args };
        },
        else => {
            self.data = @unionInit(@TypeOf(self.data), @tagName(k), {});
        },
    }

    return self;
}

pub fn init(comptime k: Kind) Self {
    var self: Self = undefined;
    self.data = @unionInit(@TypeOf(self.data), @tagName(k), {});
    return self;
}

pub fn kind(self: *const Self) Kind {
    return @as(Self.Kind, self.data);
}

pub fn format_keywords(s: *const Kw, writer: anytype) !void {
    try lib.kw.format_keywords(Kw, &lib.kw.keywords).format_(s, writer);
}

pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
    _ = fmt;
    _ = options;

    switch (self.data) {
        .eof => try writer.writeAll("eof"),
        .keyword => |val| try format_keywords(&val, writer),
        .int => |val| switch (self.mod) {
            .none => try std.fmt.format(writer, "int({d})", .{val}),
            .hex => try std.fmt.format(writer, "int(0x{x})", .{val}),
            .bin => try std.fmt.format(writer, "int(0b{b})", .{val}),
            .oct => try std.fmt.format(writer, "int(0o{o})", .{val}),
        },
        // .int => |val| ,
        .float => |val| try std.fmt.format(writer, "float({})", .{val}),
        .str => |val| try std.fmt.format(writer, "str({s})", .{val}),
        .name => |name| try std.fmt.format(writer, "name(\"{s}\")", .{name}),
        .char => |ch| try std.fmt.format(writer, "char('{}')", .{ch}),
        else => try std.fmt.format(
            writer,
            "{s}",
            .{self.kind().toString()},
        ),
    }
}
