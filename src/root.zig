const std = @import("std");

// symbols
pub const Interner = @import("interner.zig");
pub const Ctxt = @import("context.zig");

// primitive types
const chars = @import("chars.zig");
pub const Char = chars.Char;

// lexical analysis
pub const Token = @import("token.zig");
pub const Lexer = @import("lexer.zig");
pub const kw = @import("keywords.zig");

// semantical analysis
pub const ast = @import("ast.zig");

pub fn fatal(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.print("FATAL: ", .{});
    std.debug.print(fmt ++ "\n", args);
    std.process.exit(1);
}

pub fn syntax_fatal(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.print("FATAL: ", .{});
    std.debug.print(fmt ++ "\n", args);
    std.process.exit(1);
}

pub fn syntax(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("FATAL: ", .{});
    std.debug.print(fmt ++ "\n", args);
}
