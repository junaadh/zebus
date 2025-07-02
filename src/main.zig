const std = @import("std");
const lib = @import("zebus");

pub fn main() !void {
    const gpa = std.heap.page_allocator;

    const ctxt = try lib.Ctxt.init(gpa);
    // _ = ctxt;

    // const token = lib.Token.init_with_data(.keyword, .else_);

    // std.debug.print("{}\n", .{token});

    var list = std.ArrayList(*lib.ast.Typespec).init(ctxt.allocator());
    try list.appendSlice(&.{
        lib.ast.Typespec.name(ctxt, "i32"),
        lib.ast.Typespec.name(ctxt, "usize"),
    });

    const ty = lib.ast.Typespec.func(ctxt, list, lib.ast.Typespec.name(ctxt, "damn"));

    std.debug.print("{}\n", .{ty});
}
