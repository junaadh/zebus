const std = @import("std");
const lib = @import("zebus");

pub fn main() !void {
    const gpa = std.heap.page_allocator;

    const ctxt = try lib.Ctxt.init(gpa);
    _ = ctxt;

    const token = lib.Token.init_with_data(.keyword, .else_);

    std.debug.print("{}", .{token});
}
