const std = @import("std");
const lib = @import("root.zig");
const Interner = lib.Interner;

pub const Ctxt = struct {
    gpa: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    interner: Interner,

    const Self = @This();

    pub fn init(gpa: std.mem.Allocator) !*Self {
        const arena = try gpa.create(std.heap.ArenaAllocator);
        arena.* = std.heap.ArenaAllocator.init(gpa);

        const ctxt = try gpa.create(Self);

        ctxt.* = .{
            .gpa = gpa,
            .arena = arena.*,
            .interner = Interner.init(arena.allocator()),
        };

        return ctxt;
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    pub fn allocator(self: *Self) std.mem.Allocator {
        return self.arena.allocator();
    }
};
