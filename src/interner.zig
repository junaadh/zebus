const std = @import("std");

map: std.StringHashMap([]const u8),
alloc: std.mem.Allocator,

const Self = @This();

pub fn init(alloc: std.mem.Allocator) Self {
    return .{
        .alloc = alloc,
        .map = std.StringHashMap([]const u8).init(alloc),
    };
}

pub fn createBuffer(self: *Self, capacity: ?usize) !std.ArrayList(u8) {
    var list = std.ArrayList(u8).init(self.alloc);
    if (capacity) |cap| {
        try list.ensureUnusedCapacity(cap);
    }
    return list;
}

pub fn intern(self: *Self, str: []const u8) ![]const u8 {
    if (self.map.get(str)) |existing| return existing;

    const dup = try self.alloc.dupe(u8, str);
    try self.map.put(dup, dup);
    return dup;
}

/// buffer should be created with `Interner.createBuffer(self, capacity)`
///
/// self: should be a mutable interner instance
/// capacity:  should be the assumed len needed to be preallocated
pub fn internOwned(self: *Self, buffer: *std.ArrayList(u8)) ![]const u8 {
    const slice = buffer.items;
    if (self.map.get(slice)) |existing| {
        buffer.deinit();
        return existing;
    }

    const owned = try buffer.toOwnedSlice();
    try self.map.put(owned, owned);
    return owned;
}

pub fn deinit(self: *Self) void {
    self.map.deinit();
}

test "str intern test" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var interner = Self.init(arena.allocator());
    defer interner.deinit();

    var a = [_]u8{ 'h', 'e', 'l', 'l', 'o' };
    const a_str = a[0..];

    const intern_a = try interner.intern(a_str);
    try std.testing.expect(std.mem.eql(u8, a_str, intern_a));
    try std.testing.expect(intern_a.ptr == (try interner.intern(a_str)).ptr);
    try std.testing.expect((try interner.intern(intern_a)).ptr == intern_a.ptr);

    var b = [_]u8{ 'h', 'e', 'l', 'l', 'o' };
    const b_str = b[0..];

    try std.testing.expect(a_str.ptr != b_str.ptr); // separate arrays
    try std.testing.expect((try interner.intern(b_str)).ptr == intern_a.ptr);

    var c = [_]u8{ 'h', 'e', 'l', 'l', 'o', '!' };
    const c_str = c[0..];
    try std.testing.expect((try interner.intern(c_str)).ptr != intern_a.ptr);

    var d = [_]u8{ 'h', 'e', 'l', 'l' };
    const d_str = d[0..];
    try std.testing.expect((try interner.intern(d_str)).ptr != intern_a.ptr);
}
