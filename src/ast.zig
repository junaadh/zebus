const std = @import("std");
const lib = @import("root.zig");
const Ctxt = lib.Ctxt;
const Token = lib.Token;

const space = "                                                             ";
var indent: usize = 0;

fn alloc(comptime T: type, ctx: *Ctxt, comptime k: T.Kind, data: anytype) *T {
    const t = ctx.allocator().create(T) catch @panic("compiler intenal error. failed to allocate ast");
    t.*.kind = @unionInit(@TypeOf(t.kind), @tagName(k), data);
    return t;
}

pub const Typespec = struct {
    kind: union(Kind) {
        none,
        name: []const u8,
        func: Func,
        array: Array,
        ptr: *Self,
    },

    const Self = @This();

    pub const Kind = enum {
        none,
        name,
        func,
        array,
        ptr,
    };

    pub const TypespecArray = std.ArrayList(*Self);

    pub const Func = struct {
        args: TypespecArray,
        ret: *Self,
    };

    pub const Array = struct {
        elem: *Self,
        size: *Expr,
    };

    pub fn none(ctx: *Ctxt) *Self {
        return alloc(Self, ctx, .none, {});
    }

    pub fn name(ctx: *Ctxt, id: []const u8) *Self {
        return alloc(Self, ctx, .name, id);
    }

    pub fn func(
        ctx: *Ctxt,
        args: TypespecArray,
        ret: *Self,
    ) *Self {
        return alloc(Self, ctx, .func, Func{ .args = args, .ret = ret });
    }

    pub fn array(
        ctx: *Ctxt,
        elem: *Self,
        size: *Expr,
    ) *Self {
        return alloc(Self, ctx, .array, Array{ .elem = elem, .size = size });
    }

    pub fn ptr(
        ctx: *Ctxt,
        elem: *Self,
    ) *Self {
        return alloc(Self, ctx, .ptr, elem);
    }

    pub fn format(self: @This(), comptime fmt: []const u8, args: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = args;

        switch (self.kind) {
            .name => |value| try std.fmt.format(writer, "{s}", .{value}),
            .func => |value| {
                try std.fmt.format(writer, "(fn (", .{});
                for (value.args.items) |arg| {
                    try std.fmt.format(writer, " {s}", .{arg});
                }
                try std.fmt.format(writer, " ) {} )", .{value.ret});
            },
            .array => |value| try std.fmt.format(writer, "(array {} {})", .{ value.elem, value.size }),
            .ptr => |value| try std.fmt.format(writer, "(ptr {})", .{value}),
            .none => {},
        }
    }
};

pub const Decl = struct {
    name: []const u8,
    kind: union(Kind) {
        none,
        enum_: EnumDecl,
        struct_: AggregateDecl,
        union_: AggregateDecl,
        let: LetDecl,
        const_: *Expr,
        typedef: *Typespec,
        func: FuncDecl,
    },

    const Self = @This();

    pub const Kind = enum {
        none,
        enum_,
        struct_,
        union_,
        let,
        const_,
        typedef,
        func,
    };

    pub const EnumDecl = std.ArrayList(*EnumItem);

    pub const EnumItem = struct {
        name: []const u8,
        init: ?*Expr,
    };

    pub const StringArray = std.ArrayList([]const u8);

    pub const AggregateDecl = std.ArrayList(*AggregateItem);

    pub const AggregateItem = struct {
        names: StringArray,
        ty: *Typespec,
    };

    pub const LetDecl = struct {
        ty: ?*Typespec,
        expr: *Expr,
    };

    pub const FuncDecl = struct {
        params: FuncParamArray,
        ret: ?*Typespec,
        block: Stmt.Block,
    };

    pub const FuncParamArray = std.ArrayList(*FuncParam);

    pub const FuncParam = struct {
        name: []const u8,
        ty: *Typespec,
    };

    pub fn none(ctx: *Ctxt, n: []const u8) *Self {
        const t = alloc(Self, ctx, .none, {});
        t.*.name = n;
        return t;
    }

    pub fn enum_(ctx: *Ctxt, n: []const u8, items: EnumDecl) *Self {
        const t = alloc(Self, ctx, .enum_, items);
        t.*.name = n;
        return t;
    }

    pub fn struct_(ctx: *Ctxt, n: []const u8, items: AggregateDecl) *Self {
        const t = alloc(Self, ctx, .struct_, items);
        t.*.name = n;
        return t;
    }

    pub fn union_(ctx: *Ctxt, n: []const u8, items: AggregateDecl) *Self {
        const t = alloc(Self, ctx, .union_, items);
        t.*.name = n;
        return t;
    }

    pub fn let(ctx: *Ctxt, n: []const u8, ty: ?*Typespec, expr: *Expr) *Self {
        const t = alloc(Self, ctx, .let, LetDecl{ .ty = ty, .expr = expr });
        t.*.name = n;
        return t;
    }

    pub fn const_(ctx: *Ctxt, n: []const u8, expr: *Expr) *Self {
        const t = alloc(Self, ctx, .const_, expr);
        t.*.name = n;
        return t;
    }

    pub fn typedef(ctx: *Ctxt, n: []const u8, ty: *Typespec) *Self {
        const t = alloc(Self, ctx, .typedef, ty);
        t.*.name = n;
        return t;
    }

    pub fn func(ctx: *Ctxt, n: []const u8, params: FuncParamArray, ret: ?*Typespec, block: Stmt.Block) *Self {
        const t = alloc(Self, ctx, .func, FuncDecl{ .params = params, .ret = ret, .block = block });
        t.*.name = n;
        return t;
    }

    fn print_aggregate(d: []*AggregateItem, writer: anytype) !void {
        for (d) |item| {
            try std.fmt.format(writer, "\n{s}({}", .{ space[0 .. 2 * indent], item.ty });
            for (item.names.items) |n| {
                try std.fmt.format(writer, " {s}", .{n});
            }
            try std.fmt.format(writer, ")", .{});
        }
    }

    pub fn format(self: @This(), comptime fmt: []const u8, args: std.fmt.FormatOptions, writer: anytype) !void {
        _ = args;
        _ = fmt;

        switch (self.kind) {
            .none => {},
            .enum_ => |value| {
                try std.fmt.format(writer, "(enum {s}", .{self.name});
                indent += 1;
                for (value.items) |item| {
                    try std.fmt.format(writer, "\n{s}({s} ", .{ space[0 .. 2 * indent], item.name });
                    if (item.init) |v| {
                        try std.fmt.format(writer, "{}", .{v});
                    } else {
                        try std.fmt.format(writer, "nil", .{});
                    }
                    try std.fmt.format(writer, ")", .{});
                }
                indent -= 1;
                try std.fmt.format(writer, ")", .{});
            },
            .struct_ => |value| {
                try std.fmt.format(writer, "(struct {s}", .{self.name});
                indent += 1;
                try print_aggregate(value.items, writer);
                indent -= 1;
                try std.fmt.format(writer, ")", .{});
            },
            .union_ => |value| {
                try std.fmt.format(writer, "(union {s}", .{self.name});
                indent += 1;
                try print_aggregate(value.items, writer);
                indent -= 1;
                try std.fmt.format(writer, ")", .{});
            },
            .let => |value| {
                try std.fmt.format(writer, "(let {s} ", .{self.name});
                if (value.ty) |t| {
                    try std.fmt.format(writer, "{}", .{t});
                } else {
                    try std.fmt.format(writer, "nil", .{});
                }
                try std.fmt.format(writer, " {})", .{value.expr});
            },
            .const_ => |value| {
                try std.fmt.format(writer, "(const {s} {})", .{ self.name, value });
            },
            .typedef => |value| {
                try std.fmt.format(writer, "(typedef {s} {})", .{ self.name, value });
            },
            .func => |value| {
                try std.fmt.format(writer, "(fn {s} (", .{self.name});
                for (value.params.items) |item| {
                    try std.fmt.format(writer, " {s} {}", .{ item.name, item.ty });
                }
                try std.fmt.format(writer, " ) ", .{});
                if (value.ret) |t| {
                    try std.fmt.format(writer, "{}", .{t});
                }

                indent += 1;

                try std.fmt.format(writer, "\n{s}{}", .{ space[0 .. 2 * indent], value.block });

                indent -= 1;
                try std.fmt.format(writer, ")", .{});
            },
        }
    }
};

pub const Expr = struct {
    kind: union(Kind) {
        none,
        char: lib.Char,
        int: u64,
        float: f64,
        str: []const u8,
        name: []const u8,
        cast: Cast,
        call: Call,
        index: Index,
        field: Field,
        compound: Compound,
        unary: Unary,
        binary: Binary,
        terniary: Terniary,
        sizeof_expr: *Expr,
        sizeof_type: *Typespec,
    },

    const Self = @This();

    pub const Kind = enum {
        none,
        char,
        int,
        float,
        str,
        name,
        cast,
        call,
        index,
        field,
        compound,
        unary,
        binary,
        terniary,
        sizeof_expr,
        sizeof_type,
    };

    pub const ExprArray = std.ArrayList(*Expr);

    pub const Cast = struct {
        ty: *Typespec,
        expr: *Expr,
    };

    pub const Call = struct {
        expr: *Expr,
        args: ExprArray,
    };

    pub const Index = struct {
        expr: *Expr,
        index: *Expr,
    };

    pub const Field = struct {
        expr: *Expr,
        name: []const u8,
    };

    pub const Compound = struct {
        ty: ?*Typespec,
        args: ExprArray,
    };

    pub const Unary = struct {
        op: Token.Kind,
        expr: *Expr,
    };

    pub const Binary = struct {
        op: Token.Kind,
        left: *Expr,
        right: *Expr,
    };

    pub const Terniary = struct {
        cond: *Expr,
        then_expr: *Expr,
        else_expr: *Expr,
    };

    pub fn none(ctx: *Ctxt) *Self {
        return alloc(Self, ctx, .none, {});
    }

    pub fn char(ctx: *Ctxt, ch: lib.Char) *Self {
        return alloc(Self, ctx, .char, ch);
    }

    pub fn int(ctx: *Ctxt, i: u64) *Self {
        return alloc(Self, ctx, .int, i);
    }

    pub fn float(ctx: *Ctxt, f: f64) *Self {
        return alloc(Self, ctx, .float, f);
    }

    pub fn str(ctx: *Ctxt, s: []const u8) *Self {
        return alloc(Self, ctx, .str, s);
    }

    pub fn name(ctx: *Ctxt, id: []const u8) *Self {
        return alloc(Self, ctx, .name, id);
    }

    pub fn cast(
        ctx: *Ctxt,
        ty: *Typespec,
        expr: *Expr,
    ) *Self {
        return alloc(Self, ctx, .cast, Cast{ .ty = ty, .expr = expr });
    }

    pub fn call(ctx: *Ctxt, expr: *Expr, args: ExprArray) *Self {
        return alloc(Self, ctx, .call, Call{ .expr = expr, .args = args });
    }

    pub fn index(ctx: *Ctxt, expr: *Expr, idx: *Expr) *Self {
        return alloc(Self, ctx, .index, Index{ .expr = expr, .index = idx });
    }

    pub fn field(ctx: *Ctxt, expr: *Expr, n: []const u8) *Self {
        return alloc(Self, ctx, .field, Field{ .expr = expr, .name = n });
    }

    pub fn compound(ctx: *Ctxt, ty: ?*Typespec, args: ExprArray) *Self {
        return alloc(Self, ctx, .compound, Compound{ .ty = ty, .args = args });
    }

    pub fn unary(ctx: *Ctxt, op: Token.Kind, expr: *Expr) *Self {
        return alloc(Self, ctx, .unary, Unary{ .op = op, .expr = expr });
    }

    pub fn binary(ctx: *Ctxt, op: Token.Kind, left: *Expr, right: *Expr) *Self {
        return alloc(Self, ctx, .binary, Binary{ .op = op, .left = left, .right = right });
    }

    pub fn terniary(ctx: *Ctxt, cond: *Expr, then_expr: *Expr, else_expr: *Expr) *Self {
        return alloc(Self, ctx, .terniary, Terniary{ .cond = cond, .then_expr = then_expr, .else_expr = else_expr });
    }

    pub fn sizeof_expr(ctx: *Ctxt, expr: *Expr) *Self {
        return alloc(Self, ctx, .sizeof_expr, expr);
    }

    pub fn sizeof_type(ctx: *Ctxt, ty: *Typespec) *Self {
        return alloc(Self, ctx, .sizeof_type, ty);
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self.kind) {
            .none => {},
            .char => |value| try std.fmt.format(writer, "{}", .{value}),
            .int => |value| try std.fmt.format(writer, "{d}", .{value}),
            .float => |value| try std.fmt.format(writer, "{}", .{value}),
            .str => |value| try std.fmt.format(writer, "\"{s}\"", .{value}),
            .name => |value| try std.fmt.format(writer, "{s}", .{value}),
            .cast => |value| try std.fmt.format(writer, "(cast {} {})", .{ value.ty, value.expr }),
            .call => |value| {
                try std.fmt.format(writer, "({}", .{value.expr});

                for (value.args.items) |it| {
                    try std.fmt.format(writer, " {}", .{it});
                }

                try std.fmt.format(writer, ")", .{});
            },
            .index => |value| try std.fmt.format(writer, "(index {} {})", .{ value.expr, value.index }),
            .field => |value| try std.fmt.format(writer, "(field {} {s})", .{ value.expr, value.name }),
            .compound => |value| {
                try std.fmt.format(writer, "(compound ", .{});
                if (value.ty) |t| {
                    try std.fmt.format(writer, "{}", .{t});
                }

                for (value.args.items) |it| {
                    try std.fmt.format(writer, " {}", .{it});
                }

                try std.fmt.format(writer, ")", .{});
            },
            .unary => |value| try std.fmt.format(writer, "({s} {})", .{ value.op.toString(), value.expr }),

            .binary => |value| try std.fmt.format(writer, "({s} {} {})", .{ value.op.toString(), value.left, value.right }),
            .terniary => |value| try std.fmt.format(writer, "(? {} {} {})", .{ value.cond, value.then_expr, value.else_expr }),
            .sizeof_expr => |value| try std.fmt.format(writer, "(sizeof-expr {})", .{value}),
            .sizeof_type => |value| try std.fmt.format(writer, "(sizeof-type {})", .{value}),
        }
    }
};

pub const Stmt = struct {
    kind: union(Kind) {
        none,
        decl: *Decl,
        return_: Return_,
        break_,
        continue_,
        block: Block,
        if_: If_,
        while_: While_,
        for_: For_,
        switch_: Switch_,
        assign: Assign,
        init: Init,
        expr: *Expr,
    },

    const Self = @This();

    pub const Kind = enum {
        none,
        decl,
        return_,
        break_,
        continue_,
        block,
        if_,
        while_,
        for_,
        switch_,
        assign,
        init,
        expr,
    };

    pub const Return_ = struct {
        expr: *Expr,
    };

    pub const ElseIf = struct {
        cond: *Expr,
        block: Self.Block,
    };

    pub const ElseIfArray = std.ArrayList(*ElseIf);

    pub const If_ = struct {
        cond: *Expr,
        then_block: Self.Block,
        else_ifs: ElseIfArray,
        else_block: Self.Block,
    };

    pub const While_ = struct {
        cond: *Expr,
        block: Self.Block,
    };

    pub const For_ = struct {
        init: *Self,
        cond: *Expr,
        next: *Self,
        block: Self.Block,
    };

    pub const SwitchCase = struct {
        exprs: Expr.ExprArray,
        is_default: bool,
        block: Self.Block,
    };

    pub const SwitchCaseArray = std.ArrayList(*SwitchCase);

    pub const Switch_ = struct {
        expr: *Expr,
        cases: SwitchCaseArray,
    };

    pub const Assign = struct {
        op: Token.Kind,
        left: *Expr,
        right: ?*Expr,
    };

    pub const Init = struct {
        name: []const u8,
        expr: *Expr,
    };

    pub const StmtArray = std.ArrayList(*Stmt);

    pub const Block = struct {
        stmts: StmtArray,

        pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try std.fmt.format(writer, "(block", .{});
            indent += 1;

            for (self.stmts.items) |it| {
                try std.fmt.format(writer, "\n{s}{}", .{ space[0 .. 2 * indent], it });
            }

            indent -= 1;
            try std.fmt.format(writer, ")", .{});
        }
    };

    pub fn none(ctx: *Ctxt) *Self {
        return alloc(Self, ctx, .none, {});
    }

    pub fn decl(ctx: *Ctxt, d: *Decl) *Self {
        return alloc(Self, ctx, .decl, d);
    }

    pub fn return_(ctx: *Ctxt, r: *Expr) *Self {
        return alloc(Self, ctx, .return_, Return_{ .expr = r });
    }

    pub fn break_(ctx: *Ctxt) *Self {
        return alloc(Self, ctx, .break_, {});
    }

    pub fn continue_(ctx: *Ctxt) *Self {
        return alloc(Self, ctx, .continue_, {});
    }

    pub fn if_(ctx: *Ctxt, cond: *Expr, then_block: Self.Block, else_ifs: ElseIfArray, else_block: Self.Block) *Self {
        return alloc(Self, ctx, .if_, If_{ .cond = cond, .then_block = then_block, .else_ifs = else_ifs, .else_block = else_block });
    }

    pub fn while_(ctx: *Ctxt, cond: *Expr, b: Self.Block) *Self {
        return alloc(Self, ctx, .while_, While_{ .cond = cond, .block = b });
    }

    pub fn for_(ctx: *Ctxt, i: *Expr, cond: *Expr, next: *Expr, b: Self.Block) *Self {
        return alloc(Self, ctx, .for_, For_{ .init = i, .cond = cond, .next = next, .block = b });
    }

    pub fn switch_(ctx: *Ctxt, expr: *Expr, cases: Self.SwitchCaseArray) *Self {
        return alloc(Self, ctx, .switch_, Switch_{ .expr = expr, .cases = cases });
    }

    pub fn assign(ctx: *Ctxt, op: Token.Kind, left: *Expr, right: ?*Expr) *Self {
        return alloc(Self, ctx, .assign, Assign{ .op = op, .left = left, .right = right });
    }

    pub fn init(ctx: *Ctxt, name: []const u8, expr: *Expr) *Self {
        return alloc(Self, ctx, .init, Init{ .name = name, .expr = expr });
    }

    pub fn block(ctx: *Ctxt, stmts: Self.StmtArray) *Self {
        return alloc(Self, ctx, .block, Block{ .stmts = stmts });
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self.kind) {
            .none => {},
            .decl => |value| try std.fmt.format(writer, "{}", .{value}),
            .return_ => |value| try std.fmt.format(writer, "(return {})", .{value.expr}),
            .break_ => try std.fmt.format(writer, "(break)", .{}),
            .continue_ => try std.fmt.format(writer, "(continue)", .{}),
            .block => |value| try std.fmt.format(writer, "{}", value),
            .if_ => |value| {
                try std.fmt.format(writer, "(if {}", .{value.cond});
                indent += 1;

                try std.fmt.format(writer, "\n{s}{}", .{ space[0 .. 2 * indent], value.then_block });

                for (value.else_ifs.items) |it| {
                    try std.fmt.format(writer, "\n{0s}elseif {1} \n{0s}{2}", .{ space[0 .. 2 * indent], it.cond, it.block });
                }

                if (value.else_block.stmts.items.len != 0) {
                    try std.fmt.format(writer, "\n{0s}else \n{0s}{1}", .{ space[0 .. 2 * indent], value.else_block });
                }

                indent -= 1;
                try std.fmt.format(writer, ")", .{});
            },
            .while_ => |value| {
                try std.fmt.format(writer, "(while {}", .{value.cond});
                indent += 1;
                try std.fmt.format(writer, "\n{s}{}", .{ space[0 .. 2 * indent], value.block });
                indent -= 1;
                try std.fmt.format(writer, ")", .{});
            },
            .for_ => |value| {
                try std.fmt.format(writer, "(for {} {} {}", .{ value.init, value.cond, value.next });
                indent += 1;
                try std.fmt.format(writer, "\n{s}{}", .{ space[0 .. 2 * indent], value.block });
                indent -= 1;
                try std.fmt.format(writer, ")", .{});
            },
            .switch_ => |value| {
                try std.fmt.format(writer, "(switch {}", .{value.expr});
                indent += 1;
                for (value.cases.items) |it| {
                    try std.fmt.format(writer, "\n{s}(case {s}", .{ space[0 .. 2 * indent], if (it.is_default) " default" else "" });
                    for (it.exprs.items) |i| {
                        try std.fmt.format(writer, " {}", .{i});
                    }
                    try std.fmt.format(writer, " ) ", .{});
                    indent += 1;
                    try std.fmt.format(writer, "\n{s}{}", .{ space[0 .. 2 * indent], it.block });
                    indent -= 1;
                }
                indent -= 1;
                try std.fmt.format(writer, ")", .{});
            },
            .assign => |value| {
                try std.fmt.format(writer, "({s} {}", .{ value.op.toString(), value.left });

                if (value.right) |t| {
                    try std.fmt.format(writer, " {}", .{t});
                }
                try std.fmt.format(writer, ")", .{});
            },
            .init => |value| try std.fmt.format(writer, "(= {s} {})", .{ value.name, value.expr }),
            .expr => |value| try std.fmt.format(writer, "{}", .{value}),
        }
    }
};

test "typespec creation" {
    const gpa = std.heap.page_allocator;
    const ctx = try Ctxt.init(gpa);

    const x = Typespec.none(ctx);
    std.debug.print("{}\n", .{x.*});
    const y = Typespec.name(ctx, "lmao");
    std.debug.print("{}\n", .{y.*});
    const z = Stmt.assign(ctx, .add_assign, Expr.name(ctx, "hello"), Expr.int(ctx, 69));
    std.debug.print("{}\n", .{z.*});
}
