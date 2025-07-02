const std = @import("std");
const lib = @import("root.zig");
const ast = lib.ast;
const Decl = ast.Decl;
const Typespec = ast.Typespec;
const Expr = ast.Expr;
const Stmt = ast.Stmt;

ctx: *lib.Ctxt,
lexer: lib.Lexer,

const Self = @This();

pub fn init(input: []const u8, ctx: *lib.Ctxt) Self {
    const lex = lib.Lexer.init(ctx, input);

    return Self{ .ctx = ctx, .lexer = lex };
}

// pub fn parse_type_fn(self: *Self) *Typespec {
//     var args = Typespec.TypespecArray.init(self.ctx.allocator());
// }

// pub fn parse_decl(self: *Self) *ast.Decl {}

test "parse decl" {
    const decls = [_][]const u8{
        "const n = sizeof(:int*[16])",
        "const n = sizeof(1+2)",
        "let x = b == 1 ? 1+2 : 3-4",
        "fn fact(n: int): int { trace(\"fact\"); if (n == 0) { return 1; } else { return n * fact(n-1); } }",
        "fn fact(n: int): int { p := 1; for (i := 1; i <= n; i++) { p *= i; } return p; }",
        "let foo = a ? a&b + c<<d + e*f == +u-v-w + *g/h(x,y) + -i%k[x] && m <= n*(p+q)/r : 0",
        "fn f(x: int): bool { switch(x) { case 0: case 1: return true; case 2: default: return false; } }",
        "enum Color { RED = 3, GREEN, BLUE = 0 }",
        "const pi = 3.14",
        "struct Vector { x, y: float; }",
        "let v = Vector{1.0, -1.0}",
        "let v: Vector = {1.0, -1.0}",
        "union IntOrFloat { i: int; f: float; }",
        "typedef Vectors = Vector[1+2]",
        "fn f() { do { print(42); } while(1); }",
        "typedef T = (fn(int):int)[16]",
        "fn f() { enum E { A, B, C } return 42; }",
        "fn f() { if (1) { return 1; } else if (2) { return 2; } else { return 3; } }",
    };

    // const gpa = std.heap.page_allocator;
    // const ctx = try lib.Ctxt.init(gpa);
    // for (decls) |decl| {
    // var parser = Self.init(decl, ctx);
    // const t = parser.parse_decl();
    // std.debug.print("{}", .{t});
    // }
    _ = decls;
}
