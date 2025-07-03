const std = @import("std");
const lib = @import("root.zig");
const ast = lib.ast;
const Decl = ast.Decl;
const Typespec = ast.Typespec;
const Expr = ast.Expr;
const Stmt = ast.Stmt;

ctx: *lib.Ctxt,
lexer: lib.Lexer,
in_for: bool = false,

const Self = @This();

pub fn init(input: []const u8, ctx: *lib.Ctxt) Self {
    const lex = lib.Lexer.init(ctx, input);

    return Self{ .ctx = ctx, .lexer = lex };
}

pub fn parseTypeFn(self: *Self) anyerror!*Typespec {
    var args = Typespec.TypespecArray.init(self.ctx.allocator());
    _ = self.lexer.expect(.lparen);

    if (!self.lexer.is(.rparen)) {
        try args.append(try self.parseType());
        while (self.lexer.matchToken(.comma)) {
            try args.append(try self.parseType());
        }
    }

    _ = self.lexer.expect(.rparen);
    var ret: ?*Typespec = null;
    if (self.lexer.matchToken(.colon)) {
        ret = try self.parseType();
    }

    return Typespec.func(self.ctx, args, ret);
}

pub fn parseTypeBase(self: *Self) anyerror!*Typespec {
    if (self.lexer.is(.name)) {
        const name = self.lexer.token.data.name;
        self.lexer.nextToken();
        return Typespec.name(self.ctx, name);
    } else if (self.lexer.matchKeyword(.fn_)) {
        return self.parseTypeFn();
    } else if (self.lexer.matchToken(.lparen)) {
        const ty = try self.parseType();
        _ = self.lexer.expect(.rparen);
        return ty;
    } else {
        lib.syntax_fatal(@src(), "Unexpected token {} in type", .{self.lexer.token});
    }
}

pub fn parseType(self: *Self) anyerror!*Typespec {
    var ty = try self.parseTypeBase();
    while (self.lexer.is(.lbracket) or self.lexer.is(.mul)) {
        if (self.lexer.matchToken(.lbracket)) {
            var expr: ?*Expr = null;
            if (!self.lexer.is(.rbracket)) {
                expr = try self.parseExpr();
            }

            _ = self.lexer.expect(.rbracket);
            ty = Typespec.array(self.ctx, ty, expr);
        } else {
            self.lexer.assert(.mul);
            ty = Typespec.ptr(self.ctx, ty);
        }
    }

    return ty;
}

pub fn parseExprCompound(self: *Self, ty: ?*Typespec) anyerror!*Expr {
    _ = self.lexer.expect(.lbrace);
    var args = Expr.ExprArray.init(self.ctx.allocator());
    if (!self.lexer.is(.rbrace)) {
        try args.append(try self.parseExpr());
        while (self.lexer.matchToken(.comma)) {
            try args.append(try self.parseExpr());
        }
    }

    _ = self.lexer.expect(.rbrace);
    return Expr.compound(self.ctx, ty, args);
}

pub fn parseExprOperand(self: *Self) anyerror!*Expr {
    if (self.lexer.is(.int)) {
        const value = self.lexer.token.data.int;
        self.lexer.nextToken();
        return Expr.int(self.ctx, value);
    } else if (self.lexer.is(.float)) {
        const value = self.lexer.token.data.float;
        self.lexer.nextToken();
        return Expr.float(self.ctx, value);
    } else if (self.lexer.is(.str)) {
        const value = self.lexer.token.data.str;
        self.lexer.nextToken();
        return Expr.str(self.ctx, value);
    } else if (self.lexer.is(.char)) {
        const value = self.lexer.token.data.char;
        self.lexer.nextToken();
        return Expr.char(self.ctx, value);
    } else if (self.lexer.is(.name)) {
        const value = self.lexer.token.data.name;
        self.lexer.nextToken();
        if (self.lexer.is(.lbrace) and !self.in_for) {
            return self.parseExprCompound(Typespec.name(self.ctx, value));
        } else {
            return Expr.name(self.ctx, value);
        }
    } else if (self.lexer.matchKeyword(.sizeof_)) {
        _ = self.lexer.expect(.lparen);
        if (self.lexer.matchToken(.colon)) {
            const ty = try self.parseType();
            _ = self.lexer.expect(.rparen);
            return Expr.sizeof_type(self.ctx, ty);
        } else {
            const expr = try self.parseExpr();
            _ = self.lexer.expect(.rparen);
            return Expr.sizeof_expr(self.ctx, expr);
        }
    } else if (self.lexer.is(.lbrace)) {
        return self.parseExprCompound(null);
    } else if (self.lexer.matchToken(.lparen)) {
        if (self.lexer.matchToken(.colon)) {
            const ty = try self.parseType();
            _ = self.lexer.expect(.rparen);
            return self.parseExprCompound(ty);
        } else {
            const expr = try self.parseExpr();
            _ = self.lexer.expect(.rparen);
            return expr;
        }
    } else {
        // std.debug.print("debug: {s}\n", .{self.lexer.stream[self.lexer.pos..]});
        lib.syntax_fatal(@src(), "Unexpected token {} in expression", .{self.lexer.token});
    }
}

pub fn parseExprBase(self: *Self) anyerror!*Expr {
    var expr = try self.parseExprOperand();
    while (self.lexer.is(.lparen) or self.lexer.is(.lbracket) or self.lexer.is(.dot)) {
        if (self.lexer.matchToken(.lparen)) {
            var args = Expr.ExprArray.init(self.ctx.allocator());
            if (!self.lexer.is(.rparen)) {
                try args.append(try self.parseExpr());
                while (self.lexer.matchToken(.comma)) {
                    try args.append(try self.parseExpr());
                }
            }
            _ = self.lexer.expect(.rparen);
            expr = Expr.call(self.ctx, expr, args);
        } else if (self.lexer.matchToken(.lbracket)) {
            const index = try self.parseExpr();
            _ = self.lexer.expect(.rbracket);
            expr = Expr.index(self.ctx, expr, index);
        } else {
            self.lexer.assert(.dot);
            const f = self.lexer.token.data.name;
            _ = self.lexer.expect(.name);
            expr = Expr.field(self.ctx, expr, f);
        }
    }

    return expr;
}

pub fn isUnaryOp(self: *Self) bool {
    return self.lexer.is(.add) or self.lexer.is(.sub) or self.lexer.is(.mul) or self.lexer.is(.ampersand);
}

pub fn parseExprUnary(self: *Self) anyerror!*Expr {
    if (self.isUnaryOp()) {
        const op = self.lexer.token.kind();
        self.lexer.nextToken();
        return Expr.unary(self.ctx, op, try self.parseExprUnary());
    } else {
        return self.parseExprBase();
    }
}

pub fn isMulOp(self: *Self) bool {
    return self.lexer.token.kind().isMul();
}

pub fn parseExprMul(self: *Self) anyerror!*Expr {
    var expr = try self.parseExprUnary();

    while (self.isMulOp()) {
        const op = self.lexer.token.kind();
        self.lexer.nextToken();
        expr = Expr.binary(self.ctx, op, expr, try self.parseExprUnary());
    }

    return expr;
}

pub fn isAddOp(self: *Self) bool {
    return self.lexer.token.kind().isAdd();
}

pub fn parseExprAdd(self: *Self) anyerror!*Expr {
    var expr = try self.parseExprMul();

    while (self.isAddOp()) {
        const op = self.lexer.token.kind();
        self.lexer.nextToken();
        expr = Expr.binary(self.ctx, op, expr, try self.parseExprMul());
    }

    return expr;
}

pub fn parseExprRange(self: *Self) anyerror!*Expr {
    var expr: ?*Expr = null;

    if (!(self.lexer.is(.dot_dot) or self.lexer.is(.dot_dot_eq))) {
        expr = try self.parseExprAdd();
    }

    if (self.lexer.is(.dot_dot) or self.lexer.is(.dot_dot_eq)) {
        var end: ?*Expr = null;
        var inclusive = false;

        if (self.lexer.matchToken(.dot_dot)) {
            inclusive = false;
        } else {
            self.lexer.assert(.dot_dot_eq);
            inclusive = true;
        }

        if (!self.lexer.isEof() and !self.lexer.is(.rbracket)) {
            end = try self.parseExprAdd();
        }
        expr = Expr.range(self.ctx, expr, end, inclusive);
    }

    return expr.?;
}

pub fn isCmpOp(self: *Self) bool {
    return self.lexer.token.kind().isCmp();
}

pub fn parseExprCmp(self: *Self) anyerror!*Expr {
    var expr = try self.parseExprRange();

    while (self.isCmpOp()) {
        const op = self.lexer.token.kind();
        self.lexer.nextToken();
        expr = Expr.binary(self.ctx, op, expr, try self.parseExprRange());
    }

    return expr;
}

pub fn parseExprAnd(self: *Self) anyerror!*Expr {
    var expr = try self.parseExprCmp();

    while (self.lexer.matchToken(.ampersand_ampersand)) {
        expr = Expr.binary(self.ctx, .ampersand_ampersand, expr, try self.parseExprCmp());
    }

    return expr;
}

pub fn parseExprOr(self: *Self) anyerror!*Expr {
    var expr = try self.parseExprAnd();

    while (self.lexer.matchToken(.pipe_pipe)) {
        expr = Expr.binary(self.ctx, .pipe_pipe, expr, try self.parseExprAnd());
    }

    return expr;
}

pub fn parseExprTernary(self: *Self) anyerror!*Expr {
    var expr = try self.parseExprOr();

    if (self.lexer.matchToken(.question)) {
        const then_expr = try self.parseExprTernary();
        _ = self.lexer.expect(.colon);
        const else_expr = try self.parseExprTernary();
        expr = Expr.terniary(self.ctx, expr, then_expr, else_expr);
    }

    return expr;
}

pub fn parseExpr(self: *Self) anyerror!*Expr {
    return self.parseExprTernary();
}

pub fn parseExprParen(self: *Self) anyerror!*Expr {
    _ = self.lexer.expect(.lparen);
    const expr = try self.parseExpr();
    _ = self.lexer.expect(.rparen);
    return expr;
}

pub fn parseStmtBlock(self: *Self) anyerror!Stmt.Block {
    _ = self.lexer.expect(.lbrace);
    var stmts = Stmt.StmtArray.init(self.ctx.allocator());
    while (!self.lexer.isEof() and !self.lexer.is(.rbrace)) {
        try stmts.append(try self.parseStmt());
    }
    _ = self.lexer.expect(.rbrace);
    return Stmt.Block{ .stmts = stmts };
}

pub fn parseStmtIf(self: *Self) anyerror!*Stmt {
    const cond = try self.parseExpr();
    const then_block = try self.parseStmtBlock();
    var else_block: Stmt.Block = undefined;

    var else_ifs = Stmt.ElseIfArray.init(self.ctx.allocator());
    while (self.lexer.matchKeyword(.else_)) {
        if (!self.lexer.matchKeyword(.if_)) {
            else_block = try self.parseStmtBlock();
            break;
        }
        const elseif_cond = try self.parseExpr();
        const elseif_block = try self.parseStmtBlock();
        try else_ifs.append(Stmt.ElseIf{ .cond = elseif_cond, .block = elseif_block });
    }

    return Stmt.if_(self.ctx, cond, then_block, else_ifs, else_block);
}

pub fn parseStmtWhile(self: *Self) anyerror!*Stmt {
    const cond = try self.parseExpr();
    return Stmt.while_(self.ctx, cond, try self.parseStmtBlock());
}

pub fn isAssignOp(self: *Self) bool {
    return self.lexer.token.kind().isAssign();
}

pub fn parseSimpleStmt(self: *Self) anyerror!*Stmt {
    const expr = try self.parseExpr();
    var stmt: *Stmt = undefined;

    if (self.isAssignOp()) {
        const op = self.lexer.token.kind();
        self.lexer.nextToken();
        stmt = Stmt.assign(self.ctx, op, expr, try self.parseExpr());
    } else if (self.lexer.is(.inc) or self.lexer.is(.dec)) {
        const op = self.lexer.token.kind();
        self.lexer.nextToken();
        stmt = Stmt.assign(self.ctx, op, expr, null);
    } else {
        stmt = Stmt.expr(self.ctx, expr);
    }

    return stmt;
}

pub fn parseStmtFor(self: *Self) anyerror!*Stmt {
    self.in_for = true;
    const name = try self.parseExpr();
    self.lexer.assertKw(.in_);
    const iter = try self.parseExpr();

    self.in_for = false;

    return Stmt.forEach(self.ctx, name, iter, try self.parseStmtBlock());
}

pub fn parseStmtSwitch(self: *Self) anyerror!Stmt.SwitchCase {
    var exprs = Expr.ExprArray.init(self.ctx.allocator());
    var is_default = false;

    while (self.lexer.isKeyword(.case_) or self.lexer.isKeyword(.default_)) {
        if (self.lexer.matchKeyword(.case_)) {
            try exprs.append(try self.parseExpr());
        } else {
            self.lexer.assertKw(.default_);
            if (is_default) {
                lib.syntax("duplicate default labels in same switch clause", .{});
            }
            is_default = true;
        }
        _ = self.lexer.expect(.colon);
    }

    var stmts = Stmt.StmtArray.init(self.ctx.allocator());
    while (!self.lexer.isEof() and !self.lexer.is(.rbrace) and !self.lexer.isKeyword(.case_) and !self.lexer.isKeyword(.default_)) {
        try stmts.append(try self.parseStmt());
    }

    const block = Stmt.Block{ .stmts = stmts };
    return Stmt.SwitchCase{ .exprs = exprs, .is_default = is_default, .block = block };
}

pub fn parseSwitch(self: *Self) anyerror!*Stmt {
    const expr = try self.parseExprParen();
    var cases = Stmt.SwitchCaseArray.init(self.ctx.allocator());
    _ = self.lexer.expect(.lbrace);
    while (!self.lexer.isEof() and !self.lexer.is(.rbrace)) {
        try cases.append(try self.parseStmtSwitch());
    }
    _ = self.lexer.expect(.rbrace);
    return Stmt.switch_(self.ctx, expr, cases);
}

pub fn parseStmt(self: *Self) anyerror!*Stmt {
    if (self.lexer.matchKeyword(.if_)) {
        return self.parseStmtIf();
    } else if (self.lexer.matchKeyword(.while_)) {
        return self.parseStmtWhile();
    } else if (self.lexer.matchKeyword(.for_)) {
        return self.parseStmtFor();
    } else if (self.lexer.matchKeyword(.switch_)) {
        return self.parseSwitch();
    } else if (self.lexer.is(.lbrace)) {
        return Stmt.block(self.ctx, try self.parseStmtBlock());
    } else if (self.lexer.matchKeyword(.return_)) {
        const stmt = Stmt.return_(self.ctx, try self.parseExpr());
        _ = self.lexer.expect(.semicolon);
        return stmt;
    } else if (self.lexer.matchKeyword(.break_)) {
        _ = self.lexer.expect(.semicolon);
        return Stmt.break_(self.ctx);
    } else if (self.lexer.matchKeyword(.continue_)) {
        _ = self.lexer.expect(.semicolon);
        return Stmt.continue_(self.ctx);
    } else {
        const decl = try self.parseDeclOpt();
        if (decl) |d| {
            return Stmt.decl(self.ctx, d);
        }

        const stmt = try self.parseSimpleStmt();
        _ = self.lexer.expect(.semicolon);
        return stmt;
    }
}

pub fn parseName(self: *Self) []const u8 {
    const name = self.lexer.token.data.name;
    _ = self.lexer.expect(.name);
    return name;
}

pub fn parseDeclEnumItem(self: *Self) anyerror!Decl.EnumItem {
    const name = self.parseName();
    var i: ?*Expr = null;
    if (self.lexer.matchToken(.assign)) {
        i = try self.parseExpr();
    }

    return Decl.EnumItem{ .name = name, .init = i };
}

pub fn parseDeclEnum(self: *Self) anyerror!*Decl {
    const name = self.parseName();
    _ = self.lexer.expect(.lbrace);

    var items = Decl.EnumDecl.init(self.ctx.allocator());
    if (!self.lexer.is(.rbrace)) {
        try items.append(try self.parseDeclEnumItem());
        while (self.lexer.matchToken(.comma)) {
            // std.debug.print("debug: {s}\n", .{self.lexer.stream[self.lexer.pos..]});
            try items.append(try self.parseDeclEnumItem());
        }
    }
    _ = self.lexer.expect(.rbrace);
    return Decl.enum_(self.ctx, name, items);
}

pub fn parseDeclAggregateItem(self: *Self) anyerror!Decl.AggregateItem {
    var names = Decl.StringArray.init(self.ctx.allocator());
    try names.append(self.parseName());
    if (self.lexer.matchToken(.comma)) {
        try names.append(self.parseName());
    }
    _ = self.lexer.expect(.colon);
    const ty = try self.parseType();
    _ = self.lexer.expect(.comma);
    return Decl.AggregateItem{ .names = names, .ty = ty };
}

pub fn parseDeclAggregate(self: *Self, kind: Decl.Kind) anyerror!*Decl {
    std.debug.assert(kind == .struct_ or kind == .union_);
    const name = self.parseName();
    _ = self.lexer.expect(.lbrace);

    var items = Decl.AggregateDecl.init(self.ctx.allocator());
    while (!self.lexer.isEof() and !self.lexer.is(.rbrace)) {
        try items.append(try self.parseDeclAggregateItem());
    }

    _ = self.lexer.expect(.rbrace);

    switch (kind) {
        .struct_ => return Decl.struct_(self.ctx, name, items),
        .union_ => return Decl.union_(self.ctx, name, items),
        else => lib.syntax_fatal(@src(), "expected a struct or enum litera", .{}),
    }
}

pub fn parseDeclLet(self: *Self) anyerror!*Decl {
    const name = self.parseName();
    if (self.lexer.matchToken(.assign)) {
        const expr = try self.parseExpr();
        _ = self.lexer.expect(.semicolon);
        return Decl.let(self.ctx, name, null, expr);
    } else if (self.lexer.matchToken(.colon)) {
        const ty = try self.parseType();
        var expr: ?*Expr = null;
        if (self.lexer.matchToken(.assign)) {
            expr = try self.parseExpr();
        }
        _ = self.lexer.expect(.semicolon);
        return Decl.let(self.ctx, name, ty, expr);
    } else {
        lib.syntax_fatal(@src(), "Expected : or = after let, got {}", .{self.lexer.token});
    }
}

pub fn parseDeclConst(self: *Self) anyerror!*Decl {
    const name = self.parseName();
    _ = self.lexer.expect(.assign);
    return Decl.const_(self.ctx, name, try self.parseExpr());
}

pub fn parseDeclTypeDef(self: *Self) anyerror!*Decl {
    const name = self.parseName();
    _ = self.lexer.expect(.assign);
    return Decl.typedef(self.ctx, name, try self.parseType());
}

pub fn parseDeclFnParam(self: *Self) anyerror!Decl.FuncParam {
    const name = self.parseName();
    _ = self.lexer.expect(.colon);
    return Decl.FuncParam{ .name = name, .ty = try self.parseType() };
}

pub fn parseDeclFn(self: *Self) anyerror!*Decl {
    const name = self.parseName();
    _ = self.lexer.expect(.lparen);
    var params = Decl.FuncParamArray.init(self.ctx.allocator());
    if (!self.lexer.is(.rparen)) {
        try params.append(try self.parseDeclFnParam());
        while (self.lexer.matchToken(.comma)) {
            try params.append(try self.parseDeclFnParam());
        }
    }
    _ = self.lexer.expect(.rparen);
    var ret: ?*Typespec = null;
    if (self.lexer.matchToken(.colon)) {
        ret = try self.parseType();
    }

    const block = try self.parseStmtBlock();
    return Decl.func(self.ctx, name, params, ret, block);
}

pub fn parseDeclOpt(self: *Self) !?*Decl {
    if (self.lexer.matchKeyword(.enum_)) {
        return self.parseDeclEnum();
    } else if (self.lexer.matchKeyword(.struct_)) {
        return self.parseDeclAggregate(.struct_);
    } else if (self.lexer.matchKeyword(.union_)) {
        return self.parseDeclAggregate(.union_);
    } else if (self.lexer.matchKeyword(.let_)) {
        return self.parseDeclLet();
    } else if (self.lexer.matchKeyword(.const_)) {
        return self.parseDeclConst();
    } else if (self.lexer.matchKeyword(.typedef_)) {
        return self.parseDeclTypeDef();
    } else if (self.lexer.matchKeyword(.fn_)) {
        return self.parseDeclFn();
    } else {
        return null;
    }
}

pub fn parse_decl(self: *Self) *Decl {
    const maybe_decl = self.parseDeclOpt() catch unreachable;
    if (maybe_decl) |d| {
        return d;
    } else {
        lib.syntax_fatal(@src(), "Expected declaration keyword, got {}", .{self.lexer.token});
    }
}

test "parse decl" {
    const decls = [_][]const u8{
        "const n = sizeof(:int*[16])",
        "const n = sizeof(1+2)",
        "let x = b == 1 ? 1+2 : 3-4;",
        "fn fact(n: int): int { trace(\"fact\"); if (n == 0) { return 1; } else { return n * fact(n-1); } }",
        "fn fact(n: int): int { let p = 1; for i in 0..n { p *= i; } return p; }",
        "let foo = a ? a&b + c<<d + e*f == +u-v-w + *g/h(x,y) + -i%k[x] && m <= n*(p+q)/r : 0;",
        "fn f(x: int): bool { switch(x) { case 0: case 1: return true; case 2: default: return false; } }",
        "enum Color { RED = 3, GREEN, BLUE = 0 }",
        "const pi = 3.14",
        "struct Vector { x, y: float, }",
        "let v = Vector{1.0, -1.0};",
        "let v: Vector = {1.0, -1.0};",
        "union IntOrFloat { i: int, f: float, }",
        "typedef Vectors = Vector[1+2]",
        "typedef T = (fn(int):int)[16]",
        "let x = example[..];",
        "let x = example[1..];",
        "let x = example[..2];",
        "let x = example[1..2];",
        "let x = example[1..=2];",
        "fn f() { enum E { A, B, C } return 42; }",
        "fn f() { if (1) { return 1; } else if (2) { return 2; } else { return 3; } }",
    };

    const gpa = std.heap.page_allocator;
    const ctx = try lib.Ctxt.init(gpa);
    for (decls, 1..) |decl, i| {
        var parser = Self.init(decl, ctx);
        const t = parser.parse_decl();
        std.debug.print("decl {d}:\n{}\n", .{ i, t });
    }
    // _ = decls;
}
