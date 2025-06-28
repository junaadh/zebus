const std = @import("std");
const lib = @import("root.zig");
const Interner = lib.Interner;
const Token = lib.Token;
const Char = lib.Char;

pub const Lexer = struct {
    stream: []const u8,
    pos: usize = 0,
    token: Token = .init(.eof),
    interner: *Interner,

    const Self = @This();

    pub fn init(interner: *Interner, stream: []const u8) Self {
        var lex = Self{
            .interner = interner,
            .stream = stream,
        };
        lex.nextToken();
        return lex;
    }

    pub fn reinit(self: *Self, stream: []const u8) void {
        self.stream = stream;
        self.token = undefined;
        self.pos = 0;

        self.nextToken();
    }

    fn peek(self: *const Self) u8 {
        return if (self.pos < self.stream.len) self.stream[self.pos] else 0;
    }

    fn advance(self: *Self) u8 {
        const c = self.peek();
        self.pos += 1;
        return c;
    }

    fn skip_ws(self: *Self) void {
        while (std.ascii.isWhitespace(self.peek())) {
            _ = self.advance();
        }
    }

    pub fn nextToken(self: *Self) void {
        self.skip_ws();
        const start = self.pos;

        const c = self.peek();

        if (c == 0) {
            self.token = .init(.eof);
            return;
        }

        switch (c) {
            ':' => self.case(.{ c, .colon }),
            '(' => self.case(.{ c, .lparen }),
            ')' => self.case(.{ c, .rparen }),
            '{' => self.case(.{ c, .lbrace }),
            '}' => self.case(.{ c, .rbrace }),
            '[' => self.case(.{ c, .lbracket }),
            ']' => self.case(.{ c, .rbracket }),
            ',' => self.case(.{ c, .comma }),

            '.' => {
                if (std.ascii.isDigit(self.stream[self.pos + 1])) {
                    self.scan_float();
                } else {
                    self.token = .init(.dot);
                    _ = self.advance();
                }
            },

            '?' => self.case(.{ c, .question }),
            ';' => self.case(.{ c, .semicolon }),

            '=' => self.case(.{ c, .assign, '=', .eq }),
            '^' => self.case(.{ c, .xor, '=', .xor_assign }),
            '*' => self.case(.{ c, .mul, '=', .mul_assign }),
            '/' => self.case(.{ c, .div, '=', .div_assign }),
            '%' => self.case(.{ c, .mod, '=', .mod_assign }),
            '+' => self.case(.{ c, .add, '=', .add_assign, '+', .inc }),
            '-' => self.case(.{ c, .sub, '=', .sub_assign, '-', .dec }),
            '&' => self.case(.{ c, .ampersand, '=', .ampersand_assign, '&', .ampersand_ampersand }),
            '|' => self.case(.{ c, .pipe, '=', .pipe_assign, '|', .pipe_pipe }),
            '<' => {
                self.token = .init(.lt);
                _ = self.advance();

                if (self.peek() == '<') {
                    self.token = .init(.lshift);
                    _ = self.advance();
                    if (self.peek() == '=') {
                        self.token = .init(.lshift_assign);
                        _ = self.advance();
                    }
                } else if (self.peek() == '=') {
                    self.token = .init(.lteq);
                    _ = self.advance();
                }
            },

            '>' => {
                self.token = .init(.gt);
                _ = self.advance();

                if (self.peek() == '>') {
                    self.token = .init(.rshift);
                    _ = self.advance();
                    if (self.peek() == '=') {
                        self.token = .init(.rshift_assign);
                        _ = self.advance();
                    }
                } else if (self.peek() == '=') {
                    self.token = .init(.gteq);
                    _ = self.advance();
                }
            },

            '"' => self.scan_str(),
            '\'' => self.scan_char(),

            '0'...'9' => {
                while (std.ascii.isDigit(self.peek())) {
                    _ = self.advance();
                }

                const ch = self.peek();
                self.pos = start;

                if (ch == '.' or std.ascii.toLower(ch) == 'e') {
                    self.scan_float();
                } else {
                    self.scan_int();
                }
            },
            'a'...'z', 'A'...'Z', '_' => {
                while (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '_') {
                    _ = self.advance();
                }

                const slice = self.stream[start..self.pos];
                const value = self.interner.intern(slice) catch unreachable;

                self.token = .init_with_data(.name, value);
            },
            else => {
                lib.syntax("unexpected token: '{c}'", .{c});
                _ = self.advance();
            },
        }
    }

    pub fn case(self: *Lexer, pattern: anytype) void {
        const T = @TypeOf(pattern);
        const info = @typeInfo(T);

        if (info != .@"struct" or !info.@"struct".is_tuple)
            @compileError("Expected tuple like .{ 'c', TokenKind, ... } ");

        const fields = info.@"struct".fields;
        const len = fields.len;

        if (len % 2 != 0)
            @compileError("Pattern must contain an even number of items: char-token pairs");

        if (len < 2 or len > 6)
            @compileError("case() only supports 1 to 3 char-token pairs");

        const c1 = @field(pattern, fields[0].name);
        const k1 = @field(pattern, fields[1].name);

        // should never be false
        if (self.advance() != c1) return;
        self.token = .init(k1);

        if (len >= 4) {
            const c2 = @field(pattern, fields[2].name);
            const k2 = @field(pattern, fields[3].name);

            if (self.peek() == c2) {
                _ = self.advance();
                self.token = .init(k2);
            } else if (len == 6) {
                const c3 = @field(pattern, fields[4].name);
                const k3 = @field(pattern, fields[5].name);

                if (self.peek() == c3) {
                    _ = self.advance();
                    self.token = .init(k3);
                }
            }
        }
    }

    fn scan_char(self: *Self) void {
        if (self.peek() != '\'') return;

        _ = self.advance();
        const start = self.pos;
        while (self.peek() != '\'') {
            _ = self.advance();
        }
        const end = self.pos;

        const ch = Char.init(self.stream[start..end]) catch |err| {
            lib.syntax("char parsing error: {}", .{err});
            return;
        };

        if (self.peek() != '\'') {
            lib.syntax("expected closing quote, got '{c}'", .{self.peek()});
        } else {
            _ = self.advance();
        }

        self.token = .init_with_data(.char, ch);
    }

    fn scan_str(self: *Self) void {
        if (self.peek() != '"') return;

        _ = self.advance();

        var buffer = self.interner.createBuffer(null) catch unreachable;

        while (true) {
            const c = self.peek();

            if (c == 0) {
                lib.syntax("Unexpected end of file within string literal", .{});
            } else if (c == '"') {
                _ = self.advance();
                break;
            } else if (c == '\n') {
                lib.syntax("String literal cannot contain newline", .{});
                return;
            } else if (c == '\\') {
                _ = self.advance();
                const esc = self.mapEscape() catch |err| {
                    lib.syntax("Invalid string literal escape '\\{c}': {}", .{ self.peek(), err });
                    break;
                };
                buffer.appendSlice(esc.asBytes()) catch unreachable;
            } else {
                buffer.append(self.peek()) catch unreachable;
                _ = self.advance();
            }
        }

        const slice =
            self.interner.internOwned(&buffer) catch |err| {
                lib.syntax("failed to intern string: {}", .{err});
                return;
            };
        self.token = .init_with_data(.str, slice);
    }

    fn mapEscape(self: *Self) !Char {
        const c = self.peek();

        return switch (c) {
            'n' => {
                _ = self.advance();
                return .{ .one = [1]u8{'\n'} };
            },
            'r' => {
                _ = self.advance();
                return .{ .one = [1]u8{'\r'} };
            },
            't' => {
                _ = self.advance();
                return .{ .one = [1]u8{'\t'} };
            },
            '0' => {
                _ = self.advance();
                return .{ .one = [1]u8{0x00} };
            },
            '\\' => {
                _ = self.advance();
                return .{ .one = [1]u8{'\\'} };
            },
            '"' => {
                _ = self.advance();
                return .{ .one = [1]u8{'"'} };
            },
            '\'' => {
                _ = self.advance();
                return .{ .one = [1]u8{'\''} };
            },

            'u' => {
                _ = self.advance(); // consume 'u'
                if (self.peek() != '{') return error.InvalidUnicodeEscape;
                _ = self.advance(); // consume '{'

                var value: u32 = 0;
                var digits: usize = 0;

                while (true) {
                    const d = self.peek();
                    if (d == '}') break;
                    if (digits >= 6) return error.UnicodeEscapeTooLong;

                    const v = std.fmt.charToDigit(d, 16) catch {
                        return error.InvalidUnicodeDigit;
                    };

                    value = value * 16 + v;
                    digits += 1;
                    _ = self.advance();
                }

                if (digits == 0) return error.UnicodeEscapeEmpty;
                _ = self.advance(); // consume '}'

                // Validate scalar value
                if (!Char.isValidUnicodeScalar(value))
                    return error.InvalidUnicodeScalar;

                // Convert to UTF-8 and return first byte (rest can be handled elsewhere)
                var buf: [4]u8 = undefined;
                const len = std.unicode.utf8Encode(@intCast(value), &buf) catch return error.UnicodeEncodeError;
                const len_usize: usize = @intCast(len);
                return try Char.init(buf[0..len_usize]);
            },

            else => error.UnknownEscape,
        };
    }

    fn scan_float(self: *Lexer) void {
        const start = self.pos;

        while (std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        if (self.peek() == '.') {
            _ = self.advance();
        }

        while (std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        if (std.ascii.toLower(self.peek()) == 'e') {
            _ = self.advance();
            if (self.peek() == '+' or self.peek() == '-') {
                _ = self.advance();
            }

            if (!std.ascii.isDigit(self.peek())) {
                lib.syntax("Expecred digit after float literal exponent, fount '{}'", .{self.peek()});
            }

            while (std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        const float_slice = self.stream[start..self.pos];
        const float_value = std.fmt.parseFloat(f64, float_slice) catch |e| {
            lib.syntax("Float literal parse error: {}", .{e});
            return;
        };
        self.token = .init_with_data(.float, float_value);
    }

    fn scan_int(self: *Self) void {
        self.token.mod = .none;
        if (self.peek() == '0') {
            _ = self.advance();
            switch (std.ascii.toLower(self.peek())) {
                'x' => {
                    _ = self.advance();
                    self.token.mod = .hex;
                },
                'b' => {
                    _ = self.advance();
                    self.token.mod = .bin;
                },
                'o' => {
                    _ = self.advance();
                    self.token.mod = .oct;
                },
                else => |c| {
                    if (std.ascii.isDigit(c)) {
                        self.token.mod = .oct;
                    } else {
                        self.token.mod = .none;
                    }
                },
            }
        }

        var value: u64 = 0;
        const base = self.token.mod.base();
        while (true) {
            const ch = self.peek();

            if (ch == '_') {
                continue;
            }

            const digit = charToDigit(ch) orelse break;
            if (digit == 0 and ch != '0') {
                break;
            }

            if (digit >= base) {
                lib.syntax("Digit '{}' out of range for base {}", .{ ch, base });
                _ = self.advance();
                continue;
            }

            const max: u64 =
                @divTrunc((@as(u64, std.math.maxInt(u64)) - digit), base);
            if (value > max) {
                lib.syntax("integar overflow", .{});
                while (true) {
                    const c = self.peek();
                    const d = charToDigit(c) orelse break;
                    if (d == 0 and c != '0') break;
                    if (d >= base) break;
                    _ = self.advance();
                }
                value = 0;
                break;
            }

            value = value * base + digit;
            _ = self.advance();
        }

        self.token = .init_with_data(.int, .{ self.token.mod, value });
    }

    pub fn charToDigit(c: u8) ?u8 {
        return switch (c) {
            '0'...'9' => c - '0',
            'a'...'f' => 10 + (c - 'a'),
            'A'...'F' => 10 + (c - 'A'),
            else => null,
        };
    }

    pub fn is(self: *Self, kind: Token.Kind) bool {
        return self.token.kind() == kind;
    }

    pub fn isName(self: *Self, name: []const u8) bool {
        if (self.token.data == .name) {
            return std.mem.eql(u8, self.token.data.name, name);
        } else {
            return false;
        }
    }

    pub fn match(self: *Self, kind: Token.Kind) bool {
        if (self.is(kind)) {
            self.nextToken();
            return true;
        }
        return false;
    }

    pub fn expect(self: *Self, kind: Token.Kind) void {
        if (!self.match(kind)) {
            std.debug.panic("expected token {s}, got {s}", .{ kind.toString(), self.token.kind().toString() });
        }
        std.debug.print("found: {s}\n", .{kind.toString()});
    }

    pub fn expectName(self: *Self, name: []const u8) void {
        if (!self.isName(name) and self.match(.name)) {
            std.debug.panic("expected name '{s}', got '{s}'", .{ name, self.token.kind().toString() });
        }
        self.nextToken();
        std.debug.print("found: {s}\n", .{name});
    }

    pub fn expectInt(self: *Self, val: u64) void {
        if (self.token.data == .int) {
            const int = self.token.data.int;
            if (int != val) {
                std.debug.panic("expected int {d}, got {s}", .{ val, self.token.kind().toString() });
            }
        }

        std.debug.print("found: {}\n", .{self.token});
        self.nextToken();
    }

    pub fn expectChar(self: *Self, val: []const u8) void {
        const val_c = Char.init(val) catch unreachable;
        if (self.token.data == .char) {
            const c = self.token.data.char;
            if (!c.eql(&val_c)) {
                std.debug.panic("expected char {}, got {s}", .{ val_c, self.token.kind().toString() });
            }
        }

        std.debug.print("found: {}\n", .{self.token});
        self.nextToken();
    }

    pub fn expectStr(self: *Self, val: []const u8) void {
        if (self.token.data == .str) {
            const str = self.token.data.str;

            if (!std.mem.eql(u8, val, str)) {
                std.debug.panic("expected str {s}, got {s}", .{ val, str });
            }
        }

        std.debug.print("found: {}\n", .{self.token});
        self.nextToken();
    }

    pub fn expectFloat(self: *Self, val: f64) void {
        if (self.token.data == .float) {
            const float = self.token.data.float;
            if (float != val) {
                std.debug.panic("expected float {}, got {s}", .{ val, self.token.kind().toString() });
            }
        }

        std.debug.print("found: {}\n", .{self.token});
        self.nextToken();
    }
};

test "lexer test" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var interner = Interner.init(arena.allocator());
    defer interner.deinit();

    var lexer = Lexer.init(&interner, "XY+(XY)_HELLO1,234+994");

    lexer.expectName("XY");
    lexer.expect(.add);
    lexer.expect(.lparen);
    lexer.expectName("XY");
    lexer.expect(.rparen);
    lexer.expectName("_HELLO1");
    lexer.expect(.comma);
    lexer.expectInt(234);
    lexer.expect(.add);
    lexer.expectInt(994);
    lexer.expect(.eof);

    lexer.reinit("0 18446744073709551615  0xffffffffffffffff 042 0b1111 0o777");
    lexer.expectInt(0);
    std.debug.assert(lexer.token.mod == .none);
    lexer.expectInt(18446744073709551615);
    std.debug.assert(lexer.token.mod == .hex);
    lexer.expectInt(0xffffffffffffffff);
    std.debug.assert(lexer.token.mod == .oct);
    lexer.expectInt(0o42);
    std.debug.assert(lexer.token.mod == .bin);
    lexer.expectInt(15);
    std.debug.assert(lexer.token.mod == .oct);
    lexer.expectInt(0o777);
    lexer.expect(.eof);

    lexer.reinit("3.14 .123 42. 3e10");
    lexer.expectFloat(3.14);
    lexer.expectFloat(0.123);
    lexer.expectFloat(42.0);
    lexer.expectFloat(3e10);
    lexer.expect(.eof);

    lexer.reinit("'a' '\\n' '😑'");
    lexer.expectChar("a");
    lexer.expectChar("\n");
    lexer.expectChar("😑");
    lexer.expect(.eof);

    lexer.reinit("\"foo\" \"a\\nb\" \"🚀🚀🚀\"");
    lexer.expectStr("foo");
    lexer.expectStr("a\nb");
    lexer.expectStr("🚀🚀🚀");
    lexer.expect(.eof);

    lexer.reinit(": += ++ <= << <<=");
    lexer.expect(.colon);
    lexer.expect(.add_assign);
    lexer.expect(.inc);
    lexer.expect(.lteq);
    lexer.expect(.lshift);
    lexer.expect(.lshift_assign);
    lexer.expect(.eof);

    // Character literals — mix of ASCII, escape sequences, and raw unicode
    lexer.reinit("'a' '\\n' '😑' 'λ' '中'");
    lexer.expectChar("a");
    lexer.expectChar("\n");
    lexer.expectChar("😑");
    lexer.expectChar("λ");
    lexer.expectChar("中");
    lexer.expect(.eof);

    // String literals — simple, escaped, and raw Unicode
    lexer.reinit("\"foo\" \"a\\nb\" \"🚀🚀🚀\" \"こんにちは\" \"¡Hola!\" \"汉字\"");
    lexer.expectStr("foo");
    lexer.expectStr("a\nb");
    lexer.expectStr("🚀🚀🚀");
    lexer.expectStr("こんにちは");
    lexer.expectStr("¡Hola!");
    lexer.expectStr("汉字");
    lexer.expect(.eof);

    // === Character Literals ===
    lexer.reinit("'🙂' '©' 'Ω' 'Ж' '\\u{1F525}' '\\u{1F47D}' '\\u{2764}'");
    lexer.expectChar("🙂"); // Smiling face U+1F642
    lexer.expectChar("©"); // Copyright U+00A9
    lexer.expectChar("Ω"); // Greek Omega U+03A9
    lexer.expectChar("Ж"); // Cyrillic Zhe U+0416
    lexer.expectChar("🔥"); // Fire U+1F525
    lexer.expectChar("👽"); // Alien U+1F47D
    lexer.expectChar("❤"); // Heart U+2764
    lexer.expect(.eof);

    // === String Literals ===
    lexer.reinit("\"hi\" \"🌟✨\" \"\\u{1F44D}\\u{1F44E}\" \"😂👍🏽\" \"\\u{1F3C3}\\u{200D}\\u{2642}\\u{FE0F}\"");
    lexer.expectStr("hi");
    lexer.expectStr("🌟✨"); // Glowing star + sparkle
    lexer.expectStr("👍👎"); // Thumbs up/down (escaped)
    lexer.expectStr("😂👍🏽"); // Face with tears of joy + thumbs up with skin tone
    lexer.expectStr("🏃‍♂️"); // Man running (🏃 U+1F3C3 + ZWJ + ♂ U+2642 + FE0F)
    lexer.expect(.eof);
}
