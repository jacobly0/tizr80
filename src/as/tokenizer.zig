const std = @import("std");

const Tokenizer = @This();

pub const Error = error{
    UnexpectedCharacter,
    UnexpectedEndOfFile,
};

pub const Keyword = enum {
    a,
    adc,
    add,
    af,
    @"af'",
    @"and",
    b,
    bc,
    bit,
    c,
    call,
    ccf,
    cp,
    cpd,
    cpdr,
    cpi,
    cpir,
    cpl,
    d,
    daa,
    db,
    de,
    dec,
    di,
    djnz,
    dl,
    dw,
    e,
    ei,
    ex,
    exx,
    h,
    halt,
    hl,
    i,
    il,
    im,
    in,
    in0,
    inc,
    ind,
    ind2,
    ind2r,
    indm,
    indmr,
    indr,
    indrx,
    ini,
    ini2,
    ini2r,
    inim,
    inimr,
    inir,
    inirx,
    is,
    ix,
    ixh,
    ixl,
    iy,
    iyh,
    iyl,
    jp,
    jr,
    l,
    ld,
    ldd,
    lddr,
    ldi,
    ldir,
    lea,
    lil,
    lis,
    m,
    mb,
    mlt,
    nc,
    neg,
    nop,
    nz,
    @"or",
    org,
    otd2r,
    otdm,
    otdmr,
    otdr,
    otdrx,
    oti2r,
    otim,
    otimr,
    otir,
    otirx,
    out,
    out0,
    outd,
    outd2,
    outi,
    outi2,
    p,
    pe,
    po,
    pop,
    push,
    r,
    res,
    ret,
    reti,
    retn,
    rl,
    rla,
    rlc,
    rlca,
    rld,
    rr,
    rra,
    rrc,
    rrca,
    rrd,
    rsmix,
    rst,
    s,
    sbc,
    scf,
    set,
    sil,
    sis,
    sla,
    slp,
    sp,
    sra,
    srl,
    stmix,
    sub,
    tst,
    xor,
    z,
};

pub const Token = union(enum) {
    eof,
    eol,

    comment: []const u8,
    id: []const u8,
    literal: []const u8,
    keyword: Keyword,

    dollar,
    lparen,
    rparen,
    times,
    plus,
    comma,
    minus,
    dot,
    divide,
    colon,
    backslash,

    pub fn format(
        self: Token,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try switch (self) {
            .eof => writer.print("<{s}>", .{"EOF"}),
            .eol => writer.print("<{s}>", .{"EOL"}),
            .comment => |comment| writer.print("<{s}:{s}>", .{ "COMMENT", comment }),
            .id => |id| writer.print("<{s}:{s}>", .{ "ID", id }),
            .literal => |literal| writer.print("<{s}:{s}>", .{ "LITERAL", literal }),
            .keyword => |keyword| writer.print("<{s}:{s}>", .{ "KEYWORD", @tagName(keyword) }),
            .dollar => writer.print("<{s}:{c}>", .{ "DOLLAR", '$' }),
            .lparen => writer.print("<{s}:{c}>", .{ "LPAREN", '(' }),
            .rparen => writer.print("<{s}:{c}>", .{ "RPAREN", ')' }),
            .times => writer.print("<{s}:{c}>", .{ "TIMES", '*' }),
            .plus => writer.print("<{s}:{c}>", .{ "PLUS", '+' }),
            .comma => writer.print("<{s}:{c}>", .{ "COMMA", ',' }),
            .minus => writer.print("<{s}:{c}>", .{ "MINUS", '-' }),
            .dot => writer.print("<{s}:{c}>", .{ "DOT", '.' }),
            .divide => writer.print("<{s}:{c}>", .{ "DIVIDE", '/' }),
            .colon => writer.print("<{s}:{c}>", .{ "COLON", ':' }),
            .backslash => writer.print("<{s}:{c}>", .{ "BACKSLASH", '\\' }),
        };
    }
};

source: [:0]const u8,
index: usize = 0,
start: usize = 0,
bol: usize = 0,

pub fn init(source: [:0]const u8) Tokenizer {
    return .{ .source = source };
}

pub fn next(self: *Tokenizer) Error!Token {
    const source = self.source;
    var index = self.index;
    defer self.index = index;

    while (true) {
        const start = index;
        self.start = start;
        index += 1;
        switch (source[start]) {
            0 => {
                index -= 1;
                return .eof;
            },
            '\t', ' ' => continue,
            '\n', '\r' => {
                if (source[start] == '\r' and source[index] == '\n') index += 1;
                self.bol = index;
                return .eol;
            },
            '"', '\'' => {
                const quote = source[start];
                while (switch (source[index]) {
                    else => true,
                    0, '\n', '\r' => false,
                }) {
                    const current = index;
                    index += 1;
                    if (source[current] == quote) {
                        if (source[index] != quote) break;
                        index += 1;
                    }
                } else return Error.UnexpectedEndOfFile;
                return .{ .literal = source[start..index] };
            },
            '$', '%', '0'...'9', '@' => {
                while (switch (source[index]) {
                    '0'...'9', 'A'...'Z', 'a'...'z' => true,
                    else => false,
                }) index += 1;
                const literal = source[start..index];
                return if (std.mem.eql(u8, literal, "$")) .dollar else .{ .literal = literal };
            },
            '(' => return .lparen,
            ')' => return .rparen,
            '*' => return .times,
            '+' => return .plus,
            ',' => return .comma,
            '-' => return .minus,
            '.' => return .dot,
            '/' => return .divide,
            ':' => return .colon,
            ';' => {
                while (switch (source[index]) {
                    '\t', ' ' => true,
                    else => false,
                }) index += 1;
                const comment_start = index;
                while (switch (source[index]) {
                    else => true,
                    0, '\n', '\r' => false,
                }) index += 1;
                return .{ .comment = source[comment_start..index] };
            },
            'A'...'Z', '_', 'a'...'z' => {
                while (switch (source[index]) {
                    '\'', '0'...'9', 'a'...'z', 'A'...'Z', '_' => true,
                    else => false,
                }) index += 1;
                const id = source[start..index];
                inline for (@typeInfo(Keyword).Enum.fields) |field|
                    if (std.ascii.eqlIgnoreCase(field.name, id))
                        return .{ .keyword = @field(Keyword, field.name) };
                return .{ .id = id };
            },
            '\\' => return .backslash,
            else => return Error.UnexpectedCharacter,
        }
    }
}

pub fn peek(self: *Tokenizer) Error!Token {
    const index = self.index;
    const token = self.next();
    self.index = index;
    return token;
}

pub fn debugPrintLocation(self: *Tokenizer) void {
    const source = self.source;
    var index = self.bol;
    while (switch (source[index]) {
        else => true,
        0, '\n', '\r' => false,
    }) index += 1;
    std.debug.print("\n{s}\n", .{self.source[self.bol..index]});
    index = self.bol;
    while (index < self.start) : (index += 1) std.debug.print("{c}", .{@as(u8, switch (source[index]) {
        else => ' ',
        '\t' => '\t',
    })});
    std.debug.print("^\n", .{});
}

test "tokenizer" {
    var tokenizer = Tokenizer.init(@embedFile("ez80insts.src"));
    var token: Token = .eol;
    while (token != .eof) {
        token = try tokenizer.peek();
        try std.testing.expectEqual(token, try tokenizer.next());
    }
}
