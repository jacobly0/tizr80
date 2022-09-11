const std = @import("std");

const CEmuCore = @import("../cemucore.zig");
const Cpu = @import("../cpu.zig");
const decode = @import("decode.zig");
const Interpreter = @This();
const util = @import("../util.zig");

const Error = error{ConditionFailed};

backend: Cpu.Backend,
halted: bool = false,
fetch_cache: u8 = undefined,

pub fn create(allocator: std.mem.Allocator) !*Cpu.Backend {
    const self = try allocator.create(Interpreter);
    errdefer allocator.destroy(self);

    self.* = .{
        .backend = .{
            .execute = execute,
            .destroy = destroy,
        },
    };
    return &self.backend;
}

fn destroy(backend: *Cpu.Backend, allocator: std.mem.Allocator) void {
    const self = @fieldParentPtr(Interpreter, "backend", backend);
    allocator.destroy(self);
}

fn dump(self: *Interpreter, cpu: *Cpu) void {
    std.debug.print(
        \\
        \\AF {X:0>4}     {X:0>4} AF'
        \\BC {X:0>6} {X:0>6} BC'
        \\DE {X:0>6} {X:0>6} DE'
        \\HL {X:0>6} {X:0>6} HL'
        \\IX {X:0>6} {X:0>6} IY
        \\PC {X:0>6}     {X:0>2} R
        \\   {X:0>2} {:10} CC
        \\
    , .{
        cpu.get(.af),
        cpu.getShadow(.af),
        cpu.get(.ubc),
        cpu.getShadow(.ubc),
        cpu.get(.ude),
        cpu.getShadow(.ude),
        cpu.get(.uhl),
        cpu.getShadow(.uhl),
        cpu.get(.uix),
        cpu.get(.uiy),
        cpu.get(.pc),
        cpu.get(.r),
        self.fetch_cache,
        cpu.cycles,
    });
}

fn execute(backend: *Cpu.Backend, core: *CEmuCore, mode: Cpu.ExecuteMode) void {
    const enable_dump = false;
    const self = @fieldParentPtr(Interpreter, "backend", backend);
    var state = State.init(self, core);
    if (backend.flush) {
        backend.flush = false;
        core.cpu.setShadow(.pc, core.cpu.get(.pc));
        state.fetchByte(.prefetch) catch unreachable;
        if (enable_dump) self.dump(&core.cpu);
    }
    if (mode == .flush) return;
    execute: while (!self.halted) {
        decode.decode(&state) catch |err|
            std.debug.assert(@errSetCast(Error, err) == Error.ConditionFailed);
        if (backend.flush) {
            backend.flush = false;
            state.fetchByte(.prefetch) catch unreachable;
        }
        if (enable_dump) self.dump(&core.cpu);
        if (mode == .step) break :execute;
        std.debug.assert(mode == .run);
    }
    if (self.halted) std.debug.print("Doing nothing, halted!\n", .{});
}

const State = struct {
    interp: *Interpreter,
    core: *CEmuCore,
    mode: decode.Mode,
    accumulator: i32 = undefined,
    address: i32 = undefined,

    fn init(interp: *Interpreter, core: *CEmuCore) State {
        return .{
            .interp = interp,
            .core = core,
            .mode = decode.Mode.fromAdl(core.cpu.mode.adl),
        };
    }

    pub fn set(self: *State, comptime value: comptime_int) error{}!void {
        self.accumulator = value;
    }
    pub fn save(self: *State) error{}!void {
        self.address = self.accumulator;
    }
    pub fn restore(self: *State) error{}!void {
        self.accumulator = self.address;
    }
    pub fn swap(self: *State) error{}!void {
        std.mem.swap(i32, &self.accumulator, &self.address);
    }

    fn maskWord(word: u24, mode: decode.Mode.Instruction) u24 {
        return switch (mode) {
            .s => @truncate(u16, word),
            .l => word,
        };
    }
    fn maskAddress(self: *State, address: u24, mode: decode.Mode.Instruction) u24 {
        return switch (mode) {
            .s => util.toBacking(Cpu.u8u16{
                .short = @truncate(u16, address),
                .upper = self.core.cpu.get(.mb),
            }),
            .l => address,
        };
    }

    pub fn maskWordInstruction(self: *State) error{}!void {
        self.accumulator = maskWord(@intCast(u24, self.accumulator), self.mode.inst);
    }
    pub fn maskAddressAdl(self: *State) error{}!void {
        self.address = self.maskAddress(
            @intCast(u24, self.address),
            decode.Mode.Instruction.fromAdl(self.core.cpu.mode.adl),
        );
    }
    pub fn maskAddressInstruction(self: *State) error{}!void {
        self.address = self.maskAddress(@intCast(u24, self.address), self.mode.inst);
    }

    pub fn skip(self: *State) error{}!void {
        self.core.cpu.setShadow(.pc, self.core.cpu.getShadow(.pc) +% 6);
        try self.flush();
    }

    pub fn flush(self: *State) error{}!void {
        self.interp.backend.flush = true;
    }
    pub fn fetchByte(self: *State, comptime prefetch_mode: decode.PrefetchMode) error{}!void {
        const raw_pc = self.core.cpu.getShadow(.pc);
        const pc = self.maskAddress(
            raw_pc,
            decode.Mode.Instruction.fromAdl(self.core.cpu.mode.adl),
        );
        self.accumulator = self.interp.fetch_cache;
        self.core.cpu.set(.pc, pc);
        if (prefetch_mode == .prefetch) {
            self.interp.fetch_cache = self.core.mem.readCpuByte(pc);
            self.core.cpu.setShadow(.pc, raw_pc +% 1);
        }
    }
    pub fn fetchWord(self: *State, comptime prefetch_mode: decode.PrefetchMode) error{}!void {
        var word: i32 = 0;
        try self.fetchByte(.prefetch);
        word |= self.accumulator << 0;
        switch (self.mode.imm) {
            .is => {
                try self.fetchByte(prefetch_mode);
                word |= self.accumulator << 8;
            },
            .il => {
                try self.fetchByte(.prefetch);
                word |= self.accumulator << 8;
                try self.fetchByte(prefetch_mode);
                word |= self.accumulator << 16;
            },
        }
        self.accumulator = word;
    }

    pub fn setMode(self: *State, mode: decode.Mode) error{}!void {
        self.mode = mode;
    }
    pub fn setAdlInstruction(self: *State) error{}!void {
        self.core.cpu.set(.adl, @enumToInt(self.mode.inst));
    }
    pub fn setAdlImmediate(self: *State) error{}!void {
        self.core.cpu.set(.adl, @enumToInt(self.mode.imm));
    }

    pub fn halt(self: *State) error{}!void {
        self.interp.halted = true;
    }

    pub fn addR(self: *State, comptime increment: comptime_int) error{}!void {
        self.core.cpu.r +%= increment << 1;
    }
    pub fn addCycles(self: *State, comptime increment: comptime_int) error{}!void {
        self.core.cpu.cycles +%= increment;
    }

    pub fn dispatch(
        self: *State,
        comptime dispatcher: fn (anytype, comptime u8) anyerror!void,
    ) anyerror!void {
        comptime var opcode = std.math.minInt(u8);
        inline while (true) : (opcode += 1) {
            if (opcode == self.accumulator) return dispatcher(self, opcode) catch |err| return @errSetCast(Error, err);
            if (opcode == std.math.maxInt(u8)) unreachable;
        }
    }

    pub fn checkCondition(
        self: *State,
        comptime address: Cpu.RegisterAddress,
        comptime value: u1,
    ) error{ConditionFailed}!void {
        if (self.core.cpu.get(address) != value) return error.ConditionFailed;
    }

    pub fn loadRegister(self: *State, comptime address: Cpu.RegisterAddress) error{}!void {
        self.accumulator = self.core.cpu.get(address);
    }
    pub fn loadRegisterHigh(self: *State, comptime address: Cpu.RegisterAddress) error{}!void {
        self.accumulator |= @as(i32, self.core.cpu.get(address)) << 8;
    }
    pub fn loadShadowRegister(self: *State, comptime address: Cpu.RegisterAddress) error{}!void {
        self.accumulator = self.core.cpu.getShadow(address);
    }
    pub fn loadStackPointer(self: *State) error{}!void {
        try switch (self.mode.inst) {
            .s => self.loadRegister(.sps),
            .l => self.loadRegister(.spl),
        };
    }

    pub fn storeRegister(self: *State, comptime address: Cpu.RegisterAddress) error{}!void {
        self.core.cpu.set(
            address,
            @truncate(Cpu.RegisterType(address), @intCast(u24, self.accumulator)),
        );
    }
    pub fn storeShadowRegister(self: *State, comptime address: Cpu.RegisterAddress) error{}!void {
        self.core.cpu.setShadow(
            address,
            @truncate(Cpu.RegisterType(address), @intCast(u24, self.accumulator)),
        );
    }
    pub fn storeStackPointer(self: *State) error{}!void {
        try switch (self.mode.inst) {
            .s => self.storeRegister(.sps),
            .l => self.storeRegister(.spl),
        };
    }

    pub fn readPortByte(self: *State) error{}!void {
        self.accumulator = self.core.mem.readCpuPortByte(@intCast(u16, self.address));
    }
    pub fn readMemoryByte(self: *State) error{}!void {
        self.accumulator = self.core.mem.readCpuByte(@intCast(u24, self.address));
    }
    pub fn readMemoryWord(self: *State) error{}!void {
        var word: i32 = 0;
        try self.readMemoryByte();
        word |= self.accumulator << 0;
        self.address += 1;
        try self.maskAddressInstruction();
        try self.readMemoryByte();
        word |= self.accumulator << 8;
        if (self.mode.inst == .l) {
            self.address += 1;
            try self.maskAddressInstruction();
            try self.readMemoryByte();
            word |= self.accumulator << 16;
        }
        self.accumulator = word;
    }
    pub fn writePortByte(self: *State) error{}!void {
        self.core.mem.writeCpuPortByte(@intCast(u16, self.address), @intCast(u8, self.accumulator));
    }
    pub fn writeMemoryByte(self: *State) error{}!void {
        self.core.mem.writeCpuByte(@intCast(u24, self.address), @intCast(u8, self.accumulator));
    }
    pub fn writeMemoryWord(self: *State) error{}!void {
        const word = self.accumulator;
        self.accumulator = @intCast(u8, word >> 0 & 0xFF);
        try self.writeMemoryByte();
        self.address += 1;
        try self.maskAddressInstruction();
        self.accumulator = @intCast(u8, word >> 8 & 0xFF);
        try self.writeMemoryByte();
        if (self.mode.inst == .l) {
            self.address += 1;
            try self.maskAddressInstruction();
            self.accumulator = @intCast(u8, word >> 16 & 0xFF);
            try self.writeMemoryByte();
        }
    }

    pub fn addByte(self: *State, comptime rhs: comptime_int) error{}!void {
        const unsigned_lhs = @intCast(u8, self.accumulator);
        const signed_lhs = @bitCast(i8, unsigned_lhs);
        const signed_rhs = @as(i8, rhs);
        const unsigned_rhs = @bitCast(u8, signed_rhs);
        var signed_result: i8 = undefined;
        var half_result: u4 = undefined;
        self.core.cpu.set(.nf, @boolToInt(signed_rhs < 0));
        self.core.cpu.set(.pv, @boolToInt(
            @addWithOverflow(i8, signed_lhs, signed_rhs, &signed_result),
        ));
        self.core.cpu.set(.hc, @boolToInt(@addWithOverflow(
            u4,
            @truncate(u4, unsigned_lhs),
            @truncate(u4, unsigned_rhs),
            &half_result,
        )) ^ self.core.cpu.get(.nf));
        self.core.cpu.set(.zf, @boolToInt(signed_result == 0));
        self.core.cpu.set(.sf, @boolToInt(signed_result < 0));
        self.accumulator = unsigned_lhs +% unsigned_rhs;
    }
    pub fn addWord(self: *State, comptime offset: comptime_int) error{}!void {
        self.accumulator += offset;
    }

    fn addWordsT(self: *State, comptime T: type) error{}!void {
        const lhs = @truncate(T, @intCast(u24, self.accumulator));
        const rhs = @truncate(T, @intCast(u24, self.address));
        var result: T = undefined;
        self.core.cpu.set(.cf, @boolToInt(@addWithOverflow(T, lhs, rhs, &result)));
        self.core.cpu.set(.nf, 0);
        self.core.cpu.set(.hc, @boolToInt((result & 0xFFF) != (lhs & 0xFFF) + (rhs & 0xFFF)));
        self.accumulator = result;
    }
    pub fn addWords(self: *State) error{}!void {
        try switch (self.mode.inst) {
            .s => self.addWordsT(u16),
            .l => self.addWordsT(u24),
        };
    }
};

fn consumeStrZ(buffer: *[:0]const u8) [:0]const u8 {
    const result = std.mem.sliceTo(buffer.*, 0);
    buffer.* = buffer.*[result.len + 1 ..];
    return result;
}

fn consumeSlice(buffer: *[:0]const u8, len: usize) []const u8 {
    const result = buffer.*[0..len];
    buffer.* = buffer.*[result.len..];
    return result;
}

test "ezex" {
    if (false) return error.SkipZigTest;

    const core = try CEmuCore.create(.{ .allocator = std.testing.allocator, .threading = .SingleThreaded });
    defer core.destroy();

    var all_successful = true;
    var tests: [:0]const u8 = @embedFile("ezex.bin");
    std.debug.print("\n", .{});
    while (tests.len > 0) {
        const name = consumeStrZ(&tests);
        const init = consumeSlice(&tests, 64);
        const count_mask = consumeSlice(&tests, 64);
        const shift_mask = consumeSlice(&tests, 64);
        const expected_hash = consumeSlice(&tests, 32);

        if (std.mem.startsWith(u8, name, "-")) continue;

        var counter = [_]u8{0} ** 64;
        var input: [64]u8 = undefined;
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});

        std.debug.print("{s}... ", .{name});
        std.mem.copy(u8, &input, init);
        while (true) {
            shift: {
                var shift_index: usize = 0;
                var shift_bit: u8 = 0;
                while (true) {
                    input[shift_index] ^= shift_bit;

                    if (input[17] != 0o166) {
                        std.mem.copy(u8, core.mem.cursor[0..8], input[0..8]);
                        std.mem.copy(u8, core.mem.port0[0..6], input[8..14]);
                        std.mem.copy(u8, core.mem.sha256_data[14..16], input[14..16]);
                        core.cpu.setShadow(.adl, @truncate(u1, input[16] >> 0));
                        core.cpu.set(.adl, @truncate(u1, input[16] >> 1));
                        core.mem.cursor[0x100] = if (input[16] >> 2 & 1 != 0) switch (@truncate(u2, input[16] >> 3)) {
                            0 => 0o100,
                            1 => 0o111,
                            2 => 0o122,
                            3 => 0o133,
                        } else 0o000;
                        core.cpu.set(.ief, @truncate(u1, input[16] >> 5));
                        std.mem.copy(u8, core.mem.cursor[0x101..0x106], input[17..22]);
                        std.mem.set(u8, core.mem.cursor[0x106..0x117], 0);
                        core.cpu.set(.i, std.mem.readIntLittle(u16, input[22..24]));
                        core.cpu.set(.mb, std.mem.readIntLittle(u8, input[25..26]));
                        core.cpu.set(.r, std.mem.readIntLittle(u8, input[26..27]));
                        core.cpu.set(.sps, std.mem.readIntLittle(u16, input[28..30]));
                        core.cpu.set(.uiy, std.mem.readIntLittle(u24, input[31..34]));
                        core.cpu.set(.uix, std.mem.readIntLittle(u24, input[34..37]));
                        core.cpu.setShadow(.uhl, std.mem.readIntLittle(u24, input[37..40]));
                        core.cpu.setShadow(.ude, std.mem.readIntLittle(u24, input[40..43]));
                        core.cpu.setShadow(.ubc, std.mem.readIntLittle(u24, input[43..46]));
                        core.cpu.set(.uhl, std.mem.readIntLittle(u24, input[46..49]));
                        core.cpu.set(.ude, std.mem.readIntLittle(u24, input[49..52]));
                        core.cpu.set(.ubc, std.mem.readIntLittle(u24, input[52..55]));
                        core.cpu.setShadow(.af, std.mem.readIntLittle(u16, input[55..57]));
                        core.cpu.set(.af, std.mem.readIntLittle(u16, input[58..60]));
                        core.cpu.set(.spl, std.mem.readIntLittle(u24, input[61..64]));

                        core.cpu.cycles = switch (core.cpu.mode.adl) {
                            .z80 => 0x240,
                            .ez80 => 0x243,
                        };
                        core.cpu.needFlush();
                        core.cpu.set(.pc, 0xE30900);
                        while (core.cpu.get(.pc) < 0xE30906)
                            core.cpu.step();
                        core.cpu.r +%= 0x3F << 1;

                        var output: [64]u8 = undefined;

                        std.mem.copy(u8, output[0..8], core.mem.cursor[0..8]);
                        std.mem.copy(u8, output[8..14], core.mem.port0[0..6]);
                        std.mem.copy(u8, output[14..16], core.mem.sha256_data[14..16]);
                        std.mem.writeIntLittle(
                            u32,
                            output[16..20],
                            @intCast(u32, core.cpu.cycles + @as(u64, switch (core.cpu.mode.adl) {
                                .z80 => 0x117,
                                .ez80 => 0x119,
                            })),
                        );
                        std.mem.writeIntLittle(u8, output[20..21], util.toBacking(Cpu.Flags{
                            .cf = 0,
                            .nf = 0,
                            .pv = core.cpu.getShadow(.ief),
                            .xf = core.cpu.getShadow(.xf),
                            .hc = 0,
                            .yf = core.cpu.getShadow(.yf),
                            .zf = @boolToInt(core.cpu.get(.r) == 0),
                            .sf = @truncate(u1, core.cpu.get(.r) >> 7),
                        }));
                        std.mem.writeIntLittle(u8, output[21..22], core.cpu.get(.r));
                        const i = core.cpu.get(.i);
                        std.mem.writeIntLittle(u16, output[22..24], i +% core.cpu.get(.sps));
                        std.mem.writeIntLittle(u16, output[24..26], i);
                        std.mem.writeIntLittle(u8, output[26..27], core.cpu.get(.mb));
                        std.mem.writeIntLittle(u24, output[27..30], core.cpu.get(.uiy));
                        std.mem.writeIntLittle(u24, output[30..33], core.cpu.get(.uix));
                        std.mem.writeIntLittle(u24, output[33..36], core.cpu.getShadow(.uhl));
                        std.mem.writeIntLittle(u24, output[36..39], core.cpu.getShadow(.ude));
                        std.mem.writeIntLittle(u24, output[39..42], core.cpu.getShadow(.ubc));
                        std.mem.writeIntLittle(u24, output[42..45], core.cpu.get(.uhl));
                        std.mem.writeIntLittle(u24, output[45..48], core.cpu.get(.ude));
                        std.mem.writeIntLittle(u24, output[48..51], core.cpu.get(.ubc));
                        std.mem.writeIntLittle(u24, output[51..54], core.cpu.getShadow(.af));
                        std.mem.writeIntLittle(u24, output[54..57], core.cpu.get(.af));
                        std.mem.writeIntLittle(
                            u32,
                            output[57..61],
                            (@as(u32, core.cpu.get(.pc) +% 0x10) << 8 |
                                @as(u32, core.cpu.getShadow(.adl)) << 1 |
                                @as(u32, core.cpu.get(.adl)) << 0) <<
                                if (core.cpu.get(.adl) == 0) 8 else 0,
                        );
                        std.mem.writeIntLittle(u24, output[61..64], core.cpu.get(.spl));

                        if (false) std.debug.print("\n{}", .{std.fmt.fmtSliceHexUpper(&output)});

                        var offset: usize = 0;
                        while (offset < output.len) : (offset += 4) {
                            var slice = output[offset..][0..4];
                            const value = std.mem.readIntLittle(u32, slice);
                            std.mem.writeIntBig(u32, slice, value);
                        }
                        hasher.update(&output);

                        if (false) {
                            var test_hash: [32]u8 = undefined;
                            for (hasher.s) |state, index|
                                std.mem.writeIntLittle(u32, test_hash[index << 2 ..][0..4], state);
                            std.debug.print("\n{}", .{std.fmt.fmtSliceHexUpper(&test_hash)});
                        }
                    }

                    input[shift_index] ^= shift_bit;
                    while (true) {
                        if (shift_bit == 0) {
                            shift_bit = 1;
                        } else if (@shlWithOverflow(u8, shift_bit, 1, &shift_bit)) {
                            shift_index += 1;
                            if (shift_index >= 64) break :shift;
                            shift_bit = 1;
                        }
                        if (shift_mask[shift_index] & shift_bit != 0) break;
                    }
                }
            }

            count: for (count_mask) |mask, index| {
                var bit: u8 = 1;
                while (true) {
                    if (mask & bit != 0) {
                        counter[index] ^= bit;
                        input[index] ^= bit;
                        if (counter[index] & bit != 0) break :count;
                    }
                    if (@shlWithOverflow(u8, bit, 1, &bit)) break;
                }
            } else break;
        }

        var actual_hash: [32]u8 = undefined;
        for (hasher.s) |state, index|
            std.mem.writeIntLittle(u32, actual_hash[index << 2 ..][0..4], state);
        const success = std.mem.eql(u8, expected_hash, &actual_hash);
        std.debug.print("{s}\n", .{if (success) "OK" else "FAIL"});
        if (!success) {
            if (false) std.debug.print("{}\n{}\n", .{
                std.fmt.fmtSliceHexUpper(expected_hash),
                std.fmt.fmtSliceHexUpper(&actual_hash),
            });
            all_successful = false;
        }
    }
    try std.testing.expect(all_successful);
}
