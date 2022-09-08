const std = @import("std");

const CEmuCore = @import("../cemucore.zig");
const Cpu = @import("../cpu.zig");
const decode = @import("decode.zig").decode;
const Interpreter = @This();
const util = @import("../util.zig");

backend: Cpu.Backend,
halted: bool,
prefetch: u8,

pub fn create(allocator: std.mem.Allocator) !*Cpu.Backend {
    const self = try allocator.create(Interpreter);
    errdefer allocator.destroy(self);

    self.* = .{
        .backend = .{
            .flush = flush,
            .step = step,
            .destroy = destroy,
        },
        .halted = false,
        .prefetch = 0,
    };
    return &self.backend;
}

fn destroy(backend: *Cpu.Backend, allocator: std.mem.Allocator) void {
    const self = @fieldParentPtr(Interpreter, "backend", backend);
    allocator.destroy(self);
}

fn dump(self: *Interpreter, core: *CEmuCore) void {
    if (false) std.debug.print(
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
        core.cpu.get(.af),
        core.cpu.getShadow(.af),
        core.cpu.get(.ubc),
        core.cpu.getShadow(.ubc),
        core.cpu.get(.ude),
        core.cpu.getShadow(.ude),
        core.cpu.get(.uhl),
        core.cpu.getShadow(.uhl),
        core.cpu.get(.uix),
        core.cpu.get(.uiy),
        core.cpu.get(.pc),
        core.cpu.get(.r),
        self.prefetch,
        core.cpu.cycles,
    });
}

fn flush(backend: *Cpu.Backend, core: *CEmuCore) void {
    const self = @fieldParentPtr(Interpreter, "backend", backend);
    core.cpu.raw_pc = core.cpu.pc;
    var state = State.init(self, core);
    state.fetchByte();
    self.dump(core);
}

fn step(backend: *Cpu.Backend, core: *CEmuCore) void {
    const self = @fieldParentPtr(Interpreter, "backend", backend);
    if (self.halted) return std.debug.print("Doing nothing, halted!\n", .{});
    var state = State.init(self, core);
    decode(&state);
    self.dump(core);
}

const State = struct {
    const Mode = struct {
        const Instruction = enum(u1) {
            s,
            l,

            fn fromAdl(adl: Cpu.Adl) Instruction {
                return switch (adl) {
                    .z80 => .s,
                    .ez80 => .l,
                };
            }
        };
        const Immediate = enum(u1) {
            is,
            il,

            fn fromAdl(adl: Cpu.Adl) Immediate {
                return switch (adl) {
                    .z80 => .is,
                    .ez80 => .il,
                };
            }
        };

        inst: Instruction,
        imm: Immediate,
    };

    interp: *Interpreter,
    core: *CEmuCore,
    mode: Mode,
    accumulator: i32 = undefined,
    address: i32 = undefined,

    fn init(interp: *Interpreter, core: *CEmuCore) State {
        return .{
            .interp = interp,
            .core = core,
            .mode = .{
                .inst = State.Mode.Instruction.fromAdl(core.cpu.mode.adl),
                .imm = State.Mode.Immediate.fromAdl(core.cpu.mode.adl),
            },
        };
    }

    pub fn save(self: *State) void {
        self.address = self.accumulator;
    }
    pub fn restore(self: *State) void {
        self.accumulator = self.address;
    }
    pub fn swap(self: *State) void {
        std.mem.swap(i32, &self.accumulator, &self.address);
    }

    fn maskWord(word: u24, mode: Mode.Instruction) u24 {
        return switch (mode) {
            .s => @truncate(u16, word),
            .l => word,
        };
    }
    fn maskAddress(self: *State, address: u24, mode: Mode.Instruction) u24 {
        return switch (mode) {
            .s => util.toBacking(Cpu.u8u16{
                .short = @truncate(u16, address),
                .upper = self.core.cpu.get(.mb),
            }),
            .l => address,
        };
    }

    pub fn maskWordInst(self: *State) void {
        self.accumulator = maskWord(@intCast(u24, self.accumulator), self.mode.inst);
    }
    pub fn maskAddressAdl(self: *State) void {
        self.address = self.maskAddress(
            self.address,
            Mode.Instruction.fromAdl(self.core.cpu.mode.adl),
        );
    }
    pub fn maskAddressInst(self: *State) void {
        self.address = self.maskAddress(@intCast(u24, self.address), self.mode.inst);
    }

    pub fn skip(self: *State) void {
        self.core.cpu.raw_pc +%= 6;
        self.fetchByte();
    }
    pub fn fetchByte(self: *State) void {
        const pc = self.maskAddress(
            self.core.cpu.raw_pc,
            Mode.Instruction.fromAdl(self.core.cpu.mode.adl),
        );
        self.accumulator = self.interp.prefetch;
        self.interp.prefetch = self.core.mem.loadCpuByte(pc);
        self.core.cpu.set(.pc, pc);
        self.core.cpu.raw_pc +%= 1;
    }
    pub fn fetchWord(self: *State) void {
        var word: i32 = 0;
        self.fetchByte();
        word |= self.accumulator << 0;
        self.fetchByte();
        word |= self.accumulator << 8;
        if (self.mode.imm == .il) {
            self.fetchByte();
            word |= self.accumulator << 16;
        }
        self.accumulator = word;
    }

    pub fn setMode(self: *State, inst: Mode.Instruction, imm: Mode.Immediate) void {
        self.mode = .{ .inst = inst, .imm = imm };
    }

    pub fn halt(self: *State) void {
        self.interp.halted = true;
    }

    pub fn addR(self: *State, comptime increment: comptime_int) void {
        self.core.cpu.r +%= increment << 1;
    }
    pub fn addCycles(self: *State, comptime increment: comptime_int) void {
        self.core.cpu.cycles +%= increment;
    }

    pub fn dispatch(self: *State, comptime dispatcher: fn (anytype, comptime u8) void) void {
        comptime var opcode: u8 = 0;
        inline while (true) : (opcode += 1) {
            if (opcode == self.accumulator) return dispatcher(self, opcode);
            if (opcode == std.math.maxInt(u8)) unreachable;
        }
    }

    pub fn readRegister(self: *State, comptime address: Cpu.RegisterAddress) void {
        self.accumulator = self.core.cpu.get(address);
    }
    pub fn readShadowRegister(self: *State, comptime address: Cpu.RegisterAddress) void {
        self.accumulator = self.core.cpu.getShadow(address);
    }
    pub fn readStackPointer(self: *State) void {
        switch (self.mode.inst) {
            .s => self.readRegister(.sps),
            .l => self.readRegister(.spl),
        }
    }

    pub fn writeRegister(self: *State, comptime address: Cpu.RegisterAddress) void {
        self.core.cpu.set(
            address,
            @truncate(Cpu.RegisterType(address), @intCast(u24, self.accumulator)),
        );
    }
    pub fn writeShadowRegister(self: *State, comptime address: Cpu.RegisterAddress) void {
        self.core.cpu.setShadow(
            address,
            @truncate(Cpu.RegisterType(address), @intCast(u24, self.accumulator)),
        );
    }
    pub fn writeStackPointer(self: *State) void {
        switch (self.mode.inst) {
            .s => self.writeRegister(.sps),
            .l => self.writeRegister(.spl),
        }
    }

    pub fn loadByte(self: *State) void {
        self.accumulator = self.core.mem.loadCpuByte(@intCast(u24, self.address));
    }
    pub fn loadWord(self: *State) void {
        var word: i32 = 0;
        self.loadByte();
        word |= self.accumulator << 0;
        self.address += 1;
        self.maskAddressInst();
        self.loadByte();
        word |= self.accumulator << 8;
        if (self.mode.inst == .l) {
            self.address += 1;
            self.maskAddressInst();
            self.loadByte();
            word |= self.accumulator << 16;
        }
        self.accumulator = word;
    }
    pub fn storeByte(self: *State) void {
        self.core.mem.storeCpuByte(@intCast(u24, self.address), @intCast(u8, self.accumulator));
    }
    pub fn storeWord(self: *State) void {
        const word = self.accumulator;
        self.accumulator = @intCast(u8, word >> 0 & 0xFF);
        self.storeByte();
        self.address += 1;
        self.maskAddressInst();
        self.accumulator = @intCast(u8, word >> 8 & 0xFF);
        self.storeByte();
        if (self.mode.inst == .l) {
            self.address += 1;
            self.maskAddressInst();
            self.accumulator = @intCast(u8, word >> 16 & 0xFF);
            self.storeByte();
        }
    }

    pub fn addByte(self: *State, comptime offset: comptime_int) void {
        const input = @bitCast(i8, @intCast(u8, self.accumulator));
        const result = input +% offset;
        self.core.cpu.set(.nf, @boolToInt(offset < 0));
        self.core.cpu.set(.pv, @boolToInt(result != @as(i32, input) + offset));
        self.core.cpu.set(.hc, @boolToInt(
            result & 0xF != (input & 0xF) + offset,
        ));
        self.core.cpu.set(.zf, @boolToInt(result == 0));
        self.core.cpu.set(.sf, @boolToInt(result < 0));
        self.accumulator = @bitCast(u8, result);
    }
    pub fn addWord(self: *State, comptime offset: comptime_int) void {
        self.accumulator += offset;
    }

    fn addWordsT(self: *State, comptime T: type) void {
        const lhs = @truncate(T, @intCast(u24, self.accumulator));
        const rhs = @truncate(T, @intCast(u24, self.address));
        var result: T = undefined;
        self.core.cpu.set(.cf, @boolToInt(@addWithOverflow(T, lhs, rhs, &result)));
        self.core.cpu.set(.nf, 0);
        self.core.cpu.set(.hc, @boolToInt((result & 0xFFF) != (lhs & 0xFFF) + (rhs & 0xFFF)));
        self.accumulator = result;
    }
    pub fn addWords(self: *State) void {
        switch (self.mode.inst) {
            .s => self.addWordsT(u16),
            .l => self.addWordsT(u24),
        }
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
                        core.cpu.set(.madl, @truncate(u1, input[16] >> 0));
                        core.cpu.set(.adl, @truncate(u1, input[16] >> 1));
                        core.mem.cursor[0x100] = if (input[16] >> 2 & 1 != 0) switch (@truncate(u2, input[16] >> 3)) {
                            0 => 0o100,
                            1 => 0o111,
                            2 => 0o122,
                            3 => 0o133,
                        } else 0o000;
                        core.cpu.set(.ief, @truncate(u1, input[16] >> 5));
                        std.mem.copy(u8, core.mem.cursor[0x101..0x106], input[17..22]);
                        std.mem.set(u8, core.mem.cursor[0x106..0x10D], 0);
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

                        core.cpu.cycles = 0;
                        core.cpu.set(.pc, 0xE30900);
                        core.cpu.needFlush();
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
                                .z80 => 0x357,
                                .ez80 => 0x35C,
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
                                @as(u32, core.cpu.get(.madl)) << 1 |
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
