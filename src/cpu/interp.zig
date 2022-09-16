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
        cpu.getAF(),      cpu.@"af'".word.word,
        cpu.bc.long,      cpu.@"bc'".long,
        cpu.de.long,      cpu.@"de'".long,
        cpu.hl.long,      cpu.@"hl'".long,
        cpu.ix.long,      cpu.iy.long,
        cpu.epc.long,     cpu.getR(),
        self.fetch_cache, cpu.cycles,
    });
}

fn execute(backend: *Cpu.Backend, core: *CEmuCore, mode: Cpu.ExecuteMode) void {
    const enable_dump = false;
    const self = @fieldParentPtr(Interpreter, "backend", backend);
    var state = State.init(self, core);
    if (backend.flush) {
        backend.flush = false;
        core.cpu.pc = core.cpu.epc;
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
    fn effectiveAddress(self: *State, address: u24, mode: decode.Mode.Instruction) u24 {
        return switch (mode) {
            .s => (Cpu.Long{ .ext = .{
                .word = @truncate(u16, address),
                .upper = self.core.cpu.mbi.ext.upper,
            } }).long,
            .l => address,
        };
    }

    pub fn truncate(self: *State, comptime narrow: type) error{}!void {
        self.accumulator = @truncate(narrow, @intCast(u24, self.accumulator));
    }
    pub fn maskWordInstruction(self: *State) error{}!void {
        self.accumulator = maskWord(@intCast(u24, self.accumulator), self.mode.inst);
    }
    fn maskAddress(self: *State, mode: decode.Mode.Instruction) void {
        self.address = self.effectiveAddress(@intCast(u24, self.address), mode);
    }
    pub fn maskAddressAdl(self: *State) error{}!void {
        self.maskAddress(decode.Mode.Instruction.fromAdl(self.core.cpu.mode.adl));
    }
    pub fn maskAddressInstruction(self: *State) error{}!void {
        self.maskAddress(self.mode.inst);
    }

    pub fn skip(self: *State) error{}!void {
        self.core.cpu.pc.long +%= 6;
        try self.flush();
    }

    pub fn flush(self: *State) error{}!void {
        self.interp.backend.flush = true;
    }
    pub fn fetchByte(self: *State, comptime prefetch_mode: decode.PrefetchMode) error{}!void {
        const pc = self.core.cpu.pc.long;
        const epc = self.effectiveAddress(pc, decode.Mode.Instruction.fromAdl(self.core.cpu.mode.adl));
        self.accumulator = self.interp.fetch_cache;
        self.core.cpu.epc.long = epc;
        if (prefetch_mode == .prefetch) {
            self.interp.fetch_cache = self.core.mem.readCpuByte(epc);
            self.core.cpu.pc.long = pc +% 1;
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
        self.core.cpu.mode.adl = switch (self.mode.inst) {
            .s => .z80,
            .l => .ez80,
        };
    }
    pub fn setAdlImmediate(self: *State) error{}!void {
        self.core.cpu.mode.adl = switch (self.mode.imm) {
            .is => .z80,
            .il => .ez80,
        };
    }

    pub fn halt(self: *State) error{}!void {
        self.interp.halted = true;
    }

    pub fn addR(self: *State, comptime increment: comptime_int) error{}!void {
        self.core.cpu.r +%= increment << 1;
    }
    pub fn addRInstruction(self: *State) error{}!void {
        if (self.mode.inst == .l) try self.addR(1);
    }
    pub fn addCycles(self: *State, comptime increment: comptime_int) error{}!void {
        self.core.cpu.cycles +%= increment;
    }
    pub fn addCycleCall(self: *State) error{}!void {
        if (!self.mode.suffix and self.core.cpu.mode.adl == .z80) try self.addCycles(1);
    }

    pub fn dispatch(
        self: *State,
        comptime dispatcher: fn (anytype, comptime u8) anyerror!void,
    ) anyerror!void {
        comptime var opcode = std.math.minInt(u8);
        inline while (true) : (opcode += 1) {
            if (opcode == self.accumulator)
                return dispatcher(self, opcode) catch |err|
                    return @errSetCast(Error, err);
            if (opcode == std.math.maxInt(u8)) unreachable;
        }
    }

    pub fn checkNonZero(self: *State) error{ConditionFailed}!void {
        if (self.accumulator == 0) return error.ConditionFailed;
    }
    pub fn checkCondition(
        self: *State,
        comptime id: Cpu.RegisterId,
        comptime value: bool,
    ) error{ConditionFailed}!void {
        if (self.core.cpu.get(id) != @boolToInt(value)) return error.ConditionFailed;
    }

    pub fn loadRegister(self: *State, comptime id: Cpu.RegisterId) error{}!void {
        self.accumulator = self.core.cpu.get(id);
    }
    pub fn loadRegisterHigh(self: *State, comptime id: Cpu.RegisterId) error{}!void {
        self.accumulator |= @as(i32, self.core.cpu.get(id)) << 8;
    }
    pub fn loadShadowRegister(self: *State, comptime id: Cpu.RegisterId) error{}!void {
        self.accumulator = self.core.cpu.getShadow(id);
    }
    fn loadStackPointer(self: *State, mode: decode.Mode.Instruction) void {
        switch (mode) {
            .s => self.address = self.core.cpu.get(.sps),
            .l => self.address = self.core.cpu.get(.spl),
        }
    }
    pub fn loadStackPointerAdl(self: *State) error{}!void {
        self.loadStackPointer(decode.Mode.Instruction.fromAdl(self.core.cpu.mode.adl));
    }
    pub fn loadStackPointerInstruction(self: *State) error{}!void {
        self.loadStackPointer(self.mode.inst);
    }

    pub fn storeRegister(self: *State, comptime id: Cpu.RegisterId) error{}!void {
        self.core.cpu.set(id, @intCast(u24, self.accumulator));
    }
    pub fn storeShadowRegister(self: *State, comptime id: Cpu.RegisterId) error{}!void {
        self.core.cpu.setShadow(id, @intCast(u24, self.accumulator));
    }
    fn storeStackPointer(self: *State, mode: decode.Mode.Instruction) void {
        switch (mode) {
            .s => self.core.cpu.set(.sps, @truncate(u16, @intCast(u24, self.address))),
            .l => self.core.cpu.set(.spl, @intCast(u24, self.address)),
        }
    }
    pub fn storeStackPointerAdl(self: *State) error{}!void {
        self.storeStackPointer(decode.Mode.Instruction.fromAdl(self.core.cpu.mode.adl));
    }
    pub fn storeStackPointerInstruction(self: *State) error{}!void {
        self.storeStackPointer(self.mode.inst);
    }

    pub fn exchangeRegister(self: *State, comptime id: Cpu.RegisterId) error{}!void {
        var temp = self.accumulator;
        try self.loadRegister(id);
        std.mem.swap(i32, &self.accumulator, &temp);
        try self.storeRegister(id);
        self.accumulator = temp;
    }
    pub fn exchangeRegisterInstruction(
        self: *State,
        comptime partial: Cpu.RegisterId,
        comptime full: Cpu.RegisterId,
    ) error{}!void {
        try switch (self.mode.inst) {
            .s => self.exchangeRegister(partial),
            .l => self.exchangeRegister(full),
        };
    }
    pub fn exchangeShadowRegister(self: *State, comptime id: Cpu.RegisterId) error{}!void {
        var temp = self.accumulator;
        try self.loadShadowRegister(id);
        std.mem.swap(i32, &self.accumulator, &temp);
        try self.storeShadowRegister(id);
        self.accumulator = temp;
    }

    pub fn readPortByte(self: *State) error{}!void {
        self.accumulator = self.core.mem.readCpuPortByte(@intCast(u16, self.address));
    }
    pub fn readMemoryByte(self: *State) error{}!void {
        self.accumulator = self.core.mem.readCpuByte(@intCast(u24, self.address));
    }
    pub fn readMemoryWord(self: *State) error{}!void {
        try self.readMemoryByte();
        var word = self.accumulator << 0;

        try self.addAddress(1);
        try self.maskAddressInstruction();
        try self.readMemoryByte();
        word |= self.accumulator << 8;

        if (self.mode.inst == .l) {
            try self.addAddress(1);
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
    pub fn writeMemoryWord(self: *State, comptime direction: decode.Direction) error{}!void {
        const word = @intCast(u24, self.accumulator);
        switch (direction) {
            .forward => {
                self.accumulator = @truncate(u8, word >> 0);
                try self.writeMemoryByte();

                try self.addAddress(1);
                try self.maskAddressInstruction();
                self.accumulator = @truncate(u8, word >> 8);
                try self.writeMemoryByte();

                if (self.mode.inst == .l) {
                    try self.addAddress(1);
                    try self.maskAddressInstruction();
                    self.accumulator = @truncate(u8, word >> 16);
                    try self.writeMemoryByte();
                }
            },
            .reverse => {
                if (self.mode.inst == .l) {
                    self.accumulator = @truncate(u8, word >> 16);
                    try self.writeMemoryByte();
                    try self.addAddress(-1);
                    try self.maskAddressInstruction();
                }

                self.accumulator = @truncate(u8, word >> 8);
                try self.writeMemoryByte();
                try self.addAddress(-1);
                try self.maskAddressInstruction();

                self.accumulator = @truncate(u8, word >> 0);
                try self.writeMemoryByte();
            },
        }
    }

    pub fn rst(self: *State) error{}!void {
        self.addCycles(1);
        if (self.mode.suffix) {
            const pc = @intCast(u24, self.accumulator);

            if (self.core.cpu.mode.adl == .ez80) {
                self.address = self.core.cpu.spl.long;
                try self.addAddress(-1);
                self.accumulator = @truncate(u8, pc >> 16);
                try self.writeMemoryByte();

                try self.addAddress(-1);
                self.accumulator = util.toBacking(self.core.cpu.mode);
                try self.writeMemoryByte();
                self.core.cpu.spl.long = @intCast(u24, self.address);
            }

            try self.loadStackPointerInstruction();
            try self.addAddress(-1);
            try self.maskAddressInstruction();
            self.accumulator = @truncate(u8, pc >> 8);
            try self.writeMemoryByte();

            try self.addAddress(-1);
            try self.maskAddressInstruction();
            self.accumulator = @truncate(u8, pc >> 0);
            try self.writeMemoryByte();
            try self.storeStackPointerInstruction();

            if (self.core.cpu.mode.adl == .z80) {
                self.address = self.core.cpu.spl.long;
                try self.addAddress(-1);
                self.accumulator = util.toBacking(self.core.cpu.mode);
                try self.writeMemoryByte();
                self.core.cpu.spl.long = @intCast(u24, self.address);
            }
        } else {
            try self.loadStackPointerInstruction();
            try self.addAddress(-1);
            try self.maskAddressInstruction();
            try self.writeMemoryWord(.reverse);
            try self.storeStackPointerInstruction();
        }
    }
    fn call(self: *State, mixed: bool) void {
        if (mixed) {
            const pc = @intCast(u24, self.accumulator);
            if (self.mode.imm == .il) try self.addR(1);

            if (self.core.cpu.mode.adl == .ez80) {
                self.address = self.core.cpu.spl.long;
                try self.addAddress(-1);
                self.accumulator = @truncate(u8, pc >> 16);
                try self.writeMemoryByte();

                if (self.mode.imm == .is) {
                    try self.addAddress(-1);
                    self.accumulator = util.toBacking(self.core.cpu.mode);
                    try self.writeMemoryByte();
                }
                self.core.cpu.spl.long = @intCast(u24, self.address);
            }

            const stack: decode.Mode.Instruction = if (self.mode.imm == .il or
                self.mode.inst == .l and self.core.cpu.mode.adl == .z80) .l else .s;

            self.loadStackPointer(stack);
            try self.addAddress(-1);
            self.maskAddress(stack);
            self.accumulator = @truncate(u8, pc >> 8);
            try self.writeMemoryByte();

            try self.addAddress(-1);
            self.maskAddress(stack);
            self.accumulator = @truncate(u8, pc >> 0);
            try self.writeMemoryByte();
            self.storeStackPointer(stack);

            if (self.mode.imm == .il or self.core.cpu.mode.adl == .z80) {
                self.address = self.core.cpu.spl.long;
                try self.addAddress(-1);
                self.accumulator = util.toBacking(self.core.cpu.mode);
                try self.writeMemoryByte();
                self.core.cpu.spl.long = @intCast(u24, self.address);
            }
        } else {
            try self.loadStackPointerInstruction();
            try self.addAddress(-1);
            try self.maskAddressInstruction();
            try self.writeMemoryWord(.reverse);
            try self.storeStackPointerInstruction();
        }
    }
    pub fn callMadl(self: *State) error{}!void {
        self.call(self.core.cpu.mode.madl == .ez80);
    }
    pub fn callSuffix(self: *State) error{}!void {
        self.call(self.mode.suffix);
    }
    pub fn ret(self: *State) error{}!void {
        if (self.mode.suffix) {
            self.address = self.core.cpu.get(.spl);
            try self.readMemoryByte();
            const adl = @intToEnum(Cpu.Adl, @truncate(u1, @intCast(u8, self.accumulator)));
            try self.addAddress(1);
            self.core.cpu.set(.spl, @intCast(u24, self.address));

            try self.loadStackPointerAdl();
            try self.maskAddressAdl();
            try self.readMemoryByte();
            var pc: i32 = self.accumulator << 0;

            try self.addAddress(1);
            try self.maskAddressAdl();
            try self.readMemoryByte();
            pc |= self.accumulator << 8;

            try self.addAddress(1);
            try self.storeStackPointerAdl();

            if (adl == .ez80) {
                self.address = self.core.cpu.get(.spl);
                try self.readMemoryByte();
                if (self.mode.inst == .l or self.core.cpu.mode.adl == .ez80)
                    pc |= self.accumulator << 16;
                try self.addAddress(1);
                self.core.cpu.set(.spl, @intCast(u24, self.address));
            }

            self.core.cpu.mode.adl = adl;
            self.accumulator = pc;
        } else {
            try self.loadStackPointerInstruction();
            try self.maskAddressInstruction();
            try self.readMemoryWord();
            try self.addAddress(1);
            try self.storeStackPointerInstruction();
        }
    }

    pub fn offsetByte(self: *State, comptime offset: comptime_int) error{}!void {
        self.accumulator = @bitCast(u8, @bitCast(i8, @intCast(u8, self.accumulator)) +% offset);
    }

    pub fn rlcaByte(self: *State) error{}!void {
        const byte = @intCast(u8, self.accumulator);
        self.core.cpu.set(.cf, @truncate(u1, byte >> 7));
        self.core.cpu.nf = false;
        self.core.cpu.hc = false;
        self.accumulator = std.math.rotl(u8, byte, 1);
    }
    pub fn rrcaByte(self: *State) error{}!void {
        const byte = @intCast(u8, self.accumulator);
        self.core.cpu.set(.cf, @truncate(u1, byte >> 0));
        self.core.cpu.nf = false;
        self.core.cpu.hc = false;
        self.accumulator = std.math.rotr(u8, byte, 1);
    }
    pub fn rlaByte(self: *State) error{}!void {
        const carry = @boolToInt(self.core.cpu.cf);
        const byte = @intCast(u8, self.accumulator);
        self.core.cpu.set(.cf, @truncate(u1, byte >> 7));
        self.core.cpu.nf = false;
        self.core.cpu.hc = false;
        self.accumulator = byte << 1 | @as(u8, carry) << 0;
    }
    pub fn rraByte(self: *State) error{}!void {
        const carry = @boolToInt(self.core.cpu.cf);
        const byte = @intCast(u8, self.accumulator);
        self.core.cpu.set(.cf, @truncate(u1, byte >> 0));
        self.core.cpu.nf = false;
        self.core.cpu.hc = false;
        self.accumulator = byte >> 1 | @as(u8, carry) << 7;
    }
    pub fn daaByte(self: *State) error{}!void {
        const byte = @intCast(u8, self.accumulator);
        const low = @truncate(u4, byte);

        const low_offset: u4 = if (self.core.cpu.hc or low > 0x9) 0x10 - 0xA else 0;

        if (@as(u9, byte) + low_offset >> 4 > 0x9) self.core.cpu.cf = true;
        const high_offset: u8 = if (self.core.cpu.cf) 0x10 - 0xA else 0;

        const offset = high_offset << 4 | low_offset;
        var result: u8 = undefined;
        var low_result: u4 = undefined;
        if (self.core.cpu.nf) {
            result = byte -% offset;
            self.core.cpu.hc = @subWithOverflow(u4, low, low_offset, &low_result);
        } else {
            result = byte +% offset;
            self.core.cpu.hc = @addWithOverflow(u4, low, low_offset, &low_result);
        }
        self.core.cpu.pv = @popCount(result) & 1 == 0;
        self.core.cpu.zf = result == 0;
        self.core.cpu.sf = @bitCast(i8, result) < 0;
        self.accumulator = result;
    }
    pub fn cplByte(self: *State) error{}!void {
        self.core.cpu.nf = true;
        self.core.cpu.hc = true;
        self.accumulator = ~@intCast(u8, self.accumulator);
    }
    fn setCarry(self: *State, value: bool) void {
        self.core.cpu.cf = value;
        self.core.cpu.nf = false;
        self.core.cpu.hc = !value;
    }
    pub fn scf(self: *State) error{}!void {
        self.setCarry(true);
    }
    pub fn ccf(self: *State) error{}!void {
        self.setCarry(!self.core.cpu.cf);
    }
    pub fn addByte(self: *State, comptime rhs: comptime_int) error{}!void {
        const unsigned_lhs = @intCast(u8, self.accumulator);
        const signed_rhs: i8 = rhs;
        const unsigned_rhs = @bitCast(u8, signed_rhs);
        self.core.cpu.nf = signed_rhs < 0;

        const signed_lhs = @bitCast(i8, unsigned_lhs);
        var signed_result: i8 = undefined;
        self.core.cpu.pv = @addWithOverflow(i8, signed_lhs, signed_rhs, &signed_result);

        const low_lhs = @truncate(u4, unsigned_lhs);
        const low_rhs = @truncate(u4, unsigned_rhs);
        var low_result: u4 = undefined;
        self.core.cpu.hc = @addWithOverflow(u4, low_lhs, low_rhs, &low_result) != self.core.cpu.nf;

        self.core.cpu.zf = signed_result == 0;
        self.core.cpu.sf = signed_result < 0;
        self.accumulator = @bitCast(u8, signed_result);
    }
    fn addWordByte(word: *i32, byte: i8) void {
        word.* = @bitCast(u24, @bitCast(i24, @intCast(u24, word.*)) +% byte);
    }
    pub fn addWord(self: *State, comptime offset: comptime_int) error{}!void {
        addWordByte(&self.accumulator, offset);
    }
    pub fn addOffset(self: *State) error{}!void {
        addWordByte(&self.accumulator, @bitCast(i8, @intCast(u8, self.address)));
    }
    pub fn addAddress(self: *State, comptime offset: comptime_int) error{}!void {
        addWordByte(&self.address, offset);
    }

    pub fn addBytes(self: *State) error{}!void {
        const unsigned_lhs = @intCast(u8, self.accumulator);
        const unsigned_rhs = @intCast(u8, self.address);
        var unsigned_result: u8 = undefined;
        self.core.cpu.cf = @addWithOverflow(u8, unsigned_lhs, unsigned_rhs, &unsigned_result);

        self.core.cpu.nf = false;

        const signed_lhs = @bitCast(i8, unsigned_lhs);
        const signed_rhs = @bitCast(i8, unsigned_rhs);
        var signed_result: i8 = undefined;
        self.core.cpu.pv = @addWithOverflow(i8, signed_lhs, signed_rhs, &signed_result);

        const low_lhs = @truncate(u4, unsigned_lhs);
        const low_rhs = @truncate(u4, unsigned_rhs);
        var low_result: u4 = undefined;
        self.core.cpu.hc = @addWithOverflow(u4, low_lhs, low_rhs, &low_result);

        self.core.cpu.zf = unsigned_result == 0;
        self.core.cpu.sf = signed_result < 0;
        self.accumulator = unsigned_result;
    }
    pub fn adcBytes(self: *State) error{}!void {
        const carry = @boolToInt(self.core.cpu.cf);

        const unsigned_lhs = @intCast(u8, self.accumulator);
        const unsigned_rhs = @intCast(u8, self.address);
        var unsigned_result: u8 = undefined;
        const cf = @addWithOverflow(u8, unsigned_lhs, unsigned_rhs, &unsigned_result);
        self.core.cpu.cf = @addWithOverflow(u8, unsigned_result, carry, &unsigned_result) or cf;

        self.core.cpu.nf = false;

        const signed_lhs = @bitCast(i8, unsigned_lhs);
        const signed_rhs = @bitCast(i8, unsigned_rhs);
        const signed_result = signed_lhs +% signed_rhs +% carry;
        self.core.cpu.pv = signed_result != @as(i9, signed_lhs) + signed_rhs + carry;

        const low_lhs = @truncate(u4, unsigned_lhs);
        const low_rhs = @truncate(u4, unsigned_rhs);
        var low_result: u4 = undefined;
        self.core.cpu.hc = @addWithOverflow(u4, low_lhs, low_rhs, &low_result) or
            @addWithOverflow(u4, low_result, carry, &low_result);

        self.core.cpu.zf = unsigned_result == 0;
        self.core.cpu.sf = signed_result < 0;
        self.accumulator = unsigned_result;
    }
    pub fn subBytes(self: *State) error{}!void {
        const unsigned_lhs = @intCast(u8, self.accumulator);
        const unsigned_rhs = @intCast(u8, self.address);
        var unsigned_result: u8 = undefined;
        self.core.cpu.cf = @subWithOverflow(u8, unsigned_lhs, unsigned_rhs, &unsigned_result);

        self.core.cpu.nf = true;

        const signed_lhs = @bitCast(i8, unsigned_lhs);
        const signed_rhs = @bitCast(i8, unsigned_rhs);
        var signed_result: i8 = undefined;
        self.core.cpu.pv = @subWithOverflow(i8, signed_lhs, signed_rhs, &signed_result);

        const low_lhs = @truncate(u4, unsigned_lhs);
        const low_rhs = @truncate(u4, unsigned_rhs);
        var low_result: u4 = undefined;
        self.core.cpu.hc = @subWithOverflow(u4, low_lhs, low_rhs, &low_result);

        self.core.cpu.zf = unsigned_result == 0;
        self.core.cpu.sf = signed_result < 0;
        self.accumulator = unsigned_result;
    }
    pub fn sbcBytes(self: *State) error{}!void {
        const carry = @boolToInt(self.core.cpu.cf);

        const unsigned_lhs = @intCast(u8, self.accumulator);
        const unsigned_rhs = @intCast(u8, self.address);
        var unsigned_result: u8 = undefined;
        const cf = @subWithOverflow(u8, unsigned_lhs, unsigned_rhs, &unsigned_result);
        self.core.cpu.cf = @subWithOverflow(u8, unsigned_result, carry, &unsigned_result) or cf;

        self.core.cpu.nf = true;

        const signed_lhs = @bitCast(i8, unsigned_lhs);
        const signed_rhs = @bitCast(i8, unsigned_rhs);
        const signed_result = signed_lhs -% signed_rhs -% carry;
        self.core.cpu.pv = signed_result != @as(i9, signed_lhs) - signed_rhs - carry;

        const low_lhs = @truncate(u4, unsigned_lhs);
        const low_rhs = @truncate(u4, unsigned_rhs);
        var low_result: u4 = undefined;
        self.core.cpu.hc = @subWithOverflow(u4, low_lhs, low_rhs, &low_result) or
            @subWithOverflow(u4, low_result, carry, &low_result);

        self.core.cpu.zf = unsigned_result == 0;
        self.core.cpu.sf = signed_result < 0;
        self.accumulator = unsigned_result;
    }
    pub fn andBytes(self: *State) error{}!void {
        const lhs = @intCast(u8, self.accumulator);
        const rhs = @intCast(u8, self.address);
        const result = lhs & rhs;

        self.core.cpu.cf = false;
        self.core.cpu.nf = false;
        self.core.cpu.pv = @popCount(result) & 1 == 0;
        self.core.cpu.hc = true;
        self.core.cpu.zf = result == 0;
        self.core.cpu.sf = @bitCast(i8, result) < 0;
        self.accumulator = result;
    }
    pub fn xorBytes(self: *State) error{}!void {
        const lhs = @intCast(u8, self.accumulator);
        const rhs = @intCast(u8, self.address);
        const result = lhs ^ rhs;

        self.core.cpu.cf = false;
        self.core.cpu.nf = false;
        self.core.cpu.pv = @popCount(result) & 1 == 0;
        self.core.cpu.hc = false;
        self.core.cpu.zf = result == 0;
        self.core.cpu.sf = @bitCast(i8, result) < 0;
        self.accumulator = result;
    }
    pub fn orBytes(self: *State) error{}!void {
        const lhs = @intCast(u8, self.accumulator);
        const rhs = @intCast(u8, self.address);
        const result = lhs | rhs;

        self.core.cpu.cf = false;
        self.core.cpu.nf = false;
        self.core.cpu.pv = @popCount(result) & 1 == 0;
        self.core.cpu.hc = false;
        self.core.cpu.zf = result == 0;
        self.core.cpu.sf = @bitCast(i8, result) < 0;
        self.accumulator = result;
    }

    fn addWordsT(self: *State, comptime T: type) error{}!void {
        const lhs = @truncate(T, @intCast(u24, self.accumulator));
        const rhs = @truncate(T, @intCast(u24, self.address));
        var result: T = undefined;
        self.core.cpu.cf = @addWithOverflow(T, lhs, rhs, &result);

        self.core.cpu.nf = false;

        const low_lhs = @truncate(u12, lhs);
        const low_rhs = @truncate(u12, rhs);
        var low_result: u12 = undefined;
        self.core.cpu.hc = @addWithOverflow(u12, low_lhs, low_rhs, &low_result);

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

    const core = try CEmuCore.create(.{
        .allocator = std.testing.allocator,
        .threading = .SingleThreaded,
    });
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
                            .cf = false,
                            .nf = false,
                            .pv = core.cpu.ief2,
                            .xf = core.cpu.getShadow(.xf) != 0,
                            .hc = false,
                            .yf = core.cpu.getShadow(.yf) != 0,
                            .zf = core.cpu.get(.r) == 0,
                            .sf = @bitCast(i8, core.cpu.getR()) < 0,
                        }));
                        std.mem.writeIntLittle(u8, output[21..22], core.cpu.getR());
                        const i = core.cpu.mbi.word.word;
                        std.mem.writeIntLittle(u16, output[22..24], i +% core.cpu.sps.word);
                        std.mem.writeIntLittle(u16, output[24..26], i);
                        std.mem.writeIntLittle(u8, output[26..27], core.cpu.mbi.byte.upper);
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
