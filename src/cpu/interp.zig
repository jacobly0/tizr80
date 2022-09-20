const std = @import("std");

const Cpu = @import("../cpu.zig");
const Decode = @import("decode.zig");
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

fn execute(backend: *Cpu.Backend, cpu: *Cpu, mode: Cpu.ExecuteMode) void {
    const enable_dump = false;
    const self = @fieldParentPtr(Interpreter, "backend", backend);
    var state = State{ .interp = self, .cpu = cpu };
    try state.reset();
    if (backend.flush) {
        backend.flush = false;
        state.cpu.pc = state.cpu.epc;
        state.fetchByte(.prefetch) catch unreachable;
        if (enable_dump) state.dump();
    }
    if (mode == .flush) return;
    execute: while (!self.halted) {
        Decode.decode(&state) catch |err|
            std.debug.assert(@errSetCast(Error, err) == Error.ConditionFailed);
        if (backend.flush) {
            backend.flush = false;
            state.fetchByte(.prefetch) catch unreachable;
        }
        if (enable_dump) state.dump();
        if (mode == .step) break :execute;
        std.debug.assert(mode == .run);
    }
    if (self.halted) std.debug.print("Doing nothing, halted!\n", .{});
}

const State = struct {
    interp: *Interpreter,
    cpu: *Cpu,
    mode: Decode.Mode = undefined,
    accumulator: i32 = undefined,
    address: i32 = undefined,
    repeat_counter: u1 = 0,

    fn dump(self: *State) void {
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
            self.cpu.getAF(),        self.cpu.@"af'".word.word,
            self.cpu.bc.long,        self.cpu.@"bc'".long,
            self.cpu.de.long,        self.cpu.@"de'".long,
            self.cpu.hl.long,        self.cpu.@"hl'".long,
            self.cpu.ix.long,        self.cpu.iy.long,
            self.cpu.epc.long,       self.cpu.getR(),
            self.interp.fetch_cache, self.cpu.cycles,
        });
    }

    pub fn reset(self: *State) error{}!void {
        self.mode = Decode.Mode.fromAdl(self.cpu.mode.adl);
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

    fn maskWord(word: u24, mode: Decode.Mode.Instruction) u24 {
        return switch (mode) {
            .s => @truncate(u16, word),
            .l => word,
        };
    }
    fn effectiveAddress(self: *State, address: u24, mode: Decode.Mode.Instruction) u24 {
        return switch (mode) {
            .s => (Cpu.Long{ .ext = .{
                .word = @truncate(u16, address),
                .upper = self.cpu.mbi.ext.upper,
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
    fn maskAddress(self: *State, mode: Decode.Mode.Instruction) void {
        self.address = self.effectiveAddress(@intCast(u24, self.address), mode);
    }
    pub fn maskAddressAdl(self: *State) error{}!void {
        self.maskAddress(Decode.Mode.Instruction.fromAdl(self.cpu.mode.adl));
    }
    pub fn maskAddressInstruction(self: *State) error{}!void {
        self.maskAddress(self.mode.inst);
    }

    pub fn skip(self: *State) error{}!void {
        self.cpu.pc.long +%= 6;
        try self.flush();
    }

    pub fn flush(self: *State) error{}!void {
        self.interp.backend.flush = true;
    }
    pub fn fetchByte(self: *State, comptime prefetch_mode: Decode.PrefetchMode) error{}!void {
        const pc = self.cpu.pc.long;
        const epc = self.effectiveAddress(pc, Decode.Mode.Instruction.fromAdl(self.cpu.mode.adl));
        self.accumulator = self.interp.fetch_cache;
        self.cpu.epc.long = epc;
        if (prefetch_mode == .prefetch) {
            self.interp.fetch_cache = self.cpu.read(epc);
            self.cpu.pc.long = pc +% 1;
        }
    }
    pub fn fetchWord(self: *State, comptime prefetch_mode: Decode.PrefetchMode) error{}!void {
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

    pub fn setMode(self: *State, mode: Decode.Mode) error{}!void {
        self.mode = mode;
    }
    pub fn setAdlInstruction(self: *State) error{}!void {
        self.cpu.mode.adl = switch (self.mode.inst) {
            .s => .z80,
            .l => .ez80,
        };
    }
    pub fn setAdlImmediate(self: *State) error{}!void {
        self.cpu.mode.adl = switch (self.mode.imm) {
            .is => .z80,
            .il => .ez80,
        };
    }
    pub fn setInterruptMode(self: *State, comptime im: comptime_int) error{}!void {
        self.cpu.set(.im, im);
    }

    pub fn halt(self: *State) error{}!void {
        self.interp.halted = true;
    }

    pub fn addR(self: *State, comptime increment: comptime_int) error{}!void {
        self.cpu.addR(increment);
    }
    pub fn addRInstruction(self: *State) error{}!void {
        if (self.mode.inst == .l) try self.addR(1);
    }
    pub fn addCycles(self: *State, comptime increment: comptime_int) error{}!void {
        self.cpu.cycles +%= increment;
    }
    pub fn addCycleCall(self: *State) error{}!void {
        if (!self.mode.suffix and self.cpu.mode.adl == .z80) try self.addCycles(1);
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
    pub fn checkAdl(self: *State) error{ConditionFailed}!void {
        if (self.cpu.mode.adl != .ez80) return error.ConditionFailed;
    }
    pub fn checkCondition(
        self: *State,
        comptime id: Cpu.RegisterId,
        comptime value: bool,
    ) error{ConditionFailed}!void {
        if (self.cpu.get(id) != @boolToInt(value)) return error.ConditionFailed;
    }

    pub fn loadRegister(self: *State, comptime id: Cpu.RegisterId) error{}!void {
        self.accumulator = self.cpu.get(id);
    }
    pub fn loadRegisterHigh(self: *State, comptime id: Cpu.RegisterId) error{}!void {
        self.accumulator |= self.cpu.get(id) << 8;
    }
    pub fn loadShadowRegister(self: *State, comptime id: Cpu.RegisterId) error{}!void {
        self.accumulator = self.cpu.getShadow(id);
    }
    fn loadStackPointer(self: *State, mode: Decode.Mode.Instruction) void {
        switch (mode) {
            .s => self.address = self.cpu.get(.sps),
            .l => self.address = self.cpu.get(.spl),
        }
    }
    pub fn loadStackPointerAdl(self: *State) error{}!void {
        self.loadStackPointer(Decode.Mode.Instruction.fromAdl(self.cpu.mode.adl));
    }
    pub fn loadStackPointerInstruction(self: *State) error{}!void {
        self.loadStackPointer(self.mode.inst);
    }

    pub fn storeRegister(self: *State, comptime id: Cpu.RegisterId) error{}!void {
        self.cpu.set(id, @intCast(u24, self.accumulator));
    }
    pub fn storeRegisterHigh(self: *State, comptime id: Cpu.RegisterId) error{}!void {
        const word = @intCast(u16, self.accumulator);
        self.cpu.set(id, word >> 8);
        self.accumulator = @truncate(u8, word);
    }
    pub fn storeRegisterInstruction(
        self: *State,
        comptime partial: Cpu.RegisterId,
        comptime full: Cpu.RegisterId,
    ) error{}!void {
        try switch (self.mode.inst) {
            .s => self.storeRegister(partial),
            .l => self.storeRegister(full),
        };
    }
    pub fn storeShadowRegister(self: *State, comptime id: Cpu.RegisterId) error{}!void {
        self.cpu.setShadow(id, @intCast(u24, self.accumulator));
    }
    fn storeStackPointer(self: *State, mode: Decode.Mode.Instruction) void {
        switch (mode) {
            .s => self.cpu.set(.sps, @truncate(u16, @intCast(u24, self.address))),
            .l => self.cpu.set(.spl, @intCast(u24, self.address)),
        }
    }
    pub fn storeStackPointerAdl(self: *State) error{}!void {
        self.storeStackPointer(Decode.Mode.Instruction.fromAdl(self.cpu.mode.adl));
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

    pub fn in(self: *State) error{}!void {
        self.accumulator = self.cpu.in(@truncate(u16, @intCast(u24, self.address)));
    }
    pub fn inFlags(self: *State) error{}!void {
        self.cpu.nf = false;
        self.cpu.hc = false;
    }
    pub fn readMemoryByte(self: *State) error{}!void {
        self.accumulator = self.cpu.read(@intCast(u24, self.address));
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

    pub fn out(self: *State) error{}!void {
        self.cpu.out(@truncate(u16, @intCast(u24, self.address)), @intCast(u8, self.accumulator));
    }
    pub fn writeMemoryByte(self: *State) error{}!void {
        self.cpu.write(@intCast(u24, self.address), @intCast(u8, self.accumulator));
    }
    pub fn writeMemoryWord(self: *State, comptime direction: Decode.Direction) error{}!void {
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

    fn restart(self: *State, stack: Decode.Mode.Instruction, mixed: bool) void {
        try self.addCycles(1);
        if (mixed) {
            const pc = @intCast(u24, self.accumulator);

            if (self.cpu.mode.adl == .ez80) {
                self.address = self.cpu.spl.long;
                try self.addAddress(-1);
                self.accumulator = @truncate(u8, pc >> 16);
                try self.writeMemoryByte();

                try self.addAddress(-1);
                self.accumulator = util.toBacking(self.cpu.mode);
                try self.writeMemoryByte();
                self.cpu.spl.long = @intCast(u24, self.address);
            }

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

            if (self.cpu.mode.adl == .z80) {
                self.address = self.cpu.spl.long;
                try self.addAddress(-1);
                self.accumulator = util.toBacking(self.cpu.mode);
                try self.writeMemoryByte();
                self.cpu.spl.long = @intCast(u24, self.address);
            }
        } else {
            try self.loadStackPointerInstruction();
            try self.addAddress(-1);
            try self.maskAddressInstruction();
            try self.writeMemoryWord(.reverse);
            try self.storeStackPointerInstruction();
        }
    }
    pub fn interrupt(self: *State) error{}!void {
        self.restart(
            Decode.Mode.Instruction.fromAdl(self.cpu.mode.adl),
            self.cpu.mode.madl == .ez80,
        );
    }
    pub fn rst(self: *State) error{}!void {
        self.restart(self.mode.inst, self.mode.suffix);
    }
    pub fn call(self: *State) error{}!void {
        if (self.mode.suffix) {
            const pc = @intCast(u24, self.accumulator);
            if (self.mode.imm == .il) try self.addR(1);

            if (self.cpu.mode.adl == .ez80) {
                self.address = self.cpu.spl.long;
                try self.addAddress(-1);
                self.accumulator = @truncate(u8, pc >> 16);
                try self.writeMemoryByte();

                if (self.mode.imm == .is) {
                    try self.addAddress(-1);
                    self.accumulator = util.toBacking(self.cpu.mode);
                    try self.writeMemoryByte();
                }
                self.cpu.spl.long = @intCast(u24, self.address);
            }

            const stack: Decode.Mode.Instruction = if (self.mode.imm == .il or
                self.mode.inst == .l and self.cpu.mode.adl == .z80) .l else .s;

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

            if (self.mode.imm == .il or self.cpu.mode.adl == .z80) {
                self.address = self.cpu.spl.long;
                try self.addAddress(-1);
                self.accumulator = util.toBacking(self.cpu.mode);
                try self.writeMemoryByte();
                self.cpu.spl.long = @intCast(u24, self.address);
            }
        } else {
            try self.loadStackPointerInstruction();
            try self.addAddress(-1);
            try self.maskAddressInstruction();
            try self.writeMemoryWord(.reverse);
            try self.storeStackPointerInstruction();
        }
    }
    pub fn ret(self: *State) error{}!void {
        if (self.mode.suffix) {
            self.address = self.cpu.get(.spl);
            try self.readMemoryByte();
            const adl = @intToEnum(Cpu.Adl, @truncate(u1, @intCast(u8, self.accumulator)));
            try self.addAddress(1);
            self.cpu.set(.spl, @intCast(u24, self.address));

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
                self.address = self.cpu.get(.spl);
                try self.readMemoryByte();
                if (self.mode.inst == .l or self.cpu.mode.adl == .ez80)
                    pc |= self.accumulator << 16;
                try self.addAddress(1);
                self.cpu.set(.spl, @intCast(u24, self.address));
            }

            self.cpu.mode.adl = adl;
            self.accumulator = pc;
        } else {
            try self.loadStackPointerInstruction();
            try self.maskAddressInstruction();
            try self.readMemoryWord();
            try self.addAddress(1);
            try self.storeStackPointerInstruction();
        }
    }
    pub fn copyIef(self: *State) error{}!void {
        self.cpu.ief1 = self.cpu.ief2;
    }

    pub fn offsetByte(self: *State, comptime offset: comptime_int) error{}!void {
        self.accumulator = @bitCast(u8, @bitCast(i8, @intCast(u8, self.accumulator)) +% offset);
    }

    pub fn zeroFlags(self: *State) error{}!void {
        self.cpu.cf = false;
        self.cpu.nf = false;
        self.cpu.pv = false;
        self.cpu.hc = false;
        self.cpu.zf = false;
        self.cpu.sf = false;
    }
    pub fn loadRegisterFlags(self: *State) error{}!void {
        const byte = @intCast(u8, self.accumulator);
        self.cpu.nf = false;
        self.cpu.pv = self.cpu.ief2;
        self.cpu.hc = false;
        self.cpu.zf = byte == 0;
        self.cpu.sf = @bitCast(i8, byte) < 0;
    }

    pub fn rlcByte(self: *State) error{}!void {
        const byte = @intCast(u8, self.accumulator);
        self.cpu.set(.cf, @truncate(u1, byte >> 7));
        self.cpu.nf = false;
        self.cpu.hc = false;
        self.accumulator = std.math.rotl(u8, byte, 1);
    }
    pub fn rrcByte(self: *State) error{}!void {
        const byte = @intCast(u8, self.accumulator);
        self.cpu.set(.cf, @truncate(u1, byte >> 0));
        self.cpu.nf = false;
        self.cpu.hc = false;
        self.accumulator = std.math.rotr(u8, byte, 1);
    }
    pub fn rlByte(self: *State) error{}!void {
        const carry = @boolToInt(self.cpu.cf);
        const byte = @intCast(u8, self.accumulator);
        self.cpu.set(.cf, @truncate(u1, byte >> 7));
        self.cpu.nf = false;
        self.cpu.hc = false;
        self.accumulator = byte << 1 | @as(u8, carry) << 0;
    }
    pub fn rrByte(self: *State) error{}!void {
        const carry = @boolToInt(self.cpu.cf);
        const byte = @intCast(u8, self.accumulator);
        self.cpu.set(.cf, @truncate(u1, byte >> 0));
        self.cpu.nf = false;
        self.cpu.hc = false;
        self.accumulator = byte >> 1 | @as(u8, carry) << 7;
    }
    pub fn slaByte(self: *State) error{}!void {
        const byte = @intCast(u8, self.accumulator);
        self.cpu.set(.cf, @truncate(u1, byte >> 7));
        self.cpu.nf = false;
        self.cpu.hc = false;
        self.accumulator = byte << 1;
    }
    pub fn sraByte(self: *State) error{}!void {
        const byte = @intCast(u8, self.accumulator);
        self.cpu.set(.cf, @truncate(u1, byte >> 0));
        self.cpu.nf = false;
        self.cpu.hc = false;
        self.accumulator = @bitCast(u8, @bitCast(i8, byte) >> 1);
    }
    pub fn srlByte(self: *State) error{}!void {
        const byte = @intCast(u8, self.accumulator);
        self.cpu.set(.cf, @truncate(u1, byte >> 0));
        self.cpu.nf = false;
        self.cpu.hc = false;
        self.accumulator = byte >> 1;
    }
    pub fn daaByte(self: *State) error{}!void {
        const byte = @intCast(u8, self.accumulator);
        const low = @truncate(u4, byte);

        const low_offset: u4 = if (self.cpu.hc or low > 0x9) 0x10 - 0xA else 0;

        if (@as(u9, byte) + low_offset >> 4 > 0x9) self.cpu.cf = true;
        const high_offset: u8 = if (self.cpu.cf) 0x10 - 0xA else 0;

        const offset = high_offset << 4 | low_offset;
        var result: u8 = undefined;
        var low_result: u4 = undefined;
        if (self.cpu.nf) {
            result = byte -% offset;
            self.cpu.hc = @subWithOverflow(u4, low, low_offset, &low_result);
        } else {
            result = byte +% offset;
            self.cpu.hc = @addWithOverflow(u4, low, low_offset, &low_result);
        }
        self.accumulator = result;
    }
    pub fn cplByte(self: *State) error{}!void {
        self.cpu.nf = true;
        self.cpu.hc = true;
        self.accumulator = ~@intCast(u8, self.accumulator);
    }
    pub fn bitByte(self: *State, comptime bit: u3) error{}!void {
        self.cpu.nf = false;
        self.cpu.hc = true;
        self.accumulator &= 1 << bit;
    }
    pub fn resByte(self: *State, comptime bit: u3) error{}!void {
        self.accumulator &= ~@as(u8, 1 << bit);
    }
    pub fn setByte(self: *State, comptime bit: u3) error{}!void {
        self.accumulator |= 1 << bit;
    }
    pub fn setSubtractByteSign(self: *State) error{}!void {
        self.cpu.nf = @bitCast(i8, @intCast(u8, self.accumulator)) < 0;
    }
    pub fn setParityByte(self: *State) error{}!void {
        self.cpu.pv = @popCount(@intCast(u8, self.accumulator)) & 1 == 0;
    }
    pub fn setZeroByte(self: *State) error{}!void {
        self.cpu.zf = @intCast(u8, self.accumulator) == 0;
    }
    pub fn setZeroWord(self: *State) error{}!void {
        self.cpu.zf = @intCast(u24, self.accumulator) == 0;
    }
    pub fn setSignByte(self: *State) error{}!void {
        self.cpu.sf = @bitCast(i8, @intCast(u8, self.accumulator)) < 0;
    }
    fn setCarry(self: *State, value: bool) void {
        self.cpu.cf = value;
        self.cpu.nf = false;
        self.cpu.hc = !value;
    }
    pub fn scf(self: *State) error{}!void {
        self.setCarry(true);
    }
    pub fn ccf(self: *State) error{}!void {
        self.setCarry(!self.cpu.cf);
    }
    pub fn addByte(self: *State, comptime rhs: comptime_int) error{}!void {
        const unsigned_lhs = @intCast(u8, self.accumulator);
        const signed_rhs: i8 = rhs;
        const unsigned_rhs = @bitCast(u8, signed_rhs);
        self.cpu.nf = signed_rhs < 0;

        const signed_lhs = @bitCast(i8, unsigned_lhs);
        var signed_result: i8 = undefined;
        self.cpu.pv = @addWithOverflow(i8, signed_lhs, signed_rhs, &signed_result);

        const low_lhs = @truncate(u4, unsigned_lhs);
        const low_rhs = @truncate(u4, unsigned_rhs);
        var low_result: u4 = undefined;
        self.cpu.hc = @addWithOverflow(u4, low_lhs, low_rhs, &low_result) != self.cpu.nf;

        self.cpu.zf = signed_result == 0;
        self.cpu.sf = signed_result < 0;
        self.accumulator = @bitCast(u8, signed_result);
    }
    pub fn addByteHalfZeroSign(self: *State, comptime rhs: comptime_int) error{}!void {
        const unsigned_lhs = @intCast(u8, self.accumulator);
        const unsigned_rhs = @bitCast(u8, @as(i8, rhs));
        const unsigned_result = unsigned_lhs +% unsigned_rhs;

        self.cpu.cf = false;
        self.cpu.pv = false;

        const low_lhs = @truncate(u4, unsigned_lhs);
        const low_rhs = @truncate(u4, unsigned_rhs);
        var low_result: u4 = undefined;
        self.cpu.hc = @addWithOverflow(u4, low_lhs, low_rhs, &low_result) != (rhs < 0);

        self.cpu.zf = unsigned_result == 0;
        self.cpu.sf = @bitCast(i8, unsigned_result) < 0;
        self.accumulator = unsigned_result;
    }
    fn addWordByte(word: *i32, byte: i8) void {
        word.* = @bitCast(u24, @bitCast(i24, @intCast(u24, word.*)) +% byte);
    }
    pub fn addWord(self: *State, comptime offset: comptime_int) error{}!void {
        addWordByte(&self.accumulator, offset);
    }
    pub fn subWordSuffix(self: *State) error{}!void {
        if (self.mode.suffix) try self.addWord(-1);
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
        self.cpu.cf = @addWithOverflow(u8, unsigned_lhs, unsigned_rhs, &unsigned_result);

        self.cpu.nf = false;

        const signed_lhs = @bitCast(i8, unsigned_lhs);
        const signed_rhs = @bitCast(i8, unsigned_rhs);
        var signed_result: i8 = undefined;
        self.cpu.pv = @addWithOverflow(i8, signed_lhs, signed_rhs, &signed_result);

        const low_lhs = @truncate(u4, unsigned_lhs);
        const low_rhs = @truncate(u4, unsigned_rhs);
        var low_result: u4 = undefined;
        self.cpu.hc = @addWithOverflow(u4, low_lhs, low_rhs, &low_result);

        self.cpu.zf = unsigned_result == 0;
        self.cpu.sf = signed_result < 0;
        self.accumulator = unsigned_result;
    }
    pub fn adcBytes(self: *State) error{}!void {
        const carry = @boolToInt(self.cpu.cf);

        const unsigned_lhs = @intCast(u8, self.accumulator);
        const unsigned_rhs = @intCast(u8, self.address);
        var unsigned_result: u8 = undefined;
        const cf = @addWithOverflow(u8, unsigned_lhs, unsigned_rhs, &unsigned_result);
        self.cpu.cf = @addWithOverflow(u8, unsigned_result, carry, &unsigned_result) or cf;

        self.cpu.nf = false;

        const signed_lhs = @bitCast(i8, unsigned_lhs);
        const signed_rhs = @bitCast(i8, unsigned_rhs);
        const signed_result = @bitCast(i8, unsigned_result);
        self.cpu.pv = @as(i9, signed_lhs) + signed_rhs + carry != signed_result;

        const low_lhs = @truncate(u4, unsigned_lhs);
        const low_rhs = @truncate(u4, unsigned_rhs);
        var low_result: u4 = undefined;
        self.cpu.hc = @addWithOverflow(u4, low_lhs, low_rhs, &low_result) or
            @addWithOverflow(u4, low_result, carry, &low_result);

        self.cpu.zf = unsigned_result == 0;
        self.cpu.sf = signed_result < 0;
        self.accumulator = unsigned_result;
    }
    pub fn subBytes(self: *State) error{}!void {
        const unsigned_lhs = @intCast(u8, self.accumulator);
        const unsigned_rhs = @intCast(u8, self.address);
        var unsigned_result: u8 = undefined;
        self.cpu.cf = @subWithOverflow(u8, unsigned_lhs, unsigned_rhs, &unsigned_result);

        self.cpu.nf = true;

        const signed_lhs = @bitCast(i8, unsigned_lhs);
        const signed_rhs = @bitCast(i8, unsigned_rhs);
        var signed_result: i8 = undefined;
        self.cpu.pv = @subWithOverflow(i8, signed_lhs, signed_rhs, &signed_result);

        const low_lhs = @truncate(u4, unsigned_lhs);
        const low_rhs = @truncate(u4, unsigned_rhs);
        var low_result: u4 = undefined;
        self.cpu.hc = @subWithOverflow(u4, low_lhs, low_rhs, &low_result);

        self.cpu.zf = unsigned_result == 0;
        self.cpu.sf = signed_result < 0;
        self.accumulator = unsigned_result;
    }
    pub fn sbcBytes(self: *State) error{}!void {
        const carry = @boolToInt(self.cpu.cf);

        const unsigned_lhs = @intCast(u8, self.accumulator);
        const unsigned_rhs = @intCast(u8, self.address);
        var unsigned_result: u8 = undefined;
        const cf = @subWithOverflow(u8, unsigned_lhs, unsigned_rhs, &unsigned_result);
        self.cpu.cf = @subWithOverflow(u8, unsigned_result, carry, &unsigned_result) or cf;

        self.cpu.nf = true;

        const signed_lhs = @bitCast(i8, unsigned_lhs);
        const signed_rhs = @bitCast(i8, unsigned_rhs);
        const signed_result = @bitCast(i8, unsigned_result);
        self.cpu.pv = @as(i9, signed_lhs) - signed_rhs - carry != signed_result;

        const low_lhs = @truncate(u4, unsigned_lhs);
        const low_rhs = @truncate(u4, unsigned_rhs);
        var low_result: u4 = undefined;
        self.cpu.hc = @subWithOverflow(u4, low_lhs, low_rhs, &low_result) or
            @subWithOverflow(u4, low_result, carry, &low_result);

        self.cpu.zf = unsigned_result == 0;
        self.cpu.sf = signed_result < 0;
        self.accumulator = unsigned_result;
    }
    pub fn andBytes(self: *State) error{}!void {
        const lhs = @intCast(u8, self.accumulator);
        const rhs = @intCast(u8, self.address);

        self.cpu.cf = false;
        self.cpu.nf = false;
        self.cpu.hc = true;
        self.accumulator = lhs & rhs;
    }
    pub fn xorBytes(self: *State) error{}!void {
        const lhs = @intCast(u8, self.accumulator);
        const rhs = @intCast(u8, self.address);

        self.cpu.cf = false;
        self.cpu.nf = false;
        self.cpu.hc = false;
        self.accumulator = lhs ^ rhs;
    }
    pub fn orBytes(self: *State) error{}!void {
        const lhs = @intCast(u8, self.accumulator);
        const rhs = @intCast(u8, self.address);

        self.cpu.cf = false;
        self.cpu.nf = false;
        self.cpu.hc = false;
        self.accumulator = lhs | rhs;
    }
    pub fn mltBytes(self: *State) error{}!void {
        const input = @intCast(u24, self.accumulator);
        const lhs = @truncate(u8, input >> 8);
        const rhs = @truncate(u8, input >> 0);
        self.accumulator = std.math.mulWide(u8, lhs, rhs);
    }
    const Digits = packed struct(u16) { mem_low: u4, mem_high: u4, a_low: u4, a_high: u4 };
    fn rdBytesFlags(self: *State) void {
        const byte = @truncate(u8, @intCast(u16, self.accumulator) >> 8);
        self.cpu.nf = false;
        self.cpu.pv = @popCount(byte) & 1 == 0;
        self.cpu.hc = false;
        self.cpu.zf = byte == 0;
        self.cpu.sf = @bitCast(i8, byte) < 0;
    }
    pub fn rrdBytes(self: *State) error{}!void {
        const digits = util.fromBacking(Digits, @intCast(u16, self.accumulator));
        self.accumulator = util.toBacking(Digits{
            .mem_low = digits.mem_high,
            .mem_high = digits.a_low,
            .a_low = digits.mem_low,
            .a_high = digits.a_high,
        });
        self.rdBytesFlags();
    }
    pub fn rldBytes(self: *State) error{}!void {
        const digits = util.fromBacking(Digits, @intCast(u16, self.accumulator));
        self.accumulator = util.toBacking(Digits{
            .mem_low = digits.a_low,
            .mem_high = digits.mem_low,
            .a_low = digits.mem_high,
            .a_high = digits.a_high,
        });
        self.rdBytesFlags();
    }

    fn addWordsWidth(self: *State, comptime width: comptime_int) void {
        const UnsignedInt = std.meta.Int(.unsigned, width);

        const unsigned_lhs = @truncate(UnsignedInt, @intCast(u24, self.accumulator));
        const unsigned_rhs = @truncate(UnsignedInt, @intCast(u24, self.address));
        var unsigned_result: UnsignedInt = undefined;
        self.cpu.cf = @addWithOverflow(UnsignedInt, unsigned_lhs, unsigned_rhs, &unsigned_result);

        self.cpu.nf = false;

        const low_lhs = @truncate(u12, unsigned_lhs);
        const low_rhs = @truncate(u12, unsigned_rhs);
        var low_result: u12 = undefined;
        self.cpu.hc = @addWithOverflow(u12, low_lhs, low_rhs, &low_result);

        self.accumulator = unsigned_result;
    }
    pub fn addWords(self: *State) error{}!void {
        switch (self.mode.inst) {
            .s => self.addWordsWidth(16),
            .l => self.addWordsWidth(24),
        }
    }
    fn adcWordsWidth(self: *State, comptime width: comptime_int) void {
        const UnsignedInt = std.meta.Int(.unsigned, width);
        const SignedInt = std.meta.Int(.signed, width);

        const carry = @boolToInt(self.cpu.cf);

        const unsigned_lhs = @truncate(UnsignedInt, @intCast(u24, self.accumulator));
        const unsigned_rhs = @truncate(UnsignedInt, @intCast(u24, self.address));
        var unsigned_result: UnsignedInt = undefined;
        const cf = @addWithOverflow(UnsignedInt, unsigned_lhs, unsigned_rhs, &unsigned_result);
        self.cpu.cf = @addWithOverflow(UnsignedInt, unsigned_result, carry, &unsigned_result) or cf;

        self.cpu.nf = false;

        const signed_lhs = @bitCast(SignedInt, unsigned_lhs);
        const signed_rhs = @bitCast(SignedInt, unsigned_rhs);
        const signed_result = @bitCast(SignedInt, unsigned_result);
        self.cpu.pv = @as(std.meta.Int(.signed, width + 1), signed_lhs) + signed_rhs + carry !=
            signed_result;

        const low_lhs = @truncate(u12, unsigned_lhs);
        const low_rhs = @truncate(u12, unsigned_rhs);
        var low_result: u12 = undefined;
        self.cpu.hc = @addWithOverflow(u12, low_lhs, low_rhs, &low_result) or
            @addWithOverflow(u12, low_result, carry, &low_result);

        self.cpu.zf = unsigned_result == 0;
        self.cpu.sf = signed_result < 0;

        self.accumulator = unsigned_result;
    }
    pub fn adcWords(self: *State) error{}!void {
        switch (self.mode.inst) {
            .s => self.adcWordsWidth(16),
            .l => self.adcWordsWidth(24),
        }
    }
    fn sbcWordsWidth(self: *State, comptime width: comptime_int) void {
        const UnsignedInt = std.meta.Int(.unsigned, width);
        const SignedInt = std.meta.Int(.signed, width);

        const carry = @boolToInt(self.cpu.cf);

        const unsigned_lhs = @truncate(UnsignedInt, @intCast(u24, self.accumulator));
        const unsigned_rhs = @truncate(UnsignedInt, @intCast(u24, self.address));
        var unsigned_result: UnsignedInt = undefined;
        const cf = @subWithOverflow(UnsignedInt, unsigned_lhs, unsigned_rhs, &unsigned_result);
        self.cpu.cf = @subWithOverflow(UnsignedInt, unsigned_result, carry, &unsigned_result) or cf;

        self.cpu.nf = true;

        const signed_lhs = @bitCast(SignedInt, unsigned_lhs);
        const signed_rhs = @bitCast(SignedInt, unsigned_rhs);
        const signed_result = @bitCast(SignedInt, unsigned_result);
        self.cpu.pv = @as(std.meta.Int(.signed, width + 1), signed_lhs) -% signed_rhs -% carry !=
            signed_result;

        const low_lhs = @truncate(u12, unsigned_lhs);
        const low_rhs = @truncate(u12, unsigned_rhs);
        var low_result: u12 = undefined;
        self.cpu.hc = @subWithOverflow(u12, low_lhs, low_rhs, &low_result) or
            @subWithOverflow(u12, low_result, carry, &low_result);

        self.cpu.zf = unsigned_result == 0;
        self.cpu.sf = signed_result < 0;

        self.accumulator = unsigned_result;
    }
    pub fn sbcWords(self: *State) error{}!void {
        switch (self.mode.inst) {
            .s => self.sbcWordsWidth(16),
            .l => self.sbcWordsWidth(24),
        }
    }

    pub fn ldFlags(self: *State) error{}!void {
        self.cpu.nf = false;
        self.cpu.hc = false;
    }
    pub fn cpFlags(self: *State) error{}!void {
        const lhs = @intCast(u8, self.accumulator);
        const rhs = @intCast(u8, self.address);
        const result = lhs -% rhs;

        self.cpu.nf = true;

        const low_lhs = @truncate(u4, lhs);
        const low_rhs = @truncate(u4, rhs);
        var low_result: u4 = undefined;
        self.cpu.hc = @subWithOverflow(u4, low_lhs, low_rhs, &low_result);

        self.cpu.zf = result == 0;
        self.cpu.sf = @bitCast(i8, result) < 0;
    }
    pub fn repeatFlag(self: *State) error{}!void {
        self.cpu.pv = self.accumulator != 0;
    }
    pub fn repeat(self: *State) error{RepeatInstruction}!void {
        self.repeat_counter = 1;
        return switch (self.repeat_counter) {
            0 => {},
            1 => Decode.Error.RepeatInstruction,
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

    const core = try @import("../cemucore.zig").create(.{
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
                        core.setSlice(.{ .port = 0x4800 }, input[0..8]);
                        core.setSlice(.{ .port = 0x0020 }, input[8..14]);
                        core.setSlice(.{ .port = 0x201E }, input[14..16]);
                        core.cpu.setShadow(.adl, @truncate(u1, input[16] >> 0));
                        core.cpu.set(.adl, @truncate(u1, input[16] >> 1));
                        core.set(
                            .{ .port = 0x4900 },
                            if (input[16] >> 2 & 1 != 0) switch (@truncate(u2, input[16] >> 3)) {
                                0 => 0o100,
                                1 => 0o111,
                                2 => 0o122,
                                3 => 0o133,
                            } else 0o000,
                        );
                        core.cpu.set(.ief, @truncate(u1, input[16] >> 5));
                        core.setSlice(.{ .port = 0x4901 }, input[17..22]);
                        core.setSlice(.{ .port = 0x4906 }, &[_]u8{0} ** (0x4917 - 0x4906 + 1));
                        core.cpu.set(.ui, std.mem.readIntLittle(u16, input[22..24]));
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
                        core.cpu.addR(0x18);
                        core.cpu.set(.pc, 0xE30900);
                        while (core.cpu.get(.pc) < 0xE30906)
                            core.cpu.step();
                        core.cpu.addR(0x27);

                        var output: [64]u8 = undefined;

                        core.getSlice(.{ .port = 0x4800 }, output[0..8]);
                        core.getSlice(.{ .port = 0x0020 }, output[8..14]);
                        core.getSlice(.{ .port = 0x201E }, output[14..16]);
                        std.mem.writeIntLittle(
                            u32,
                            output[16..20],
                            @intCast(u32, core.cpu.cycles + @as(u64, switch (core.cpu.mode.adl) {
                                .z80 => 0x117,
                                .ez80 => 0x119,
                            })),
                        );
                        if (false) std.mem.writeIntLittle(u32, output[16..20], 0);
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
                        if (false) {
                            std.mem.writeIntLittle(u8, output[20..21], util.toBacking(Cpu.Flags{
                                .cf = false,
                                .nf = false,
                                .pv = true,
                                .xf = core.cpu.getShadow(.xf) != 0,
                                .hc = false,
                                .yf = core.cpu.getShadow(.yf) != 0,
                                .zf = true,
                                .sf = false,
                            }));
                            std.mem.writeIntLittle(u8, output[21..22], 0);
                        }
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
                                util.toBacking(core.cpu.mode)) <<
                                if (core.cpu.mode.adl == .z80) 8 else 0,
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
