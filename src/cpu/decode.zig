const std = @import("std");

const CEmuCore = @import("../cemucore.zig");
const Cpu = @import("../cpu.zig");

pub const Mode = struct {
    pub const Instruction = enum(u1) {
        s,
        l,

        pub fn fromAdl(adl: Cpu.Adl) Instruction {
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

    pub fn fromAdl(adl: Cpu.Adl) Mode {
        return .{
            .inst = Instruction.fromAdl(adl),
            .imm = Immediate.fromAdl(adl),
        };
    }
};
pub const PrefetchMode = enum(u1) { prefetch, cached };

const Uop = enum {
    unimplemented,

    save,
    restore,
    swap,

    mask_word_inst,
    mask_addr_adl,
    mask_addr_inst,

    add_r_1,
    add_cc_1,

    dispatch_base,

    mode_sis,
    mode_lis,
    mode_sil,
    mode_lil,
    adl_inst,
    adl_imm,
    clear_ief,
    set_ief,

    halt,

    nz,
    z,
    nc,
    c,
    po,
    pe,
    p,
    m,

    load_a_high,
    load_b,
    load_c,
    load_d,
    load_e,
    load_h,
    load_l,
    load_a,
    load_af,
    load_bc,
    load_de,
    load_hl,
    load_sp,
    load_pc,
    @"load_af'",
    @"load_bc'",
    @"load_de'",
    @"load_hl'",

    store_b,
    store_c,
    store_d,
    store_e,
    store_h,
    store_l,
    store_a,
    store_af,
    store_bc,
    store_de,
    store_hl,
    store_sp,
    store_pc,
    @"store_af'",
    @"store_bc'",
    @"store_de'",
    @"store_hl'",

    flush,
    fetch_byte,
    fetch_word,
    fetch_word_cached,

    read_port,
    read_byte,
    read_word,

    write_port,
    write_byte,
    write_word,

    inc_byte,
    dec_byte,
    inc_word,
    dec_word,
    add_words,
};

const base = [_][]const Uop{
    &[_]Uop{}, // nop
    &[_]Uop{ .fetch_word, .mask_word_inst, .store_bc }, // ld bc,nn
    &[_]Uop{ .load_bc, .save, .mask_addr_inst, .load_a, .write_byte }, // ld (bc),a
    &[_]Uop{ .load_bc, .inc_word, .mask_word_inst, .store_bc }, // inc bc
    &[_]Uop{ .load_b, .inc_byte, .store_b }, // inc b
    &[_]Uop{ .load_b, .dec_byte, .store_b }, // dec b
    &[_]Uop{ .fetch_byte, .store_b }, // ld b,n
    &[_]Uop{.unimplemented}, // rlca
    &[_]Uop{ .load_af, .save, .@"load_af'", .store_af, .restore, .@"store_af'" }, // ex af,af'
    &[_]Uop{ .load_bc, .save, .load_hl, .add_words, .store_hl }, // add hl,bc
    &[_]Uop{ .load_bc, .save, .mask_addr_inst, .read_byte, .store_a }, // ld a,(bc)
    &[_]Uop{ .load_bc, .dec_word, .mask_word_inst, .store_bc }, // dec bc
    &[_]Uop{ .load_c, .inc_byte, .store_c }, // inc c
    &[_]Uop{ .load_c, .dec_byte, .store_c }, // dec c
    &[_]Uop{ .fetch_byte, .store_c }, // ld c,n
    &[_]Uop{.unimplemented}, // rrca
    &[_]Uop{.unimplemented}, // djnz d
    &[_]Uop{ .fetch_word, .mask_word_inst, .store_de }, // ld de,nn
    &[_]Uop{ .load_de, .save, .mask_addr_inst, .load_a, .write_byte }, // ld (de),a
    &[_]Uop{ .load_de, .inc_word, .mask_word_inst, .store_de }, // inc de
    &[_]Uop{ .load_d, .inc_byte, .store_d }, // inc d
    &[_]Uop{ .load_d, .dec_byte, .store_d }, // dec d
    &[_]Uop{ .fetch_byte, .store_d }, // ld d,n
    &[_]Uop{.unimplemented}, // rla
    &[_]Uop{.unimplemented}, // jr d
    &[_]Uop{ .load_de, .save, .load_hl, .add_words, .store_hl }, // add hl,de
    &[_]Uop{ .load_de, .save, .mask_addr_inst, .read_byte, .store_a }, // ld a,(de)
    &[_]Uop{ .load_de, .dec_word, .mask_word_inst, .store_de }, // dec de
    &[_]Uop{ .load_e, .inc_byte, .store_e }, // inc e
    &[_]Uop{ .load_e, .dec_byte, .store_e }, // dec e
    &[_]Uop{ .fetch_byte, .store_e }, // ld e,n
    &[_]Uop{.unimplemented}, // rra
    &[_]Uop{.unimplemented}, // jr nz,d
    &[_]Uop{ .fetch_word, .mask_word_inst, .store_hl }, // ld hl,nn
    &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .load_hl, .write_word }, // ld (nn),hl
    &[_]Uop{ .load_hl, .inc_word, .mask_word_inst, .store_hl }, // inc hl
    &[_]Uop{ .load_h, .inc_byte, .store_h }, // inc h
    &[_]Uop{ .load_h, .dec_byte, .store_h }, // dec h
    &[_]Uop{ .fetch_byte, .store_h }, // ld h,n
    &[_]Uop{.unimplemented}, // daa
    &[_]Uop{.unimplemented}, // jr z,d
    &[_]Uop{ .load_hl, .save, .add_words, .store_hl }, // add hl,hl
    &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_word, .store_hl }, // ld hl,(nn)
    &[_]Uop{ .load_hl, .dec_word, .mask_word_inst, .store_hl }, // dec hl
    &[_]Uop{ .load_l, .inc_byte, .store_l }, // inc l
    &[_]Uop{ .load_l, .dec_byte, .store_l }, // dec l
    &[_]Uop{ .fetch_byte, .store_l }, // ld l,n
    &[_]Uop{.unimplemented}, // cpl
    &[_]Uop{.unimplemented}, // jr nc,d
    &[_]Uop{ .fetch_word, .store_sp }, // ld sp,nn
    &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .load_a, .write_byte }, // ld (nn),a
    &[_]Uop{ .load_sp, .inc_word, .store_sp }, // inc sp
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .inc_byte, .write_byte }, // inc (hl)
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .dec_byte, .write_byte }, // dec (hl)
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .fetch_byte, .write_byte }, // ld (hl),n
    &[_]Uop{.unimplemented}, // scf
    &[_]Uop{.unimplemented}, // jr c,d
    &[_]Uop{ .load_sp, .save, .load_hl, .add_words, .store_hl }, // add hl,sp
    &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_byte, .store_a }, // ld a,(nn)
    &[_]Uop{ .load_sp, .dec_word, .store_sp }, // dec sp
    &[_]Uop{ .load_a, .inc_byte, .store_a }, // inc a
    &[_]Uop{ .load_a, .dec_byte, .store_a }, // dec a
    &[_]Uop{ .fetch_byte, .store_a }, // ld a,n
    &[_]Uop{.unimplemented}, // ccf
    &[_]Uop{ .mode_sis, .add_r_1, .fetch_byte, .dispatch_base }, // .sis
    &[_]Uop{ .load_c, .store_b }, // ld b,c
    &[_]Uop{ .load_d, .store_b }, // ld b,d
    &[_]Uop{ .load_e, .store_b }, // ld b,e
    &[_]Uop{ .load_h, .store_b }, // ld b,h
    &[_]Uop{ .load_l, .store_b }, // ld b,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .store_b }, // ld b,(hl)
    &[_]Uop{ .load_a, .store_b }, // ld b,a
    &[_]Uop{ .load_b, .store_c }, // ld c,b
    &[_]Uop{ .mode_lis, .add_r_1, .fetch_byte, .dispatch_base }, // .lis
    &[_]Uop{ .load_d, .store_c }, // ld c,d
    &[_]Uop{ .load_e, .store_c }, // ld c,e
    &[_]Uop{ .load_h, .store_c }, // ld c,h
    &[_]Uop{ .load_l, .store_c }, // ld c,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .store_c }, // ld c,(hl)
    &[_]Uop{ .load_a, .store_c }, // ld c,a
    &[_]Uop{ .load_b, .store_d }, // ld d,b
    &[_]Uop{ .load_c, .store_d }, // ld d,c
    &[_]Uop{ .mode_sil, .add_r_1, .fetch_byte, .dispatch_base }, // .sil
    &[_]Uop{ .load_e, .store_d }, // ld d,e
    &[_]Uop{ .load_h, .store_d }, // ld d,h
    &[_]Uop{ .load_l, .store_d }, // ld d,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .store_d }, // ld d,(hl)
    &[_]Uop{ .load_a, .store_d }, // ld d,a
    &[_]Uop{ .load_b, .store_e }, // ld e,b
    &[_]Uop{ .load_c, .store_e }, // ld e,c
    &[_]Uop{ .load_d, .store_e }, // ld e,d
    &[_]Uop{ .mode_lil, .add_r_1, .fetch_byte, .dispatch_base }, // .lil
    &[_]Uop{ .load_h, .store_e }, // ld e,h
    &[_]Uop{ .load_l, .store_e }, // ld e,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .store_e }, // ld e,(hl)
    &[_]Uop{ .load_a, .store_e }, // ld e,a
    &[_]Uop{ .load_b, .store_h }, // ld h,b
    &[_]Uop{ .load_c, .store_h }, // ld h,c
    &[_]Uop{ .load_d, .store_h }, // ld h,d
    &[_]Uop{ .load_e, .store_h }, // ld h,e
    &[_]Uop{}, // ld h,h
    &[_]Uop{ .load_l, .store_h }, // ld h,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .store_h }, // ld h,(hl)
    &[_]Uop{ .load_a, .store_h }, // ld h,a
    &[_]Uop{ .load_b, .store_l }, // ld l,b
    &[_]Uop{ .load_c, .store_l }, // ld l,c
    &[_]Uop{ .load_d, .store_l }, // ld l,d
    &[_]Uop{ .load_e, .store_l }, // ld l,e
    &[_]Uop{ .load_h, .store_l }, // ld l,h
    &[_]Uop{}, // ld l,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .store_l }, // ld l,(hl)
    &[_]Uop{ .load_a, .store_l }, // ld a,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .load_b, .write_byte }, // ld (hl),b
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .load_c, .write_byte }, // ld (hl),c
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .load_d, .write_byte }, // ld (hl),d
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .load_e, .write_byte }, // ld (hl),e
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .load_h, .write_byte }, // ld (hl),h
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .load_l, .write_byte }, // ld (hl),l
    &[_]Uop{.halt}, // halt
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .load_a, .write_byte }, // ld (hl),a
    &[_]Uop{ .load_b, .store_a }, // ld a,b
    &[_]Uop{ .load_c, .store_a }, // ld a,c
    &[_]Uop{ .load_d, .store_a }, // ld a,d
    &[_]Uop{ .load_e, .store_a }, // ld a,e
    &[_]Uop{ .load_h, .store_a }, // ld a,h
    &[_]Uop{ .load_l, .store_a }, // ld a,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .store_a }, // ld a,(hl)
    &[_]Uop{}, // ld a,a
    &[_]Uop{.unimplemented}, // add a,b
    &[_]Uop{.unimplemented}, // add a,c
    &[_]Uop{.unimplemented}, // add a,d
    &[_]Uop{.unimplemented}, // add a,e
    &[_]Uop{.unimplemented}, // add a,h
    &[_]Uop{.unimplemented}, // add a,l
    &[_]Uop{.unimplemented}, // add a,(hl)
    &[_]Uop{.unimplemented}, // add a,a
    &[_]Uop{.unimplemented}, // adc a,b
    &[_]Uop{.unimplemented}, // adc a,c
    &[_]Uop{.unimplemented}, // adc a,d
    &[_]Uop{.unimplemented}, // adc a,e
    &[_]Uop{.unimplemented}, // adc a,h
    &[_]Uop{.unimplemented}, // adc a,l
    &[_]Uop{.unimplemented}, // adc a,(hl)
    &[_]Uop{.unimplemented}, // adc a,a
    &[_]Uop{.unimplemented}, // sub a,b
    &[_]Uop{.unimplemented}, // sub a,c
    &[_]Uop{.unimplemented}, // sub a,d
    &[_]Uop{.unimplemented}, // sub a,e
    &[_]Uop{.unimplemented}, // sub a,h
    &[_]Uop{.unimplemented}, // sub a,l
    &[_]Uop{.unimplemented}, // sub a,(hl)
    &[_]Uop{.unimplemented}, // sub a,a
    &[_]Uop{.unimplemented}, // sbc a,b
    &[_]Uop{.unimplemented}, // sbc a,c
    &[_]Uop{.unimplemented}, // sbc a,d
    &[_]Uop{.unimplemented}, // sbc a,e
    &[_]Uop{.unimplemented}, // sbc a,h
    &[_]Uop{.unimplemented}, // sbc a,l
    &[_]Uop{.unimplemented}, // sbc a,(hl)
    &[_]Uop{.unimplemented}, // sbc a,a
    &[_]Uop{.unimplemented}, // and a,b
    &[_]Uop{.unimplemented}, // and a,c
    &[_]Uop{.unimplemented}, // and a,d
    &[_]Uop{.unimplemented}, // and a,e
    &[_]Uop{.unimplemented}, // and a,h
    &[_]Uop{.unimplemented}, // and a,l
    &[_]Uop{.unimplemented}, // and a,(hl)
    &[_]Uop{.unimplemented}, // and a,a
    &[_]Uop{.unimplemented}, // xor a,b
    &[_]Uop{.unimplemented}, // xor a,c
    &[_]Uop{.unimplemented}, // xor a,d
    &[_]Uop{.unimplemented}, // xor a,e
    &[_]Uop{.unimplemented}, // xor a,h
    &[_]Uop{.unimplemented}, // xor a,l
    &[_]Uop{.unimplemented}, // xor a,(hl)
    &[_]Uop{.unimplemented}, // xor a,a
    &[_]Uop{.unimplemented}, // or a,b
    &[_]Uop{.unimplemented}, // or a,c
    &[_]Uop{.unimplemented}, // or a,d
    &[_]Uop{.unimplemented}, // or a,e
    &[_]Uop{.unimplemented}, // or a,h
    &[_]Uop{.unimplemented}, // or a,l
    &[_]Uop{.unimplemented}, // or a,(hl)
    &[_]Uop{.unimplemented}, // or a,a
    &[_]Uop{.unimplemented}, // cp a,b
    &[_]Uop{.unimplemented}, // cp a,c
    &[_]Uop{.unimplemented}, // cp a,d
    &[_]Uop{.unimplemented}, // cp a,e
    &[_]Uop{.unimplemented}, // cp a,h
    &[_]Uop{.unimplemented}, // cp a,l
    &[_]Uop{.unimplemented}, // cp a,(hl)
    &[_]Uop{.unimplemented}, // cp a,a
    &[_]Uop{.unimplemented}, // ret nz
    &[_]Uop{.unimplemented}, // pop bc
    &[_]Uop{ .fetch_word_cached, .flush, .nz, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp nz,nn
    &[_]Uop{ .fetch_word_cached, .flush, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp nn
    &[_]Uop{.unimplemented}, // call nz,nn
    &[_]Uop{.unimplemented}, // push bc
    &[_]Uop{ .fetch_byte, .unimplemented }, // add a,n
    &[_]Uop{.unimplemented}, // rst 00h
    &[_]Uop{.unimplemented}, // ret z
    &[_]Uop{.unimplemented}, // ret
    &[_]Uop{ .fetch_word_cached, .flush, .z, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp z,nn
    &[_]Uop{.unimplemented}, // CB
    &[_]Uop{.unimplemented}, // call z,nn
    &[_]Uop{.unimplemented}, // call nn
    &[_]Uop{ .fetch_byte, .unimplemented }, // adc a,n
    &[_]Uop{.unimplemented}, // rst 08h
    &[_]Uop{.unimplemented}, // ret nc
    &[_]Uop{.unimplemented}, // pop de
    &[_]Uop{ .fetch_word_cached, .flush, .nc, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp nc,nn
    &[_]Uop{ .fetch_byte, .load_a_high, .save, .load_a, .write_port }, // out (n),a
    &[_]Uop{.unimplemented}, // call nc,nn
    &[_]Uop{.unimplemented}, // push de
    &[_]Uop{ .fetch_byte, .unimplemented }, // sub a,n
    &[_]Uop{.unimplemented}, // rst 10h
    &[_]Uop{.unimplemented}, // ret c
    &[_]Uop{
        .load_bc, .save, .@"load_bc'", .store_bc, .restore, .@"store_bc'",
        .load_de, .save, .@"load_de'", .store_de, .restore, .@"store_de'",
        .load_hl, .save, .@"load_hl'", .store_hl, .restore, .@"store_hl'",
    }, // exx
    &[_]Uop{ .fetch_word_cached, .flush, .c, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp c,nn
    &[_]Uop{ .fetch_byte, .load_a_high, .save, .read_port, .store_a }, // in a,(n)
    &[_]Uop{.unimplemented}, // call c,nn
    &[_]Uop{.unimplemented}, // DD
    &[_]Uop{ .fetch_byte, .unimplemented }, // sbc a,n
    &[_]Uop{.unimplemented}, // rst 18h
    &[_]Uop{.unimplemented}, // ret po
    &[_]Uop{.unimplemented}, // pop hl
    &[_]Uop{ .fetch_word_cached, .flush, .po, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp po,nn
    &[_]Uop{.unimplemented}, // ex (sp),hl
    &[_]Uop{.unimplemented}, // call po,nn
    &[_]Uop{.unimplemented}, // push hl
    &[_]Uop{ .fetch_byte, .unimplemented }, // and a,n
    &[_]Uop{.unimplemented}, // rst 20h
    &[_]Uop{.unimplemented}, // ret pe
    &[_]Uop{ .flush, .fetch_byte, .load_hl, .mask_word_inst, .store_pc, .adl_inst }, // jp (hl)
    &[_]Uop{ .fetch_word_cached, .flush, .pe, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp pe,nn
    &[_]Uop{ .load_de, .save, .load_hl, .mask_word_inst, .store_de, .restore, .mask_word_inst, .store_hl }, // ex de,hl
    &[_]Uop{.unimplemented}, // call pe,nn
    &[_]Uop{.unimplemented}, // ED
    &[_]Uop{ .fetch_byte, .unimplemented }, // xor a,n
    &[_]Uop{.unimplemented}, // rst 28h
    &[_]Uop{.unimplemented}, // ret p
    &[_]Uop{.unimplemented}, // pop af
    &[_]Uop{ .fetch_word_cached, .flush, .p, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp p,nn
    &[_]Uop{.clear_ief}, // di
    &[_]Uop{.unimplemented}, // call p,nn
    &[_]Uop{.unimplemented}, // push af
    &[_]Uop{ .fetch_byte, .unimplemented }, // or a,n
    &[_]Uop{.unimplemented}, // rst 30h
    &[_]Uop{.unimplemented}, // ret m
    &[_]Uop{ .load_hl, .store_sp }, // ld sp,hl
    &[_]Uop{ .fetch_word_cached, .flush, .m, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp m,nn
    &[_]Uop{ .set_ief, .add_r_1, .fetch_byte, .dispatch_base }, // ei
    &[_]Uop{.unimplemented}, // call m,nn
    &[_]Uop{.unimplemented}, // FD
    &[_]Uop{ .fetch_byte, .unimplemented }, // cp a,n
    &[_]Uop{.unimplemented}, // rst 38h
};

pub fn decode(impl: anytype) anyerror!void {
    try dispatchAll(impl, &[_]Uop{ .add_r_1, .fetch_byte, .dispatch_base });
}

fn dispatcherFor(comptime table: *const [1 << 8][]const Uop) fn (anytype, comptime u8) anyerror!void {
    return struct {
        fn dispatcher(impl: anytype, comptime opcode: u8) anyerror!void {
            try dispatchAll(impl, table[opcode]);
        }
    }.dispatcher;
}

fn dispatchAll(impl: anytype, comptime uops: []const Uop) anyerror!void {
    inline for (uops) |uop| try dispatch(impl, uop);
}

fn dispatch(impl: anytype, comptime uop: Uop) anyerror!void {
    try switch (uop) {
        .unimplemented => if (true)
            impl.skip()
        else
            std.debug.todo(@tagName(uop) ++ " opcode"),

        .save => impl.save(),
        .restore => impl.restore(),
        .swap => impl.swap(),

        .mask_word_inst => impl.maskWordInstruction(),
        .mask_addr_adl => impl.maskAddressAdl(),
        .mask_addr_inst => impl.maskAddressInstruction(),

        .add_r_1 => impl.addR(1),
        .add_cc_1 => impl.addCycles(1),

        .dispatch_base => impl.dispatch(dispatcherFor(&base)),

        .mode_sis => impl.setMode(.{ .inst = .s, .imm = .is }),
        .mode_lis => impl.setMode(.{ .inst = .l, .imm = .is }),
        .mode_sil => impl.setMode(.{ .inst = .s, .imm = .il }),
        .mode_lil => impl.setMode(.{ .inst = .l, .imm = .il }),
        .adl_inst => impl.setAdlInstruction(),
        .adl_imm => impl.setAdlImmediate(),
        .clear_ief, .set_ief => err: {
            try impl.set(@boolToInt(uop == .set_ief));
            try impl.storeRegister(.ief);
            break :err impl.storeShadowRegister(.ief);
        },

        .halt => impl.halt(),

        .nz => impl.checkCondition(.zf, 0),
        .z => impl.checkCondition(.zf, 1),
        .nc => impl.checkCondition(.cf, 0),
        .c => impl.checkCondition(.cf, 1),
        .po => impl.checkCondition(.pv, 0),
        .pe => impl.checkCondition(.pv, 1),
        .p => impl.checkCondition(.sf, 0),
        .m => impl.checkCondition(.sf, 1),

        .load_a_high => impl.loadRegisterHigh(.a),
        .load_b => impl.loadRegister(.b),
        .load_c => impl.loadRegister(.c),
        .load_d => impl.loadRegister(.d),
        .load_e => impl.loadRegister(.e),
        .load_h => impl.loadRegister(.h),
        .load_l => impl.loadRegister(.l),
        .load_a => impl.loadRegister(.a),
        .load_af => impl.loadRegister(.af),
        .load_bc => impl.loadRegister(.ubc),
        .load_de => impl.loadRegister(.ude),
        .load_hl => impl.loadRegister(.uhl),
        .load_sp => impl.loadStackPointer(),
        .load_pc => impl.loadShadowRegister(.pc),
        .@"load_af'" => impl.loadShadowRegister(.af),
        .@"load_bc'" => impl.loadShadowRegister(.ubc),
        .@"load_de'" => impl.loadShadowRegister(.ude),
        .@"load_hl'" => impl.loadShadowRegister(.uhl),

        .store_b => impl.storeRegister(.b),
        .store_c => impl.storeRegister(.c),
        .store_d => impl.storeRegister(.d),
        .store_e => impl.storeRegister(.e),
        .store_h => impl.storeRegister(.h),
        .store_l => impl.storeRegister(.l),
        .store_a => impl.storeRegister(.a),
        .store_af => impl.storeRegister(.af),
        .store_bc => impl.storeRegister(.ubc),
        .store_de => impl.storeRegister(.ude),
        .store_hl => impl.storeRegister(.uhl),
        .store_sp => impl.storeStackPointer(),
        .store_pc => impl.storeShadowRegister(.pc),
        .@"store_af'" => impl.storeShadowRegister(.af),
        .@"store_bc'" => impl.storeShadowRegister(.ubc),
        .@"store_de'" => impl.storeShadowRegister(.ude),
        .@"store_hl'" => impl.storeShadowRegister(.uhl),

        .flush => impl.flush(),
        .fetch_byte => impl.fetchByte(.prefetch),
        .fetch_word => impl.fetchWord(.prefetch),
        .fetch_word_cached => impl.fetchWord(.cached),

        .read_port => impl.readPortByte(),
        .read_byte => impl.readMemoryByte(),
        .read_word => impl.readMemoryWord(),

        .write_port => impl.writePortByte(),
        .write_byte => impl.writeMemoryByte(),
        .write_word => impl.writeMemoryWord(),

        .inc_byte => impl.addByte(1),
        .dec_byte => impl.addByte(-1),
        .inc_word => impl.addWord(1),
        .dec_word => impl.addWord(-1),
        .add_words => impl.addWords(),
    };
}
