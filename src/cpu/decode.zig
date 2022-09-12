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

    suffix: bool,
    inst: Instruction,
    imm: Immediate,

    pub fn fromAdl(adl: Cpu.Adl) Mode {
        return .{
            .suffix = false,
            .inst = Instruction.fromAdl(adl),
            .imm = Immediate.fromAdl(adl),
        };
    }
};
pub const PrefetchMode = enum(u1) { prefetch, cached };

pub const Direction = enum(u1) { forward, reverse };

const Uop = enum {
    unimplemented,

    set_00h,
    set_08h,
    set_10h,
    set_18h,
    set_20h,
    set_28h,
    set_30h,
    set_38h,
    save,
    restore,
    swap,

    mask_word_inst,
    mask_addr_adl,
    mask_addr_inst,

    add_r_1,
    add_r_inst,
    add_cc_1,
    add_cc_call,

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

    non_zero,
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

    ex_hl,
    ex_hl_inst,
    @"ex_af'",
    @"ex_bc'",
    @"ex_de'",
    @"ex_hl'",
    ex_pc,

    flush,
    fetch_byte,
    fetch_word,
    fetch_word_flush,

    read_port,
    read_byte,
    read_word,

    write_port,
    write_byte,
    write_word,
    write_word_rev,

    call,
    ret,

    rlca_byte,
    rrca_byte,
    rla_byte,
    rra_byte,
    daa_byte,
    cpl_byte,
    scf,
    ccf,
    inc_byte,
    dec_byte,
    inc_word,
    dec_word,
    inc_addr,
    dec_addr,
    add_offset,

    add_bytes,
    adc_bytes,
    sub_bytes,
    sbc_bytes,
    and_bytes,
    xor_bytes,
    or_bytes,

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
    &[_]Uop{ .load_a, .rlca_byte, .store_a }, // rlca
    &[_]Uop{ .load_af, .@"ex_af'", .store_af }, // ex af,af'
    &[_]Uop{ .load_bc, .save, .load_hl, .add_words, .store_hl }, // add hl,bc
    &[_]Uop{ .load_bc, .save, .mask_addr_inst, .read_byte, .store_a }, // ld a,(bc)
    &[_]Uop{ .load_bc, .dec_word, .mask_word_inst, .store_bc }, // dec bc
    &[_]Uop{ .load_c, .inc_byte, .store_c }, // inc c
    &[_]Uop{ .load_c, .dec_byte, .store_c }, // dec c
    &[_]Uop{ .fetch_byte, .store_c }, // ld c,n
    &[_]Uop{ .load_a, .rrca_byte, .store_a }, // rrca
    &[_]Uop{ .fetch_byte, .save, .load_b, .dec_word, .store_b, .non_zero, .flush, .add_cc_1, .load_pc, .dec_word, .add_offset, .mask_word_inst, .store_pc }, // djnz d
    &[_]Uop{ .fetch_word, .mask_word_inst, .store_de }, // ld de,nn
    &[_]Uop{ .load_de, .save, .mask_addr_inst, .load_a, .write_byte }, // ld (de),a
    &[_]Uop{ .load_de, .inc_word, .mask_word_inst, .store_de }, // inc de
    &[_]Uop{ .load_d, .inc_byte, .store_d }, // inc d
    &[_]Uop{ .load_d, .dec_byte, .store_d }, // dec d
    &[_]Uop{ .fetch_byte, .store_d }, // ld d,n
    &[_]Uop{ .load_a, .rla_byte, .store_a }, // rla
    &[_]Uop{ .fetch_byte, .save, .flush, .load_pc, .dec_word, .add_offset, .mask_word_inst, .store_pc }, // jr d
    &[_]Uop{ .load_de, .save, .load_hl, .add_words, .store_hl }, // add hl,de
    &[_]Uop{ .load_de, .save, .mask_addr_inst, .read_byte, .store_a }, // ld a,(de)
    &[_]Uop{ .load_de, .dec_word, .mask_word_inst, .store_de }, // dec de
    &[_]Uop{ .load_e, .inc_byte, .store_e }, // inc e
    &[_]Uop{ .load_e, .dec_byte, .store_e }, // dec e
    &[_]Uop{ .fetch_byte, .store_e }, // ld e,n
    &[_]Uop{ .load_a, .rra_byte, .store_a }, // rra
    &[_]Uop{ .fetch_byte, .nz, .flush, .add_cc_1, .save, .load_pc, .dec_word, .add_offset, .mask_word_inst, .store_pc }, // jr nz,d
    &[_]Uop{ .fetch_word, .mask_word_inst, .store_hl }, // ld hl,nn
    &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .load_hl, .write_word }, // ld (nn),hl
    &[_]Uop{ .load_hl, .inc_word, .mask_word_inst, .store_hl }, // inc hl
    &[_]Uop{ .load_h, .inc_byte, .store_h }, // inc h
    &[_]Uop{ .load_h, .dec_byte, .store_h }, // dec h
    &[_]Uop{ .fetch_byte, .store_h }, // ld h,n
    &[_]Uop{ .load_a, .daa_byte, .store_a }, // daa
    &[_]Uop{ .fetch_byte, .z, .flush, .add_cc_1, .save, .load_pc, .dec_word, .add_offset, .mask_word_inst, .store_pc }, // jr z,d
    &[_]Uop{ .load_hl, .save, .add_words, .store_hl }, // add hl,hl
    &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_word, .store_hl }, // ld hl,(nn)
    &[_]Uop{ .load_hl, .dec_word, .mask_word_inst, .store_hl }, // dec hl
    &[_]Uop{ .load_l, .inc_byte, .store_l }, // inc l
    &[_]Uop{ .load_l, .dec_byte, .store_l }, // dec l
    &[_]Uop{ .fetch_byte, .store_l }, // ld l,n
    &[_]Uop{ .load_a, .cpl_byte, .store_a }, // cpl
    &[_]Uop{ .fetch_byte, .nc, .flush, .add_cc_1, .save, .load_pc, .dec_word, .add_offset, .mask_word_inst, .store_pc }, // jr nc,d
    &[_]Uop{ .fetch_word, .save, .store_sp }, // ld sp,nn
    &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .load_a, .write_byte }, // ld (nn),a
    &[_]Uop{ .load_sp, .inc_addr, .store_sp }, // inc sp
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .inc_byte, .write_byte }, // inc (hl)
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .dec_byte, .write_byte }, // dec (hl)
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .fetch_byte, .write_byte }, // ld (hl),n
    &[_]Uop{.scf}, // scf
    &[_]Uop{ .fetch_byte, .c, .flush, .save, .add_cc_1, .load_pc, .dec_word, .add_offset, .mask_word_inst, .store_pc }, // jr c,d
    &[_]Uop{ .load_sp, .load_hl, .add_words, .store_hl }, // add hl,sp
    &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_byte, .store_a }, // ld a,(nn)
    &[_]Uop{ .load_sp, .dec_addr, .store_sp }, // dec sp
    &[_]Uop{ .load_a, .inc_byte, .store_a }, // inc a
    &[_]Uop{ .load_a, .dec_byte, .store_a }, // dec a
    &[_]Uop{ .fetch_byte, .store_a }, // ld a,n
    &[_]Uop{.ccf}, // ccf
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
    &[_]Uop{ .load_b, .save, .load_a, .add_bytes, .store_a }, // add a,b
    &[_]Uop{ .load_c, .save, .load_a, .add_bytes, .store_a }, // add a,c
    &[_]Uop{ .load_d, .save, .load_a, .add_bytes, .store_a }, // add a,d
    &[_]Uop{ .load_e, .save, .load_a, .add_bytes, .store_a }, // add a,e
    &[_]Uop{ .load_h, .save, .load_a, .add_bytes, .store_a }, // add a,h
    &[_]Uop{ .load_l, .save, .load_a, .add_bytes, .store_a }, // add a,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .save, .load_a, .add_bytes, .store_a }, // add a,(hl)
    &[_]Uop{ .load_a, .save, .add_bytes, .store_a }, // add a,a
    &[_]Uop{ .load_b, .save, .load_a, .adc_bytes, .store_a }, // adc a,b
    &[_]Uop{ .load_c, .save, .load_a, .adc_bytes, .store_a }, // adc a,c
    &[_]Uop{ .load_d, .save, .load_a, .adc_bytes, .store_a }, // adc a,d
    &[_]Uop{ .load_e, .save, .load_a, .adc_bytes, .store_a }, // adc a,e
    &[_]Uop{ .load_h, .save, .load_a, .adc_bytes, .store_a }, // adc a,h
    &[_]Uop{ .load_l, .save, .load_a, .adc_bytes, .store_a }, // adc a,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .save, .load_a, .adc_bytes, .store_a }, // adc a,(hl)
    &[_]Uop{ .load_a, .save, .adc_bytes, .store_a }, // adc a,a
    &[_]Uop{ .load_b, .save, .load_a, .sub_bytes, .store_a }, // sub a,b
    &[_]Uop{ .load_c, .save, .load_a, .sub_bytes, .store_a }, // sub a,c
    &[_]Uop{ .load_d, .save, .load_a, .sub_bytes, .store_a }, // sub a,d
    &[_]Uop{ .load_e, .save, .load_a, .sub_bytes, .store_a }, // sub a,e
    &[_]Uop{ .load_h, .save, .load_a, .sub_bytes, .store_a }, // sub a,h
    &[_]Uop{ .load_l, .save, .load_a, .sub_bytes, .store_a }, // sub a,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .save, .load_a, .sub_bytes, .store_a }, // sub a,(hl)
    &[_]Uop{ .load_a, .save, .sub_bytes, .store_a }, // sub a,a
    &[_]Uop{ .load_b, .save, .load_a, .sbc_bytes, .store_a }, // sbc a,b
    &[_]Uop{ .load_c, .save, .load_a, .sbc_bytes, .store_a }, // sbc a,c
    &[_]Uop{ .load_d, .save, .load_a, .sbc_bytes, .store_a }, // sbc a,d
    &[_]Uop{ .load_e, .save, .load_a, .sbc_bytes, .store_a }, // sbc a,e
    &[_]Uop{ .load_h, .save, .load_a, .sbc_bytes, .store_a }, // sbc a,h
    &[_]Uop{ .load_l, .save, .load_a, .sbc_bytes, .store_a }, // sbc a,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .save, .load_a, .sbc_bytes, .store_a }, // sbc a,(hl)
    &[_]Uop{ .load_a, .save, .sbc_bytes, .store_a }, // sbc a,a
    &[_]Uop{ .load_b, .save, .load_a, .and_bytes, .store_a }, // and a,b
    &[_]Uop{ .load_c, .save, .load_a, .and_bytes, .store_a }, // and a,c
    &[_]Uop{ .load_d, .save, .load_a, .and_bytes, .store_a }, // and a,d
    &[_]Uop{ .load_e, .save, .load_a, .and_bytes, .store_a }, // and a,e
    &[_]Uop{ .load_h, .save, .load_a, .and_bytes, .store_a }, // and a,h
    &[_]Uop{ .load_l, .save, .load_a, .and_bytes, .store_a }, // and a,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .save, .load_a, .and_bytes, .store_a }, // and a,(hl)
    &[_]Uop{ .load_a, .save, .and_bytes, .store_a }, // and a,a
    &[_]Uop{ .load_b, .save, .load_a, .xor_bytes, .store_a }, // xor a,b
    &[_]Uop{ .load_c, .save, .load_a, .xor_bytes, .store_a }, // xor a,c
    &[_]Uop{ .load_d, .save, .load_a, .xor_bytes, .store_a }, // xor a,d
    &[_]Uop{ .load_e, .save, .load_a, .xor_bytes, .store_a }, // xor a,e
    &[_]Uop{ .load_h, .save, .load_a, .xor_bytes, .store_a }, // xor a,h
    &[_]Uop{ .load_l, .save, .load_a, .xor_bytes, .store_a }, // xor a,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .save, .load_a, .xor_bytes, .store_a }, // xor a,(hl)
    &[_]Uop{ .load_a, .save, .xor_bytes, .store_a }, // xor a,a
    &[_]Uop{ .load_b, .save, .load_a, .or_bytes, .store_a }, // or a,b
    &[_]Uop{ .load_c, .save, .load_a, .or_bytes, .store_a }, // or a,c
    &[_]Uop{ .load_d, .save, .load_a, .or_bytes, .store_a }, // or a,d
    &[_]Uop{ .load_e, .save, .load_a, .or_bytes, .store_a }, // or a,e
    &[_]Uop{ .load_h, .save, .load_a, .or_bytes, .store_a }, // or a,h
    &[_]Uop{ .load_l, .save, .load_a, .or_bytes, .store_a }, // or a,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .save, .load_a, .or_bytes, .store_a }, // or a,(hl)
    &[_]Uop{ .load_a, .save, .or_bytes, .store_a }, // or a,a
    &[_]Uop{ .load_b, .save, .load_a, .sub_bytes }, // cp a,b
    &[_]Uop{ .load_c, .save, .load_a, .sub_bytes }, // cp a,c
    &[_]Uop{ .load_d, .save, .load_a, .sub_bytes }, // cp a,d
    &[_]Uop{ .load_e, .save, .load_a, .sub_bytes }, // cp a,e
    &[_]Uop{ .load_h, .save, .load_a, .sub_bytes }, // cp a,h
    &[_]Uop{ .load_l, .save, .load_a, .sub_bytes }, // cp a,l
    &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .save, .load_a, .sub_bytes }, // cp a,(hl)
    &[_]Uop{ .load_a, .save, .sub_bytes }, // cp a,a
    &[_]Uop{ .add_cc_1, .nz, .flush, .add_r_1, .ret, .store_pc, .add_cc_1 }, // ret nz
    &[_]Uop{ .load_sp, .mask_addr_inst, .read_word, .store_bc, .inc_addr, .mask_addr_inst, .store_sp }, // pop bc
    &[_]Uop{ .fetch_word_flush, .nz, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp nz,nn
    &[_]Uop{ .fetch_word_flush, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp nn
    &[_]Uop{ .fetch_word_flush, .nz, .add_cc_call, .ex_pc, .call }, // call nz,nn
    &[_]Uop{ .add_r_inst, .load_sp, .dec_addr, .mask_addr_inst, .load_bc, .write_word_rev, .store_sp }, // push bc
    &[_]Uop{ .fetch_byte, .save, .load_a, .add_bytes, .store_a }, // add a,n
    &[_]Uop{ .add_cc_1, .set_00h, .ex_pc, .call }, // rst 00h
    &[_]Uop{ .add_cc_1, .z, .flush, .add_r_1, .ret, .store_pc, .add_cc_1 }, // ret z
    &[_]Uop{ .flush, .ret, .store_pc, .add_cc_1 }, // ret
    &[_]Uop{ .fetch_word_flush, .z, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp z,nn
    &[_]Uop{.unimplemented}, // CB
    &[_]Uop{ .fetch_word_flush, .z, .add_cc_call, .ex_pc, .call }, // call z,nn
    &[_]Uop{ .fetch_word_flush, .ex_pc, .call }, // call nn
    &[_]Uop{ .fetch_byte, .save, .load_a, .adc_bytes, .store_a }, // adc a,n
    &[_]Uop{ .add_cc_1, .set_08h, .ex_pc, .call }, // rst 08h
    &[_]Uop{ .add_cc_1, .nc, .flush, .add_r_1, .ret, .store_pc, .add_cc_1 }, // ret nc
    &[_]Uop{ .load_sp, .mask_addr_inst, .read_word, .store_de, .inc_addr, .mask_addr_inst, .store_sp }, // pop de
    &[_]Uop{ .fetch_word_flush, .nc, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp nc,nn
    &[_]Uop{ .fetch_byte, .load_a_high, .save, .load_a, .write_port }, // out (n),a
    &[_]Uop{ .fetch_word_flush, .nc, .add_cc_call, .ex_pc, .call }, // call nc,nn
    &[_]Uop{ .add_r_inst, .load_sp, .dec_addr, .mask_addr_inst, .load_de, .write_word_rev, .store_sp }, // push de
    &[_]Uop{ .fetch_byte, .save, .load_a, .sub_bytes, .store_a }, // sub a,n
    &[_]Uop{ .add_cc_1, .set_10h, .ex_pc, .call }, // rst 10h
    &[_]Uop{ .add_cc_1, .c, .flush, .add_r_1, .ret, .store_pc, .add_cc_1 }, // ret c
    &[_]Uop{ .load_bc, .@"ex_bc'", .store_bc, .load_de, .@"ex_de'", .store_de, .load_hl, .@"ex_hl'", .store_hl }, // exx
    &[_]Uop{ .fetch_word_flush, .c, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp c,nn
    &[_]Uop{ .fetch_byte, .load_a_high, .save, .read_port, .store_a }, // in a,(n)
    &[_]Uop{ .fetch_word_flush, .c, .add_cc_call, .ex_pc, .call }, // call c,nn
    &[_]Uop{.unimplemented}, // DD
    &[_]Uop{ .fetch_byte, .save, .load_a, .sbc_bytes, .store_a }, // sbc a,n
    &[_]Uop{ .add_cc_1, .set_18h, .ex_pc, .call }, // rst 18h
    &[_]Uop{ .add_cc_1, .po, .flush, .add_r_1, .ret, .store_pc, .add_cc_1 }, // ret po
    &[_]Uop{ .load_sp, .mask_addr_inst, .read_word, .store_hl, .inc_addr, .mask_addr_inst, .store_sp }, // pop hl
    &[_]Uop{ .fetch_word_flush, .po, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp po,nn
    &[_]Uop{ .load_sp, .mask_addr_inst, .read_word, .ex_hl_inst, .write_word_rev }, // ex (sp),hl
    &[_]Uop{ .fetch_word_flush, .po, .add_cc_call, .ex_pc, .call }, // call po,nn
    &[_]Uop{ .add_r_inst, .load_sp, .dec_addr, .mask_addr_inst, .load_hl, .write_word_rev, .store_sp }, // push hl
    &[_]Uop{ .fetch_byte, .save, .load_a, .and_bytes, .store_a }, // and a,n
    &[_]Uop{ .add_cc_1, .set_20h, .ex_pc, .call }, // rst 20h
    &[_]Uop{ .add_cc_1, .pe, .flush, .add_r_1, .ret, .store_pc, .add_cc_1 }, // ret pe
    &[_]Uop{ .flush, .fetch_byte, .load_hl, .mask_word_inst, .store_pc, .adl_inst }, // jp (hl)
    &[_]Uop{ .fetch_word_flush, .pe, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp pe,nn
    &[_]Uop{ .load_de, .mask_word_inst, .ex_hl, .mask_word_inst, .store_de }, // ex de,hl
    &[_]Uop{ .fetch_word_flush, .pe, .add_cc_call, .ex_pc, .call }, // call pe,nn
    &[_]Uop{.unimplemented}, // ED
    &[_]Uop{ .fetch_byte, .save, .load_a, .xor_bytes, .store_a }, // xor a,n
    &[_]Uop{ .add_cc_1, .set_28h, .ex_pc, .call }, // rst 28h
    &[_]Uop{ .add_cc_1, .p, .flush, .add_r_1, .ret, .store_pc, .add_cc_1 }, // ret p
    &[_]Uop{ .load_sp, .mask_addr_inst, .read_word, .store_af, .inc_addr, .mask_addr_inst, .store_sp }, // pop af
    &[_]Uop{ .fetch_word_flush, .p, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp p,nn
    &[_]Uop{.clear_ief}, // di
    &[_]Uop{ .fetch_word_flush, .p, .add_cc_call, .ex_pc, .call }, // call p,nn
    &[_]Uop{ .add_r_inst, .load_sp, .dec_addr, .mask_addr_inst, .load_af, .write_word_rev, .store_sp }, // push af
    &[_]Uop{ .fetch_byte, .save, .load_a, .or_bytes, .store_a }, // or a,n
    &[_]Uop{ .add_cc_1, .set_30h, .ex_pc, .call }, // rst 30h
    &[_]Uop{ .add_cc_1, .m, .flush, .add_r_1, .ret, .store_pc, .add_cc_1 }, // ret m
    &[_]Uop{ .load_hl, .save, .store_sp }, // ld sp,hl
    &[_]Uop{ .fetch_word_flush, .m, .add_cc_1, .mask_word_inst, .store_pc, .adl_inst }, // jp m,nn
    &[_]Uop{ .set_ief, .add_r_1, .fetch_byte, .dispatch_base }, // ei
    &[_]Uop{ .fetch_word_flush, .m, .add_cc_call, .ex_pc, .call }, // call m,nn
    &[_]Uop{.unimplemented}, // FD
    &[_]Uop{ .fetch_byte, .save, .load_a, .sub_bytes }, // cp a,n
    &[_]Uop{ .add_cc_1, .set_38h, .ex_pc, .call }, // rst 38h
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

        .set_00h => impl.set(0x00),
        .set_08h => impl.set(0x08),
        .set_10h => impl.set(0x10),
        .set_18h => impl.set(0x18),
        .set_20h => impl.set(0x20),
        .set_28h => impl.set(0x28),
        .set_30h => impl.set(0x30),
        .set_38h => impl.set(0x38),
        .save => impl.save(),
        .restore => impl.restore(),
        .swap => impl.swap(),

        .mask_word_inst => impl.maskWordInstruction(),
        .mask_addr_adl => impl.maskAddressAdl(),
        .mask_addr_inst => impl.maskAddressInstruction(),

        .add_r_1 => impl.addR(1),
        .add_r_inst => impl.addRInstruction(),
        .add_cc_1 => impl.addCycles(1),
        .add_cc_call => impl.addCycleCall(),

        .dispatch_base => impl.dispatch(dispatcherFor(&base)),

        .mode_sis => impl.setMode(.{ .suffix = true, .inst = .s, .imm = .is }),
        .mode_lis => impl.setMode(.{ .suffix = true, .inst = .l, .imm = .is }),
        .mode_sil => impl.setMode(.{ .suffix = true, .inst = .s, .imm = .il }),
        .mode_lil => impl.setMode(.{ .suffix = true, .inst = .l, .imm = .il }),
        .adl_inst => impl.setAdlInstruction(),
        .adl_imm => impl.setAdlImmediate(),
        .clear_ief, .set_ief => err: {
            try impl.set(@boolToInt(uop == .set_ief));
            try impl.storeRegister(.ief);
            break :err impl.storeShadowRegister(.ief);
        },

        .halt => impl.halt(),

        .non_zero => impl.checkNonZero(),
        .nz => impl.checkCondition(.zf, false),
        .z => impl.checkCondition(.zf, true),
        .nc => impl.checkCondition(.cf, false),
        .c => impl.checkCondition(.cf, true),
        .po => impl.checkCondition(.pv, false),
        .pe => impl.checkCondition(.pv, true),
        .p => impl.checkCondition(.sf, false),
        .m => impl.checkCondition(.sf, true),

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
        .load_sp => impl.loadStackPointerInstruction(),
        .load_pc => impl.loadShadowRegister(.pc),

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
        .store_sp => impl.storeStackPointerInstruction(),
        .store_pc => impl.storeShadowRegister(.pc),

        .ex_hl => impl.exchangeRegister(.uhl),
        .ex_hl_inst => impl.exchangeRegisterInstruction(.hl, .uhl),
        .@"ex_af'" => impl.exchangeShadowRegister(.af),
        .@"ex_bc'" => impl.exchangeShadowRegister(.ubc),
        .@"ex_de'" => impl.exchangeShadowRegister(.ude),
        .@"ex_hl'" => impl.exchangeShadowRegister(.uhl),
        .ex_pc => impl.exchangeShadowRegister(.pc),

        .flush => impl.flush(),
        .fetch_byte => impl.fetchByte(.prefetch),
        .fetch_word => impl.fetchWord(.prefetch),
        .fetch_word_flush => err: {
            try impl.fetchWord(.cached);
            break :err impl.flush();
        },

        .read_port => impl.readPortByte(),
        .read_byte => impl.readMemoryByte(),
        .read_word => impl.readMemoryWord(),

        .write_port => impl.writePortByte(),
        .write_byte => impl.writeMemoryByte(),
        .write_word => impl.writeMemoryWord(.forward),
        .write_word_rev => impl.writeMemoryWord(.reverse),

        .call => impl.callSuffix(),
        .ret => impl.ret(),

        .rlca_byte => impl.rlcaByte(),
        .rrca_byte => impl.rrcaByte(),
        .rla_byte => impl.rlaByte(),
        .rra_byte => impl.rraByte(),
        .daa_byte => impl.daaByte(),
        .cpl_byte => impl.cplByte(),
        .scf => impl.scf(),
        .ccf => impl.ccf(),
        .inc_byte => impl.addByte(1),
        .dec_byte => impl.addByte(-1),
        .inc_word => impl.addWord(1),
        .dec_word => impl.addWord(-1),
        .inc_addr => impl.addAddress(1),
        .dec_addr => impl.addAddress(-1),
        .add_offset => impl.addOffset(),

        .add_bytes => impl.addBytes(),
        .adc_bytes => impl.adcBytes(),
        .sub_bytes => impl.subBytes(),
        .sbc_bytes => impl.sbcBytes(),
        .and_bytes => impl.andBytes(),
        .xor_bytes => impl.xorBytes(),
        .or_bytes => impl.orBytes(),

        .add_words => impl.addWords(),
    };
}
