const std = @import("std");

const CEmuCore = @import("../cemucore.zig");
const Cpu = @import("../cpu.zig");

pub const Error = error{RepeatInstruction};

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
    reset,

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

    sub_r_2,
    sub_r_1,
    add_r_1,
    add_r_2,
    add_r_inst,
    add_cc_1,
    add_cc_4,
    add_cc_call,

    dispatch_base,
    dispatch_cb,
    dispatch_dd,
    dispatch_ddcb,
    dispatch_ed,
    dispatch_fd,
    dispatch_fdcb,

    mode_sis,
    mode_lis,
    mode_sil,
    mode_lil,
    set_adl_inst,
    set_adl_imm,
    clear_madl,
    set_madl,
    clear_ief,
    set_ief,
    im_0,
    im_1,
    im_2,

    halt,

    non_zero,
    adl,
    nz,
    z,
    nc,
    c,
    po,
    pe,
    p,
    m,

    load_b,
    load_c,
    load_d,
    load_e,
    load_h,
    load_l,
    load_a,
    load_a_high,
    load_ixh,
    load_ixl,
    load_iyh,
    load_iyl,
    load_i,
    load_mbi,
    load_r,
    load_mb,
    load_af,
    load_bc,
    load_de,
    load_hl,
    load_ix,
    load_iy,
    load_sp,
    load_pc,

    store_b,
    store_c,
    store_d,
    store_e,
    store_h,
    store_l,
    store_a,
    store_a_high,
    store_ixh,
    store_ixl,
    store_iyh,
    store_iyl,
    store_i,
    store_r,
    store_mb,
    store_af,
    store_bc,
    store_bc_inst,
    store_de,
    store_hl,
    store_ix,
    store_iy,
    store_sp,
    store_pc,

    ex_hl,
    ex_hl_inst,
    ex_ix_inst,
    ex_iy_inst,
    @"ex_af'",
    @"ex_bc'",
    @"ex_de'",
    @"ex_hl'",
    ex_pc,

    flush,
    fetch_byte,
    fetch_word,
    fetch_word_flush,

    in,
    in_f,
    read_byte,
    read_word,

    out,
    write_byte,
    write_word,
    write_word_rev,

    interrupt,
    rst,
    call,
    ret,
    copy_ief,

    rlc_byte,
    rrc_byte,
    rl_byte,
    rr_byte,
    sla_byte,
    sra_byte,
    srl_byte,
    daa_byte,
    cpl_byte,
    bit_0_byte,
    bit_1_byte,
    bit_2_byte,
    bit_3_byte,
    bit_4_byte,
    bit_5_byte,
    bit_6_byte,
    bit_7_byte,
    res_0_byte,
    res_1_byte,
    res_2_byte,
    res_3_byte,
    res_4_byte,
    res_5_byte,
    res_6_byte,
    res_7_byte,
    set_0_byte,
    set_1_byte,
    set_2_byte,
    set_3_byte,
    set_4_byte,
    set_5_byte,
    set_6_byte,
    set_7_byte,
    nf_byte_sign,
    pzs_byte,
    zf_byte,
    zf_word,
    zs_byte,
    scf,
    ccf,
    inc_byte,
    dec_byte,
    dec_byte_hzs,
    add_byte_1,
    sub_byte_1,
    inc_word,
    dec_word,
    sub_word_2,
    sub_word_3,
    sub_word_suffix,
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
    mlt_bytes,
    rrd_bytes,
    rld_bytes,

    add_words,
    adc_words,
    sbc_words,

    ld_flags,
    cp_flags,
    repeat_flag,
    repeat,
};

const ez80 = struct {
    const base = [_][]const Uop{
        &[_]Uop{}, // nop
        &[_]Uop{ .fetch_word, .mask_word_inst, .store_bc }, // ld bc,nn
        &[_]Uop{ .load_bc, .save, .mask_addr_inst, .load_a, .write_byte }, // ld (bc),a
        &[_]Uop{ .load_bc, .inc_word, .mask_word_inst, .store_bc }, // inc bc
        &[_]Uop{ .load_b, .inc_byte, .store_b }, // inc b
        &[_]Uop{ .load_b, .dec_byte, .store_b }, // dec b
        &[_]Uop{ .fetch_byte, .store_b }, // ld b,n
        &[_]Uop{ .load_a, .rlc_byte, .store_a }, // rlca
        &[_]Uop{ .load_af, .@"ex_af'", .store_af }, // ex af,af'
        &[_]Uop{ .load_bc, .save, .load_hl, .add_words, .store_hl }, // add hl,bc
        &[_]Uop{ .load_bc, .save, .mask_addr_inst, .read_byte, .store_a }, // ld a,(bc)
        &[_]Uop{ .load_bc, .dec_word, .mask_word_inst, .store_bc }, // dec bc
        &[_]Uop{ .load_c, .inc_byte, .store_c }, // inc c
        &[_]Uop{ .load_c, .dec_byte, .store_c }, // dec c
        &[_]Uop{ .fetch_byte, .store_c }, // ld c,n
        &[_]Uop{ .load_a, .rrc_byte, .store_a }, // rrca
        &[_]Uop{ .fetch_byte, .save, .load_b, .sub_byte_1, .store_b, .non_zero, .flush, .add_cc_1, .load_pc, .dec_word, .add_offset, .mask_word_inst, .store_pc }, // djnz d
        &[_]Uop{ .fetch_word, .mask_word_inst, .store_de }, // ld de,nn
        &[_]Uop{ .load_de, .save, .mask_addr_inst, .load_a, .write_byte }, // ld (de),a
        &[_]Uop{ .load_de, .inc_word, .mask_word_inst, .store_de }, // inc de
        &[_]Uop{ .load_d, .inc_byte, .store_d }, // inc d
        &[_]Uop{ .load_d, .dec_byte, .store_d }, // dec d
        &[_]Uop{ .fetch_byte, .store_d }, // ld d,n
        &[_]Uop{ .load_a, .rl_byte, .store_a }, // rla
        &[_]Uop{ .fetch_byte, .save, .flush, .load_pc, .dec_word, .add_offset, .mask_word_inst, .store_pc }, // jr d
        &[_]Uop{ .load_de, .save, .load_hl, .add_words, .store_hl }, // add hl,de
        &[_]Uop{ .load_de, .save, .mask_addr_inst, .read_byte, .store_a }, // ld a,(de)
        &[_]Uop{ .load_de, .dec_word, .mask_word_inst, .store_de }, // dec de
        &[_]Uop{ .load_e, .inc_byte, .store_e }, // inc e
        &[_]Uop{ .load_e, .dec_byte, .store_e }, // dec e
        &[_]Uop{ .fetch_byte, .store_e }, // ld e,n
        &[_]Uop{ .load_a, .rr_byte, .store_a }, // rra
        &[_]Uop{ .fetch_byte, .nz, .flush, .add_cc_1, .save, .load_pc, .dec_word, .add_offset, .mask_word_inst, .store_pc }, // jr nz,d
        &[_]Uop{ .fetch_word, .mask_word_inst, .store_hl }, // ld hl,nn
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .load_hl, .write_word }, // ld (nn),hl
        &[_]Uop{ .load_hl, .inc_word, .mask_word_inst, .store_hl }, // inc hl
        &[_]Uop{ .load_h, .inc_byte, .store_h }, // inc h
        &[_]Uop{ .load_h, .dec_byte, .store_h }, // dec h
        &[_]Uop{ .fetch_byte, .store_h }, // ld h,n
        &[_]Uop{ .load_a, .daa_byte, .pzs_byte, .store_a }, // daa
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
        &[_]Uop{ .load_b, .save, .load_a, .and_bytes, .pzs_byte, .store_a }, // and a,b
        &[_]Uop{ .load_c, .save, .load_a, .and_bytes, .pzs_byte, .store_a }, // and a,c
        &[_]Uop{ .load_d, .save, .load_a, .and_bytes, .pzs_byte, .store_a }, // and a,d
        &[_]Uop{ .load_e, .save, .load_a, .and_bytes, .pzs_byte, .store_a }, // and a,e
        &[_]Uop{ .load_h, .save, .load_a, .and_bytes, .pzs_byte, .store_a }, // and a,h
        &[_]Uop{ .load_l, .save, .load_a, .and_bytes, .pzs_byte, .store_a }, // and a,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .save, .load_a, .and_bytes, .pzs_byte, .store_a }, // and a,(hl)
        &[_]Uop{ .load_a, .save, .and_bytes, .pzs_byte, .store_a }, // and a,a
        &[_]Uop{ .load_b, .save, .load_a, .xor_bytes, .pzs_byte, .store_a }, // xor a,b
        &[_]Uop{ .load_c, .save, .load_a, .xor_bytes, .pzs_byte, .store_a }, // xor a,c
        &[_]Uop{ .load_d, .save, .load_a, .xor_bytes, .pzs_byte, .store_a }, // xor a,d
        &[_]Uop{ .load_e, .save, .load_a, .xor_bytes, .pzs_byte, .store_a }, // xor a,e
        &[_]Uop{ .load_h, .save, .load_a, .xor_bytes, .pzs_byte, .store_a }, // xor a,h
        &[_]Uop{ .load_l, .save, .load_a, .xor_bytes, .pzs_byte, .store_a }, // xor a,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .save, .load_a, .xor_bytes, .pzs_byte, .store_a }, // xor a,(hl)
        &[_]Uop{ .load_a, .save, .xor_bytes, .pzs_byte, .store_a }, // xor a,a
        &[_]Uop{ .load_b, .save, .load_a, .or_bytes, .pzs_byte, .store_a }, // or a,b
        &[_]Uop{ .load_c, .save, .load_a, .or_bytes, .pzs_byte, .store_a }, // or a,c
        &[_]Uop{ .load_d, .save, .load_a, .or_bytes, .pzs_byte, .store_a }, // or a,d
        &[_]Uop{ .load_e, .save, .load_a, .or_bytes, .pzs_byte, .store_a }, // or a,e
        &[_]Uop{ .load_h, .save, .load_a, .or_bytes, .pzs_byte, .store_a }, // or a,h
        &[_]Uop{ .load_l, .save, .load_a, .or_bytes, .pzs_byte, .store_a }, // or a,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .save, .load_a, .or_bytes, .pzs_byte, .store_a }, // or a,(hl)
        &[_]Uop{ .load_a, .save, .or_bytes, .pzs_byte, .store_a }, // or a,a
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
        &[_]Uop{ .fetch_word_flush, .nz, .add_cc_1, .mask_word_inst, .store_pc, .set_adl_inst }, // jp nz,nn
        &[_]Uop{ .fetch_word_flush, .add_cc_1, .mask_word_inst, .store_pc, .set_adl_inst }, // jp nn
        &[_]Uop{ .fetch_word_flush, .nz, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call nz,nn
        &[_]Uop{ .add_r_inst, .load_sp, .dec_addr, .mask_addr_inst, .load_bc, .write_word_rev, .store_sp }, // push bc
        &[_]Uop{ .fetch_byte, .save, .load_a, .add_bytes, .store_a }, // add a,n
        &[_]Uop{ .flush, .add_cc_1, .set_00h, .ex_pc, .rst, .set_adl_inst }, // rst 00h
        &[_]Uop{ .add_cc_1, .z, .flush, .add_r_1, .ret, .store_pc, .add_cc_1 }, // ret z
        &[_]Uop{ .flush, .ret, .store_pc, .add_cc_1 }, // ret
        &[_]Uop{ .fetch_word_flush, .z, .add_cc_1, .mask_word_inst, .store_pc, .set_adl_inst }, // jp z,nn
        &[_]Uop{ .add_r_1, .fetch_byte, .dispatch_cb }, // CB
        &[_]Uop{ .fetch_word_flush, .z, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call z,nn
        &[_]Uop{ .fetch_word_flush, .ex_pc, .call, .set_adl_imm }, // call nn
        &[_]Uop{ .fetch_byte, .save, .load_a, .adc_bytes, .store_a }, // adc a,n
        &[_]Uop{ .flush, .add_cc_1, .set_08h, .ex_pc, .rst, .set_adl_inst }, // rst 08h
        &[_]Uop{ .add_cc_1, .nc, .flush, .add_r_1, .ret, .store_pc, .add_cc_1 }, // ret nc
        &[_]Uop{ .load_sp, .mask_addr_inst, .read_word, .store_de, .inc_addr, .mask_addr_inst, .store_sp }, // pop de
        &[_]Uop{ .fetch_word_flush, .nc, .add_cc_1, .mask_word_inst, .store_pc, .set_adl_inst }, // jp nc,nn
        &[_]Uop{ .fetch_byte, .load_a_high, .save, .load_a, .out }, // out (n),a
        &[_]Uop{ .fetch_word_flush, .nc, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call nc,nn
        &[_]Uop{ .add_r_inst, .load_sp, .dec_addr, .mask_addr_inst, .load_de, .write_word_rev, .store_sp }, // push de
        &[_]Uop{ .fetch_byte, .save, .load_a, .sub_bytes, .store_a }, // sub a,n
        &[_]Uop{ .flush, .add_cc_1, .set_10h, .ex_pc, .rst, .set_adl_inst }, // rst 10h
        &[_]Uop{ .add_cc_1, .c, .flush, .add_r_1, .ret, .store_pc, .add_cc_1 }, // ret c
        &[_]Uop{ .load_bc, .@"ex_bc'", .store_bc, .load_de, .@"ex_de'", .store_de, .load_hl, .@"ex_hl'", .store_hl }, // exx
        &[_]Uop{ .fetch_word_flush, .c, .add_cc_1, .mask_word_inst, .store_pc, .set_adl_inst }, // jp c,nn
        &[_]Uop{ .fetch_byte, .load_a_high, .save, .in, .store_a }, // in a,(n)
        &[_]Uop{ .fetch_word_flush, .c, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call c,nn
        &[_]Uop{ .add_r_1, .fetch_byte, .dispatch_dd }, // DD
        &[_]Uop{ .fetch_byte, .save, .load_a, .sbc_bytes, .store_a }, // sbc a,n
        &[_]Uop{ .flush, .add_cc_1, .set_18h, .ex_pc, .rst, .set_adl_inst }, // rst 18h
        &[_]Uop{ .add_cc_1, .po, .flush, .add_r_1, .ret, .store_pc, .add_cc_1 }, // ret po
        &[_]Uop{ .load_sp, .mask_addr_inst, .read_word, .store_hl, .inc_addr, .mask_addr_inst, .store_sp }, // pop hl
        &[_]Uop{ .fetch_word_flush, .po, .add_cc_1, .mask_word_inst, .store_pc, .set_adl_inst }, // jp po,nn
        &[_]Uop{ .load_sp, .mask_addr_inst, .read_word, .ex_hl_inst, .write_word_rev }, // ex (sp),hl
        &[_]Uop{ .fetch_word_flush, .po, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call po,nn
        &[_]Uop{ .add_r_inst, .load_sp, .dec_addr, .mask_addr_inst, .load_hl, .write_word_rev, .store_sp }, // push hl
        &[_]Uop{ .fetch_byte, .save, .load_a, .and_bytes, .pzs_byte, .store_a }, // and a,n
        &[_]Uop{ .flush, .add_cc_1, .set_20h, .ex_pc, .rst, .set_adl_inst }, // rst 20h
        &[_]Uop{ .add_cc_1, .pe, .flush, .add_r_1, .ret, .store_pc, .add_cc_1 }, // ret pe
        &[_]Uop{ .flush, .fetch_byte, .load_hl, .mask_word_inst, .store_pc, .set_adl_inst }, // jp (hl)
        &[_]Uop{ .fetch_word_flush, .pe, .add_cc_1, .mask_word_inst, .store_pc, .set_adl_inst }, // jp pe,nn
        &[_]Uop{ .load_de, .mask_word_inst, .ex_hl, .mask_word_inst, .store_de }, // ex de,hl
        &[_]Uop{ .fetch_word_flush, .pe, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call pe,nn
        &[_]Uop{ .add_r_1, .fetch_byte, .dispatch_ed }, // ED
        &[_]Uop{ .fetch_byte, .save, .load_a, .xor_bytes, .pzs_byte, .store_a }, // xor a,n
        &[_]Uop{ .flush, .add_cc_1, .set_28h, .ex_pc, .rst, .set_adl_inst }, // rst 28h
        &[_]Uop{ .add_cc_1, .p, .flush, .add_r_1, .ret, .store_pc, .add_cc_1 }, // ret p
        &[_]Uop{ .load_sp, .mask_addr_inst, .read_word, .store_af, .inc_addr, .mask_addr_inst, .store_sp }, // pop af
        &[_]Uop{ .fetch_word_flush, .p, .add_cc_1, .mask_word_inst, .store_pc, .set_adl_inst }, // jp p,nn
        &[_]Uop{.clear_ief}, // di
        &[_]Uop{ .fetch_word_flush, .p, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call p,nn
        &[_]Uop{ .add_r_inst, .load_sp, .dec_addr, .mask_addr_inst, .load_af, .write_word_rev, .store_sp }, // push af
        &[_]Uop{ .fetch_byte, .save, .load_a, .or_bytes, .pzs_byte, .store_a }, // or a,n
        &[_]Uop{ .flush, .add_cc_1, .set_30h, .ex_pc, .rst, .set_adl_inst }, // rst 30h
        &[_]Uop{ .add_cc_1, .m, .flush, .add_r_1, .ret, .store_pc, .add_cc_1 }, // ret m
        &[_]Uop{ .load_hl, .save, .store_sp }, // ld sp,hl
        &[_]Uop{ .fetch_word_flush, .m, .add_cc_1, .mask_word_inst, .store_pc, .set_adl_inst }, // jp m,nn
        &[_]Uop{ .set_ief, .reset, .add_r_1, .fetch_byte, .dispatch_base }, // ei
        &[_]Uop{ .fetch_word_flush, .m, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call m,nn
        &[_]Uop{ .add_r_1, .fetch_byte, .dispatch_fd }, // FD
        &[_]Uop{ .fetch_byte, .save, .load_a, .sub_bytes }, // cp a,n
        &[_]Uop{ .flush, .add_cc_1, .set_38h, .ex_pc, .rst, .set_adl_inst }, // rst 38h
    };

    const cb = [_][]const Uop{
        &[_]Uop{ .load_b, .rlc_byte, .pzs_byte, .store_b }, // rlc b
        &[_]Uop{ .load_c, .rlc_byte, .pzs_byte, .store_c }, // rlc c
        &[_]Uop{ .load_d, .rlc_byte, .pzs_byte, .store_d }, // rlc d
        &[_]Uop{ .load_e, .rlc_byte, .pzs_byte, .store_e }, // rlc e
        &[_]Uop{ .load_h, .rlc_byte, .pzs_byte, .store_h }, // rlc h
        &[_]Uop{ .load_l, .rlc_byte, .pzs_byte, .store_l }, // rlc l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rlc_byte, .pzs_byte, .write_byte }, // rlc (hl)
        &[_]Uop{ .load_a, .rlc_byte, .pzs_byte, .store_a }, // rlc a
        &[_]Uop{ .load_b, .rrc_byte, .pzs_byte, .store_b }, // rrc b
        &[_]Uop{ .load_c, .rrc_byte, .pzs_byte, .store_c }, // rrc c
        &[_]Uop{ .load_d, .rrc_byte, .pzs_byte, .store_d }, // rrc d
        &[_]Uop{ .load_e, .rrc_byte, .pzs_byte, .store_e }, // rrc e
        &[_]Uop{ .load_h, .rrc_byte, .pzs_byte, .store_h }, // rrc h
        &[_]Uop{ .load_l, .rrc_byte, .pzs_byte, .store_l }, // rrc l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rrc_byte, .pzs_byte, .write_byte }, // rrc (hl)
        &[_]Uop{ .load_a, .rrc_byte, .pzs_byte, .store_a }, // rrc a
        &[_]Uop{ .load_b, .rl_byte, .pzs_byte, .store_b }, // rl b
        &[_]Uop{ .load_c, .rl_byte, .pzs_byte, .store_c }, // rl c
        &[_]Uop{ .load_d, .rl_byte, .pzs_byte, .store_d }, // rl d
        &[_]Uop{ .load_e, .rl_byte, .pzs_byte, .store_e }, // rl e
        &[_]Uop{ .load_h, .rl_byte, .pzs_byte, .store_h }, // rl h
        &[_]Uop{ .load_l, .rl_byte, .pzs_byte, .store_l }, // rl l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rl_byte, .pzs_byte, .write_byte }, // rl (hl)
        &[_]Uop{ .load_a, .rl_byte, .pzs_byte, .store_a }, // rl a
        &[_]Uop{ .load_b, .rr_byte, .pzs_byte, .store_b }, // rr b
        &[_]Uop{ .load_c, .rr_byte, .pzs_byte, .store_c }, // rr c
        &[_]Uop{ .load_d, .rr_byte, .pzs_byte, .store_d }, // rr d
        &[_]Uop{ .load_e, .rr_byte, .pzs_byte, .store_e }, // rr e
        &[_]Uop{ .load_h, .rr_byte, .pzs_byte, .store_h }, // rr h
        &[_]Uop{ .load_l, .rr_byte, .pzs_byte, .store_l }, // rr l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rr_byte, .pzs_byte, .write_byte }, // rr (hl)
        &[_]Uop{ .load_a, .rr_byte, .pzs_byte, .store_a }, // rr a
        &[_]Uop{ .load_b, .sla_byte, .pzs_byte, .store_b }, // sla b
        &[_]Uop{ .load_c, .sla_byte, .pzs_byte, .store_c }, // sla c
        &[_]Uop{ .load_d, .sla_byte, .pzs_byte, .store_d }, // sla d
        &[_]Uop{ .load_e, .sla_byte, .pzs_byte, .store_e }, // sla e
        &[_]Uop{ .load_h, .sla_byte, .pzs_byte, .store_h }, // sla h
        &[_]Uop{ .load_l, .sla_byte, .pzs_byte, .store_l }, // sla l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sla_byte, .pzs_byte, .write_byte }, // sla (hl)
        &[_]Uop{ .load_a, .sla_byte, .pzs_byte, .store_a }, // sla a
        &[_]Uop{ .load_b, .sra_byte, .pzs_byte, .store_b }, // sra b
        &[_]Uop{ .load_c, .sra_byte, .pzs_byte, .store_c }, // sra c
        &[_]Uop{ .load_d, .sra_byte, .pzs_byte, .store_d }, // sra d
        &[_]Uop{ .load_e, .sra_byte, .pzs_byte, .store_e }, // sra e
        &[_]Uop{ .load_h, .sra_byte, .pzs_byte, .store_h }, // sra h
        &[_]Uop{ .load_l, .sra_byte, .pzs_byte, .store_l }, // sra l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sra_byte, .pzs_byte, .write_byte }, // sra (hl)
        &[_]Uop{ .load_a, .sra_byte, .pzs_byte, .store_a }, // sra a
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_b, .srl_byte, .pzs_byte, .store_b }, // srl b
        &[_]Uop{ .load_c, .srl_byte, .pzs_byte, .store_c }, // srl c
        &[_]Uop{ .load_d, .srl_byte, .pzs_byte, .store_d }, // srl d
        &[_]Uop{ .load_e, .srl_byte, .pzs_byte, .store_e }, // srl e
        &[_]Uop{ .load_h, .srl_byte, .pzs_byte, .store_h }, // srl h
        &[_]Uop{ .load_l, .srl_byte, .pzs_byte, .store_l }, // srl l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .srl_byte, .pzs_byte, .write_byte }, // srl (hl)
        &[_]Uop{ .load_a, .srl_byte, .pzs_byte, .store_a }, // srl a
        &[_]Uop{ .load_b, .bit_0_byte, .pzs_byte }, // bit 0,b
        &[_]Uop{ .load_c, .bit_0_byte, .pzs_byte }, // bit 0,c
        &[_]Uop{ .load_d, .bit_0_byte, .pzs_byte }, // bit 0,d
        &[_]Uop{ .load_e, .bit_0_byte, .pzs_byte }, // bit 0,e
        &[_]Uop{ .load_h, .bit_0_byte, .pzs_byte }, // bit 0,h
        &[_]Uop{ .load_l, .bit_0_byte, .pzs_byte }, // bit 0,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .bit_0_byte, .pzs_byte }, // bit 0,(hl)
        &[_]Uop{ .load_a, .bit_0_byte, .pzs_byte }, // bit 0,a
        &[_]Uop{ .load_b, .bit_1_byte, .pzs_byte }, // bit 1,b
        &[_]Uop{ .load_c, .bit_1_byte, .pzs_byte }, // bit 1,c
        &[_]Uop{ .load_d, .bit_1_byte, .pzs_byte }, // bit 1,d
        &[_]Uop{ .load_e, .bit_1_byte, .pzs_byte }, // bit 1,e
        &[_]Uop{ .load_h, .bit_1_byte, .pzs_byte }, // bit 1,h
        &[_]Uop{ .load_l, .bit_1_byte, .pzs_byte }, // bit 1,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .bit_1_byte, .pzs_byte }, // bit 1,(hl)
        &[_]Uop{ .load_a, .bit_1_byte, .pzs_byte }, // bit 1,a
        &[_]Uop{ .load_b, .bit_2_byte, .pzs_byte }, // bit 2,b
        &[_]Uop{ .load_c, .bit_2_byte, .pzs_byte }, // bit 2,c
        &[_]Uop{ .load_d, .bit_2_byte, .pzs_byte }, // bit 2,d
        &[_]Uop{ .load_e, .bit_2_byte, .pzs_byte }, // bit 2,e
        &[_]Uop{ .load_h, .bit_2_byte, .pzs_byte }, // bit 2,h
        &[_]Uop{ .load_l, .bit_2_byte, .pzs_byte }, // bit 2,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .bit_2_byte, .pzs_byte }, // bit 2,(hl)
        &[_]Uop{ .load_a, .bit_2_byte, .pzs_byte }, // bit 2,a
        &[_]Uop{ .load_b, .bit_3_byte, .pzs_byte }, // bit 3,b
        &[_]Uop{ .load_c, .bit_3_byte, .pzs_byte }, // bit 3,c
        &[_]Uop{ .load_d, .bit_3_byte, .pzs_byte }, // bit 3,d
        &[_]Uop{ .load_e, .bit_3_byte, .pzs_byte }, // bit 3,e
        &[_]Uop{ .load_h, .bit_3_byte, .pzs_byte }, // bit 3,h
        &[_]Uop{ .load_l, .bit_3_byte, .pzs_byte }, // bit 3,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .bit_3_byte, .pzs_byte }, // bit 3,(hl)
        &[_]Uop{ .load_a, .bit_3_byte, .pzs_byte }, // bit 3,a
        &[_]Uop{ .load_b, .bit_4_byte, .pzs_byte }, // bit 4,b
        &[_]Uop{ .load_c, .bit_4_byte, .pzs_byte }, // bit 4,c
        &[_]Uop{ .load_d, .bit_4_byte, .pzs_byte }, // bit 4,d
        &[_]Uop{ .load_e, .bit_4_byte, .pzs_byte }, // bit 4,e
        &[_]Uop{ .load_h, .bit_4_byte, .pzs_byte }, // bit 4,h
        &[_]Uop{ .load_l, .bit_4_byte, .pzs_byte }, // bit 4,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .bit_4_byte, .pzs_byte }, // bit 4,(hl)
        &[_]Uop{ .load_a, .bit_4_byte, .pzs_byte }, // bit 4,a
        &[_]Uop{ .load_b, .bit_5_byte, .pzs_byte }, // bit 5,b
        &[_]Uop{ .load_c, .bit_5_byte, .pzs_byte }, // bit 5,c
        &[_]Uop{ .load_d, .bit_5_byte, .pzs_byte }, // bit 5,d
        &[_]Uop{ .load_e, .bit_5_byte, .pzs_byte }, // bit 5,e
        &[_]Uop{ .load_h, .bit_5_byte, .pzs_byte }, // bit 5,h
        &[_]Uop{ .load_l, .bit_5_byte, .pzs_byte }, // bit 5,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .bit_5_byte, .pzs_byte }, // bit 5,(hl)
        &[_]Uop{ .load_a, .bit_5_byte, .pzs_byte }, // bit 5,a
        &[_]Uop{ .load_b, .bit_6_byte, .pzs_byte }, // bit 6,b
        &[_]Uop{ .load_c, .bit_6_byte, .pzs_byte }, // bit 6,c
        &[_]Uop{ .load_d, .bit_6_byte, .pzs_byte }, // bit 6,d
        &[_]Uop{ .load_e, .bit_6_byte, .pzs_byte }, // bit 6,e
        &[_]Uop{ .load_h, .bit_6_byte, .pzs_byte }, // bit 6,h
        &[_]Uop{ .load_l, .bit_6_byte, .pzs_byte }, // bit 6,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .bit_6_byte, .pzs_byte }, // bit 6,(hl)
        &[_]Uop{ .load_a, .bit_6_byte, .pzs_byte }, // bit 6,a
        &[_]Uop{ .load_b, .bit_7_byte, .pzs_byte }, // bit 7,b
        &[_]Uop{ .load_c, .bit_7_byte, .pzs_byte }, // bit 7,c
        &[_]Uop{ .load_d, .bit_7_byte, .pzs_byte }, // bit 7,d
        &[_]Uop{ .load_e, .bit_7_byte, .pzs_byte }, // bit 7,e
        &[_]Uop{ .load_h, .bit_7_byte, .pzs_byte }, // bit 7,h
        &[_]Uop{ .load_l, .bit_7_byte, .pzs_byte }, // bit 7,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .bit_7_byte, .pzs_byte }, // bit 7,(hl)
        &[_]Uop{ .load_a, .bit_7_byte, .pzs_byte }, // bit 7,a
        &[_]Uop{ .load_b, .res_0_byte, .store_b }, // res 0,b
        &[_]Uop{ .load_c, .res_0_byte, .store_c }, // res 0,c
        &[_]Uop{ .load_d, .res_0_byte, .store_d }, // res 0,d
        &[_]Uop{ .load_e, .res_0_byte, .store_e }, // res 0,e
        &[_]Uop{ .load_h, .res_0_byte, .store_h }, // res 0,h
        &[_]Uop{ .load_l, .res_0_byte, .store_l }, // res 0,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_0_byte, .write_byte }, // res 0,(hl)
        &[_]Uop{ .load_a, .res_0_byte, .store_a }, // res 0,a
        &[_]Uop{ .load_b, .res_1_byte, .store_b }, // res 1,b
        &[_]Uop{ .load_c, .res_1_byte, .store_c }, // res 1,c
        &[_]Uop{ .load_d, .res_1_byte, .store_d }, // res 1,d
        &[_]Uop{ .load_e, .res_1_byte, .store_e }, // res 1,e
        &[_]Uop{ .load_h, .res_1_byte, .store_h }, // res 1,h
        &[_]Uop{ .load_l, .res_1_byte, .store_l }, // res 1,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_1_byte, .write_byte }, // res 1,(hl)
        &[_]Uop{ .load_a, .res_1_byte, .store_a }, // res 1,a
        &[_]Uop{ .load_b, .res_2_byte, .store_b }, // res 2,b
        &[_]Uop{ .load_c, .res_2_byte, .store_c }, // res 2,c
        &[_]Uop{ .load_d, .res_2_byte, .store_d }, // res 2,d
        &[_]Uop{ .load_e, .res_2_byte, .store_e }, // res 2,e
        &[_]Uop{ .load_h, .res_2_byte, .store_h }, // res 2,h
        &[_]Uop{ .load_l, .res_2_byte, .store_l }, // res 2,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_2_byte, .write_byte }, // res 2,(hl)
        &[_]Uop{ .load_a, .res_2_byte, .store_a }, // res 2,a
        &[_]Uop{ .load_b, .res_3_byte, .store_b }, // res 3,b
        &[_]Uop{ .load_c, .res_3_byte, .store_c }, // res 3,c
        &[_]Uop{ .load_d, .res_3_byte, .store_d }, // res 3,d
        &[_]Uop{ .load_e, .res_3_byte, .store_e }, // res 3,e
        &[_]Uop{ .load_h, .res_3_byte, .store_h }, // res 3,h
        &[_]Uop{ .load_l, .res_3_byte, .store_l }, // res 3,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_3_byte, .write_byte }, // res 3,(hl)
        &[_]Uop{ .load_a, .res_3_byte, .store_a }, // res 3,a
        &[_]Uop{ .load_b, .res_4_byte, .store_b }, // res 4,b
        &[_]Uop{ .load_c, .res_4_byte, .store_c }, // res 4,c
        &[_]Uop{ .load_d, .res_4_byte, .store_d }, // res 4,d
        &[_]Uop{ .load_e, .res_4_byte, .store_e }, // res 4,e
        &[_]Uop{ .load_h, .res_4_byte, .store_h }, // res 4,h
        &[_]Uop{ .load_l, .res_4_byte, .store_l }, // res 4,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_4_byte, .write_byte }, // res 4,(hl)
        &[_]Uop{ .load_a, .res_4_byte, .store_a }, // res 4,a
        &[_]Uop{ .load_b, .res_5_byte, .store_b }, // res 5,b
        &[_]Uop{ .load_c, .res_5_byte, .store_c }, // res 5,c
        &[_]Uop{ .load_d, .res_5_byte, .store_d }, // res 5,d
        &[_]Uop{ .load_e, .res_5_byte, .store_e }, // res 5,e
        &[_]Uop{ .load_h, .res_5_byte, .store_h }, // res 5,h
        &[_]Uop{ .load_l, .res_5_byte, .store_l }, // res 5,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_5_byte, .write_byte }, // res 5,(hl)
        &[_]Uop{ .load_a, .res_5_byte, .store_a }, // res 5,a
        &[_]Uop{ .load_b, .res_6_byte, .store_b }, // res 6,b
        &[_]Uop{ .load_c, .res_6_byte, .store_c }, // res 6,c
        &[_]Uop{ .load_d, .res_6_byte, .store_d }, // res 6,d
        &[_]Uop{ .load_e, .res_6_byte, .store_e }, // res 6,e
        &[_]Uop{ .load_h, .res_6_byte, .store_h }, // res 6,h
        &[_]Uop{ .load_l, .res_6_byte, .store_l }, // res 6,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_6_byte, .write_byte }, // res 6,(hl)
        &[_]Uop{ .load_a, .res_6_byte, .store_a }, // res 6,a
        &[_]Uop{ .load_b, .res_7_byte, .store_b }, // res 7,b
        &[_]Uop{ .load_c, .res_7_byte, .store_c }, // res 7,c
        &[_]Uop{ .load_d, .res_7_byte, .store_d }, // res 7,d
        &[_]Uop{ .load_e, .res_7_byte, .store_e }, // res 7,e
        &[_]Uop{ .load_h, .res_7_byte, .store_h }, // res 7,h
        &[_]Uop{ .load_l, .res_7_byte, .store_l }, // res 7,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_7_byte, .write_byte }, // res 7,(hl)
        &[_]Uop{ .load_a, .res_7_byte, .store_a }, // res 7,a
        &[_]Uop{ .load_b, .set_0_byte, .store_b }, // set 0,b
        &[_]Uop{ .load_c, .set_0_byte, .store_c }, // set 0,c
        &[_]Uop{ .load_d, .set_0_byte, .store_d }, // set 0,d
        &[_]Uop{ .load_e, .set_0_byte, .store_e }, // set 0,e
        &[_]Uop{ .load_h, .set_0_byte, .store_h }, // set 0,h
        &[_]Uop{ .load_l, .set_0_byte, .store_l }, // set 0,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_0_byte, .write_byte }, // set 0,(hl)
        &[_]Uop{ .load_a, .set_0_byte, .store_a }, // set 0,a
        &[_]Uop{ .load_b, .set_1_byte, .store_b }, // set 1,b
        &[_]Uop{ .load_c, .set_1_byte, .store_c }, // set 1,c
        &[_]Uop{ .load_d, .set_1_byte, .store_d }, // set 1,d
        &[_]Uop{ .load_e, .set_1_byte, .store_e }, // set 1,e
        &[_]Uop{ .load_h, .set_1_byte, .store_h }, // set 1,h
        &[_]Uop{ .load_l, .set_1_byte, .store_l }, // set 1,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_1_byte, .write_byte }, // set 1,(hl)
        &[_]Uop{ .load_a, .set_1_byte, .store_a }, // set 1,a
        &[_]Uop{ .load_b, .set_2_byte, .store_b }, // set 2,b
        &[_]Uop{ .load_c, .set_2_byte, .store_c }, // set 2,c
        &[_]Uop{ .load_d, .set_2_byte, .store_d }, // set 2,d
        &[_]Uop{ .load_e, .set_2_byte, .store_e }, // set 2,e
        &[_]Uop{ .load_h, .set_2_byte, .store_h }, // set 2,h
        &[_]Uop{ .load_l, .set_2_byte, .store_l }, // set 2,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_2_byte, .write_byte }, // set 2,(hl)
        &[_]Uop{ .load_a, .set_2_byte, .store_a }, // set 2,a
        &[_]Uop{ .load_b, .set_3_byte, .store_b }, // set 3,b
        &[_]Uop{ .load_c, .set_3_byte, .store_c }, // set 3,c
        &[_]Uop{ .load_d, .set_3_byte, .store_d }, // set 3,d
        &[_]Uop{ .load_e, .set_3_byte, .store_e }, // set 3,e
        &[_]Uop{ .load_h, .set_3_byte, .store_h }, // set 3,h
        &[_]Uop{ .load_l, .set_3_byte, .store_l }, // set 3,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_3_byte, .write_byte }, // set 3,(hl)
        &[_]Uop{ .load_a, .set_3_byte, .store_a }, // set 3,a
        &[_]Uop{ .load_b, .set_4_byte, .store_b }, // set 4,b
        &[_]Uop{ .load_c, .set_4_byte, .store_c }, // set 4,c
        &[_]Uop{ .load_d, .set_4_byte, .store_d }, // set 4,d
        &[_]Uop{ .load_e, .set_4_byte, .store_e }, // set 4,e
        &[_]Uop{ .load_h, .set_4_byte, .store_h }, // set 4,h
        &[_]Uop{ .load_l, .set_4_byte, .store_l }, // set 4,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_4_byte, .write_byte }, // set 4,(hl)
        &[_]Uop{ .load_a, .set_4_byte, .store_a }, // set 4,a
        &[_]Uop{ .load_b, .set_5_byte, .store_b }, // set 5,b
        &[_]Uop{ .load_c, .set_5_byte, .store_c }, // set 5,c
        &[_]Uop{ .load_d, .set_5_byte, .store_d }, // set 5,d
        &[_]Uop{ .load_e, .set_5_byte, .store_e }, // set 5,e
        &[_]Uop{ .load_h, .set_5_byte, .store_h }, // set 5,h
        &[_]Uop{ .load_l, .set_5_byte, .store_l }, // set 5,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_5_byte, .write_byte }, // set 5,(hl)
        &[_]Uop{ .load_a, .set_5_byte, .store_a }, // set 5,a
        &[_]Uop{ .load_b, .set_6_byte, .store_b }, // set 6,b
        &[_]Uop{ .load_c, .set_6_byte, .store_c }, // set 6,c
        &[_]Uop{ .load_d, .set_6_byte, .store_d }, // set 6,d
        &[_]Uop{ .load_e, .set_6_byte, .store_e }, // set 6,e
        &[_]Uop{ .load_h, .set_6_byte, .store_h }, // set 6,h
        &[_]Uop{ .load_l, .set_6_byte, .store_l }, // set 6,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_6_byte, .write_byte }, // set 6,(hl)
        &[_]Uop{ .load_a, .set_6_byte, .store_a }, // set 6,a
        &[_]Uop{ .load_b, .set_7_byte, .store_b }, // set 7,b
        &[_]Uop{ .load_c, .set_7_byte, .store_c }, // set 7,c
        &[_]Uop{ .load_d, .set_7_byte, .store_d }, // set 7,d
        &[_]Uop{ .load_e, .set_7_byte, .store_e }, // set 7,e
        &[_]Uop{ .load_h, .set_7_byte, .store_h }, // set 7,h
        &[_]Uop{ .load_l, .set_7_byte, .store_l }, // set 7,l
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_7_byte, .write_byte }, // set 7,(hl)
        &[_]Uop{ .load_a, .set_7_byte, .store_a }, // set 7,a
    };

    const dd = [_][]const Uop{
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_word, .store_bc }, // ld bc,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_bc, .save, .load_ix, .add_words, .store_ix }, // add ix,bc
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .load_bc, .write_word }, // ld (ix+d),bc
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_word, .store_de }, // ld de,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_de, .save, .load_ix, .add_words, .store_ix }, // add ix,de
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .load_de, .write_word }, // ld (ix+d),de
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_word, .mask_word_inst, .store_ix }, // ld ix,nn
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .load_ix, .write_word }, // ld (nn),ix
        &[_]Uop{ .load_ix, .inc_word, .mask_word_inst, .store_ix }, // inc ix
        &[_]Uop{ .load_ixh, .inc_byte, .store_ixh }, // inc ixh
        &[_]Uop{ .load_ixh, .dec_byte, .store_ixh }, // dec ixh
        &[_]Uop{ .fetch_byte, .store_ixh }, // ld ixh,n
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_word, .store_hl }, // ld hl,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_ix, .save, .add_words, .store_ix }, // add ix,ix
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_word, .store_ix }, // ld ix,(nn)
        &[_]Uop{ .load_ix, .dec_word, .mask_word_inst, .store_ix }, // dec ix
        &[_]Uop{ .load_ixl, .inc_byte, .store_ixl }, // inc ixl
        &[_]Uop{ .load_ixl, .dec_byte, .store_ixl }, // dec ixl
        &[_]Uop{ .fetch_byte, .store_ixl }, // ld ixl,n
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .load_hl, .write_word }, // ld (ix+d),hl
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_word, .store_iy }, // ld iy,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .inc_byte, .write_byte }, // inc (ix+d)
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .dec_byte, .write_byte }, // dec (ix+d)
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .fetch_byte, .write_byte }, // ld (ix+d),n
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_word, .store_ix }, // ld ix,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_sp, .load_ix, .add_words, .store_ix }, // add ix,sp
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .load_iy, .write_word }, // ld (ix+d),iy
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .load_ix, .write_word }, // ld (ix+d),ix
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_ixh, .store_b }, // ld b,ixh
        &[_]Uop{ .load_ixl, .store_b }, // ld b,ixl
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .store_b }, // ld b,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_ixh, .store_c }, // ld c,ixh
        &[_]Uop{ .load_ixl, .store_c }, // ld c,ixl
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .store_c }, // ld c,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_ixh, .store_d }, // ld d,ixh
        &[_]Uop{ .load_ixl, .store_d }, // ld d,ixl
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .store_d }, // ld d,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_ixh, .store_e }, // ld e,ixh
        &[_]Uop{ .load_ixl, .store_e }, // ld e,ixl
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .store_e }, // ld e,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_b, .store_ixh }, // ld ixh,b
        &[_]Uop{ .load_c, .store_ixh }, // ld ixh,c
        &[_]Uop{ .load_d, .store_ixh }, // ld ixh,d
        &[_]Uop{ .load_e, .store_ixh }, // ld ixh,e
        &[_]Uop{}, // ld ixh,ixh
        &[_]Uop{ .load_ixl, .store_ixh }, // ld ixh,ixl
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .store_h }, // ld h,(ix+d)
        &[_]Uop{ .load_a, .store_ixh }, // ld ixh,a
        &[_]Uop{ .load_b, .store_ixl }, // ld ixl,b
        &[_]Uop{ .load_c, .store_ixl }, // ld ixl,c
        &[_]Uop{ .load_d, .store_ixl }, // ld ixl,d
        &[_]Uop{ .load_e, .store_ixl }, // ld ixl,e
        &[_]Uop{ .load_ixh, .store_ixl }, // ld ixl,ixh
        &[_]Uop{}, // ld ixl,ixl
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .store_l }, // ld l,(ix+d)
        &[_]Uop{ .load_a, .store_ixl }, // ld ixl,a
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .load_b, .write_byte }, // ld (ix+d),b
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .load_c, .write_byte }, // ld (ix+d),c
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .load_d, .write_byte }, // ld (ix+d),d
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .load_e, .write_byte }, // ld (ix+d),e
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .load_h, .write_byte }, // ld (ix+d),h
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .load_l, .write_byte }, // ld (ix+d),l
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .load_a, .write_byte }, // ld (ix+d),a
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_ixh, .store_a }, // ld a,ixh
        &[_]Uop{ .load_ixl, .store_a }, // ld a,ixl
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .store_a }, // ld a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_ixh, .save, .load_a, .add_bytes, .store_a }, // add a,ixh
        &[_]Uop{ .load_ixl, .save, .load_a, .add_bytes, .store_a }, // add a,ixl
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .add_bytes, .store_a }, // add a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_ixh, .save, .load_a, .adc_bytes, .store_a }, // adc a,ixh
        &[_]Uop{ .load_ixl, .save, .load_a, .adc_bytes, .store_a }, // adc a,ixl
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .adc_bytes, .store_a }, // adc a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_ixh, .save, .load_a, .sub_bytes, .store_a }, // sub a,ixh
        &[_]Uop{ .load_ixl, .save, .load_a, .sub_bytes, .store_a }, // sub a,ixl
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .sub_bytes, .store_a }, // sub a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_ixh, .save, .load_a, .sbc_bytes, .store_a }, // sbc a,ixh
        &[_]Uop{ .load_ixl, .save, .load_a, .sbc_bytes, .store_a }, // sbc a,ixl
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .sbc_bytes, .store_a }, // sbc a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_ixh, .save, .load_a, .and_bytes, .pzs_byte, .store_a }, // and a,ixh
        &[_]Uop{ .load_ixl, .save, .load_a, .and_bytes, .pzs_byte, .store_a }, // and a,ixl
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .and_bytes, .pzs_byte, .store_a }, // and a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_ixh, .save, .load_a, .xor_bytes, .pzs_byte, .store_a }, // xor a,ixh
        &[_]Uop{ .load_ixl, .save, .load_a, .xor_bytes, .pzs_byte, .store_a }, // xor a,ixl
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .xor_bytes, .pzs_byte, .store_a }, // xor a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_ixh, .save, .load_a, .or_bytes, .pzs_byte, .store_a }, // or a,ixh
        &[_]Uop{ .load_ixl, .save, .load_a, .or_bytes, .pzs_byte, .store_a }, // or a,ixl
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .or_bytes, .pzs_byte, .store_a }, // or a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_ixh, .save, .load_a, .sub_bytes }, // cp a,ixh
        &[_]Uop{ .load_ixl, .save, .load_a, .sub_bytes }, // cp a,ixl
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .sub_bytes }, // cp a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .fetch_byte, .dispatch_ddcb }, // DDCB
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_sp, .mask_addr_inst, .read_word, .store_ix, .inc_addr, .mask_addr_inst, .store_sp }, // pop ix
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_sp, .mask_addr_inst, .read_word, .ex_ix_inst, .write_word_rev }, // ex (sp),ix
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_sp, .dec_addr, .mask_addr_inst, .load_ix, .write_word_rev, .store_sp }, // push ix
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .load_ix, .mask_word_inst, .store_pc, .set_adl_inst }, // jp (ix)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_ix, .save, .store_sp }, // ld sp,ix
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
    };

    const ddcb = [_][]const Uop{
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rlc_byte, .pzs_byte, .write_byte }, // rlc (ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rrc_byte, .pzs_byte, .write_byte }, // rrc (ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rl_byte, .pzs_byte, .write_byte }, // rl (ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rr_byte, .pzs_byte, .write_byte }, // rr (ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sla_byte, .pzs_byte, .write_byte }, // sla (ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sra_byte, .pzs_byte, .write_byte }, // sra (ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .srl_byte, .pzs_byte, .write_byte }, // srl (ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_0_byte, .pzs_byte }, // bit 0,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_1_byte, .pzs_byte }, // bit 1,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_2_byte, .pzs_byte }, // bit 2,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_3_byte, .pzs_byte }, // bit 3,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_4_byte, .pzs_byte }, // bit 4,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_5_byte, .pzs_byte }, // bit 5,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_6_byte, .pzs_byte }, // bit 6,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_7_byte, .pzs_byte }, // bit 7,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_0_byte, .write_byte }, // res 0,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_1_byte, .write_byte }, // res 1,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_2_byte, .write_byte }, // res 2,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_3_byte, .write_byte }, // res 3,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_4_byte, .write_byte }, // res 4,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_5_byte, .write_byte }, // res 5,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_6_byte, .write_byte }, // res 6,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_7_byte, .write_byte }, // res 7,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_0_byte, .write_byte }, // set 0,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_1_byte, .write_byte }, // set 1,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_2_byte, .write_byte }, // set 2,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_3_byte, .write_byte }, // set 3,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_4_byte, .write_byte }, // set 4,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_5_byte, .write_byte }, // set 5,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_6_byte, .write_byte }, // set 6,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_7_byte, .write_byte }, // set 7,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
    };

    const ed = [_][]const Uop{
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte, .store_b }, // in0 b,(n)
        &[_]Uop{ .fetch_byte, .save, .load_b, .out }, // out0 (n),b
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .mask_word_inst, .store_bc }, // lea bc,ix+d
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .mask_word_inst, .store_bc }, // lea bc,iy+d
        &[_]Uop{ .load_b, .save, .load_a, .and_bytes, .pzs_byte }, // tst a,b
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_word, .store_bc }, // ld bc,(hl)
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte, .store_c }, // in0 c,(n)
        &[_]Uop{ .fetch_byte, .save, .load_c, .out }, // out0 (n),c
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_c, .save, .load_a, .and_bytes, .pzs_byte }, // tst a,c
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .load_bc, .write_word }, // ld (hl),bc
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte, .store_d }, // in0 d,(n)
        &[_]Uop{ .fetch_byte, .save, .load_d, .out }, // out0 (n),d
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .mask_word_inst, .store_de }, // lea de,ix+d
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .mask_word_inst, .store_de }, // lea de,iy+d
        &[_]Uop{ .load_d, .save, .load_a, .and_bytes, .pzs_byte }, // tst a,d
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_word, .store_de }, // ld de,(hl)
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte, .store_e }, // in0 e,(n)
        &[_]Uop{ .fetch_byte, .save, .load_e, .out }, // out0 (n),e
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_e, .save, .load_a, .and_bytes, .pzs_byte }, // tst a,e
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .load_de, .write_word }, // ld (hl),de
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte, .store_h }, // in0 h,(n)
        &[_]Uop{ .fetch_byte, .save, .load_h, .out }, // out0 (n),h
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .mask_word_inst, .store_hl }, // lea hl,ix+d
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .mask_word_inst, .store_hl }, // lea hl,iy+d
        &[_]Uop{ .load_h, .save, .load_a, .and_bytes, .pzs_byte }, // tst a,h
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_word, .store_hl }, // ld hl,(hl)
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte, .store_l }, // in0 l,(n)
        &[_]Uop{ .fetch_byte, .save, .load_l, .out }, // out0 (n),l
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_l, .save, .load_a, .and_bytes, .pzs_byte }, // tst a,l
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .write_word }, // ld (hl),hl
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte }, // in0 (n)
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_word, .store_iy }, // ld iy,(hl)
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .mask_word_inst, .store_ix }, // lea ix,ix+d
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .mask_word_inst, .store_iy }, // lea iy,iy+d
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .save, .load_a, .and_bytes, .pzs_byte }, // tst a,(hl)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_word, .store_ix }, // ld ix,(hl)
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte, .store_a }, // in0 a,(n)
        &[_]Uop{ .fetch_byte, .save, .load_a, .out }, // out0 (n),a
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_a, .save, .and_bytes, .pzs_byte }, // tst a,a
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .load_iy, .write_word }, // ld (hl),iy
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .load_ix, .write_word }, // ld (hl),ix
        &[_]Uop{ .load_bc, .save, .in_f, .pzs_byte, .store_b }, // in b,(bc)
        &[_]Uop{ .load_bc, .save, .load_b, .out }, // out (bc),b
        &[_]Uop{ .load_bc, .save, .load_hl, .sbc_words, .store_hl }, // sbc hl,bc
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .load_bc, .write_word }, // ld (nn),bc
        &[_]Uop{ .load_a, .save, .set_00h, .sub_bytes, .store_a }, // neg
        &[_]Uop{ .flush, .ret, .store_pc, .add_cc_1, .copy_ief }, // retn
        &[_]Uop{.im_0}, // im 0
        &[_]Uop{ .load_a, .store_i }, // ld i,a
        &[_]Uop{ .load_bc, .save, .in_f, .pzs_byte, .store_c }, // in c,(bc)
        &[_]Uop{ .load_bc, .save, .load_c, .out }, // out (bc),c
        &[_]Uop{ .load_bc, .save, .load_hl, .adc_words, .store_hl }, // adc hl,bc
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_word, .store_bc }, // ld bc,(nn)
        &[_]Uop{ .load_bc, .add_cc_4, .mlt_bytes, .store_bc }, // mlt bc
        &[_]Uop{ .flush, .ret, .store_pc, .add_cc_1, .copy_ief }, // reti
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_a, .store_r }, // ld r,a
        &[_]Uop{ .load_bc, .save, .in_f, .pzs_byte, .store_d }, // in d,(bc)
        &[_]Uop{ .load_bc, .save, .load_d, .out }, // out (bc),d
        &[_]Uop{ .load_de, .save, .load_hl, .sbc_words, .store_hl }, // sbc hl,de
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .load_de, .write_word }, // ld (nn),de
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .mask_word_inst, .store_ix }, // lea ix,iy+d
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .mask_word_inst, .store_iy }, // lea iy,ix+d
        &[_]Uop{.im_1}, // im 1
        &[_]Uop{ .load_i, .store_a }, // ld a,i
        &[_]Uop{ .load_bc, .save, .in_f, .pzs_byte, .store_e }, // in e,(bc)
        &[_]Uop{ .load_bc, .save, .load_e, .out }, // out (bc),e
        &[_]Uop{ .load_de, .save, .load_hl, .adc_words, .store_hl }, // adc hl,de
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_word, .store_de }, // ld de,(nn)
        &[_]Uop{ .load_de, .add_cc_4, .mlt_bytes, .store_de }, // mlt de
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{.im_2}, // im 2
        &[_]Uop{ .load_r, .store_a }, // ld a,r
        &[_]Uop{ .load_bc, .save, .in_f, .pzs_byte, .store_h }, // in h,(bc)
        &[_]Uop{ .load_bc, .save, .load_h, .out }, // out (bc),h
        &[_]Uop{ .load_hl, .save, .sbc_words, .store_hl }, // sbc hl,hl
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .load_hl, .write_word }, // ld (nn),hl
        &[_]Uop{ .fetch_byte, .save, .load_a, .and_bytes, .pzs_byte }, // tst a,n
        &[_]Uop{ .fetch_byte, .save, .load_ix, .add_offset, .load_sp, .dec_addr, .mask_addr_inst, .write_word_rev, .store_sp }, // pea ix+d
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .load_sp, .dec_addr, .mask_addr_inst, .write_word_rev, .store_sp }, // pea iy+d
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .load_a_high, .add_cc_1, .rrd_bytes, .store_a_high, .write_byte }, // rrd
        &[_]Uop{ .load_bc, .save, .in_f, .pzs_byte, .store_l }, // in l,(bc)
        &[_]Uop{ .load_bc, .save, .load_l, .out }, // out (bc),l
        &[_]Uop{ .load_hl, .save, .adc_words, .store_hl }, // adc hl,hl
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_word, .store_hl }, // ld hl,(nn)
        &[_]Uop{ .load_hl, .add_cc_4, .mlt_bytes, .store_hl }, // mlt hl
        &[_]Uop{ .adl, .load_a, .store_mb }, // ld mb,a
        &[_]Uop{ .load_mb, .store_a }, // ld a,mb
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .load_a_high, .add_cc_1, .rld_bytes, .store_a_high, .write_byte }, // rld
        &[_]Uop{ .load_bc, .save, .in_f, .pzs_byte }, // in (bc)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_sp, .load_hl, .sbc_words, .store_hl }, // sbc hl,sp
        &[_]Uop{ .fetch_word, .load_sp, .swap, .mask_addr_inst, .write_word }, // ld (nn),sp
        &[_]Uop{ .load_c, .save, .in, .save, .fetch_byte, .swap, .and_bytes, .pzs_byte }, // tstio n
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{.halt}, // slp
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_bc, .save, .in_f, .pzs_byte, .store_a }, // in a,(bc)
        &[_]Uop{ .load_bc, .save, .load_a, .out }, // out (bc),a
        &[_]Uop{ .load_sp, .load_hl, .adc_words, .store_hl }, // adc hl,sp
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_word, .save, .store_sp }, // ld sp,(nn)
        &[_]Uop{ .load_sp, .restore, .add_cc_4, .mlt_bytes, .save, .store_sp }, // mlt sp
        &[_]Uop{.set_madl}, // stmix
        &[_]Uop{.clear_madl}, // rsmix
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_c, .save, .in, .save, .load_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .store_hl, .load_c, .add_byte_1, .store_c, .load_b, .dec_byte_hzs, .store_b }, // inim
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .store_hl, .load_c, .swap, .add_cc_1, .out, .nf_byte_sign, .load_c, .add_byte_1, .store_c, .load_b, .dec_byte_hzs, .store_b }, // otim
        &[_]Uop{ .load_bc, .save, .in, .save, .load_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .store_hl, .load_c, .add_byte_1, .store_c, .load_b, .sub_byte_1, .zf_byte, .store_b }, // ini2
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_c, .save, .in, .save, .load_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .store_hl, .load_c, .sub_byte_1, .store_c, .load_b, .dec_byte_hzs, .store_b }, // indm
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .store_hl, .load_c, .swap, .add_cc_1, .out, .nf_byte_sign, .load_c, .sub_byte_1, .store_c, .load_b, .dec_byte_hzs, .store_b }, // otdm
        &[_]Uop{ .load_bc, .save, .in, .save, .load_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .store_hl, .load_c, .sub_byte_1, .store_c, .load_b, .sub_byte_1, .zf_byte, .store_b }, // ind2
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .sub_r_1, .load_c, .save, .in, .save, .load_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .store_hl, .load_c, .add_byte_1, .store_c, .load_b, .sub_byte_1, .zf_byte, .store_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // inimr
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .store_hl, .load_c, .swap, .add_cc_1, .out, .nf_byte_sign, .load_c, .add_byte_1, .store_c, .load_b, .dec_byte_hzs, .store_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // otimr
        &[_]Uop{ .load_de, .save, .in, .inc_addr, .swap, .mask_word_inst, .store_de, .load_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .store_hl, .load_bc, .dec_word, .mask_word_inst, .zf_word, .store_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // ini2r
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .sub_r_1, .load_c, .save, .in, .save, .load_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .store_hl, .load_c, .sub_byte_1, .store_c, .load_b, .sub_byte_1, .zf_byte, .store_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // indmr
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .store_hl, .load_c, .swap, .add_cc_1, .out, .nf_byte_sign, .load_c, .sub_byte_1, .store_c, .load_b, .dec_byte_hzs, .store_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // otdmr
        &[_]Uop{ .load_de, .save, .in, .dec_addr, .swap, .mask_word_inst, .store_de, .load_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .store_hl, .load_bc, .dec_word, .mask_word_inst, .zf_word, .store_bc_inst, .nz, .add_r_1, .repeat, .sub_r_1, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // ind2r
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .store_hl, .load_de, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .inc_addr, .restore, .mask_word_inst, .store_de, .ld_flags, .load_bc, .dec_word, .mask_word_inst, .repeat_flag, .store_bc_inst }, // ldi
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .store_hl, .load_a, .cp_flags, .load_bc, .dec_word, .mask_word_inst, .repeat_flag, .store_bc_inst }, // cpi
        &[_]Uop{ .load_bc, .save, .in, .save, .load_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .store_hl, .load_b, .sub_byte_1, .zf_byte, .store_b }, // ini
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .store_hl, .load_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .load_b, .sub_byte_1, .zf_byte, .store_b }, // outi
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .store_hl, .load_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .load_c, .add_byte_1, .store_c, .load_b, .sub_byte_1, .zf_byte, .store_b }, // outi2
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .store_hl, .load_de, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .dec_addr, .restore, .mask_word_inst, .store_de, .ld_flags, .load_bc, .dec_word, .mask_word_inst, .repeat_flag, .store_bc_inst }, // ldd
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .store_hl, .load_a, .cp_flags, .load_bc, .dec_word, .mask_word_inst, .repeat_flag, .store_bc_inst }, // cpd
        &[_]Uop{ .load_bc, .save, .in, .save, .load_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .store_hl, .load_b, .sub_byte_1, .zf_byte, .store_b }, // ind
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .store_hl, .load_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .load_b, .sub_byte_1, .zf_byte, .store_b }, // outd
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .store_hl, .load_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .load_c, .sub_byte_1, .store_c, .load_b, .sub_byte_1, .store_b, .zf_byte }, // outd2
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .store_hl, .load_de, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .inc_addr, .restore, .mask_word_inst, .store_de, .ld_flags, .load_bc, .dec_word, .mask_word_inst, .repeat_flag, .store_bc_inst, .pe, .add_r_2, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // ldir
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .store_hl, .load_a, .cp_flags, .load_bc, .dec_word, .mask_word_inst, .repeat_flag, .store_bc_inst, .add_cc_1, .nz, .pe, .add_r_2, .add_cc_1, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // cpir
        &[_]Uop{ .load_bc, .save, .in, .save, .load_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .store_hl, .load_b, .sub_byte_1, .zf_byte, .store_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // inir
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .store_hl, .load_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .load_b, .sub_byte_1, .zf_byte, .store_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // otir
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .store_hl, .load_de, .swap, .add_cc_1, .out, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .store_de, .load_bc, .dec_word, .mask_word_inst, .zf_word, .store_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // oti2r
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .store_hl, .load_de, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .dec_addr, .restore, .mask_word_inst, .store_de, .ld_flags, .load_bc, .dec_word, .mask_word_inst, .repeat_flag, .store_bc_inst, .pe, .add_r_2, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // lddr
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .store_hl, .load_a, .cp_flags, .load_bc, .dec_word, .mask_word_inst, .repeat_flag, .store_bc_inst, .add_cc_1, .nz, .pe, .add_r_2, .add_cc_1, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // cpdr
        &[_]Uop{ .load_bc, .save, .in, .save, .load_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .store_hl, .load_b, .sub_byte_1, .zf_byte, .store_b, .nz, .add_r_1, .repeat, .sub_r_1, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // indr
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .store_hl, .load_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .load_b, .sub_byte_1, .zf_byte, .store_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // otdr
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .store_hl, .load_de, .swap, .add_cc_1, .out, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .store_de, .load_bc, .dec_word, .mask_word_inst, .zf_word, .store_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // otd2r
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_de, .save, .in, .save, .load_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .store_hl, .load_bc, .dec_word, .mask_word_inst, .zf_word, .store_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // inirx
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .store_hl, .load_de, .swap, .add_cc_1, .out, .nf_byte_sign, .load_bc, .dec_word, .mask_word_inst, .zf_word, .store_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // otirx
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_hl, .store_i }, // ld i,hl
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_de, .save, .in, .save, .load_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .store_hl, .load_bc, .dec_word, .mask_word_inst, .zf_word, .store_bc_inst, .nz, .add_r_1, .repeat, .sub_r_1, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // indrx
        &[_]Uop{ .load_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .store_hl, .load_de, .swap, .add_cc_1, .out, .nf_byte_sign, .load_bc, .dec_word, .mask_word_inst, .zf_word, .store_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .load_pc, .sub_word_3, .sub_word_suffix, .store_pc }, // otdrx
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_mbi, .mask_word_inst, .store_hl }, // ld hl,i
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
    };

    const fd = [_][]const Uop{
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_word, .store_bc }, // ld bc,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_bc, .save, .load_iy, .add_words, .store_iy }, // add iy,bc
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .load_bc, .write_word }, // ld (iy+d),bc
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_word, .store_de }, // ld de,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_de, .save, .load_iy, .add_words, .store_iy }, // add iy,de
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .load_de, .write_word }, // ld (iy+d),de
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_word, .mask_word_inst, .store_iy }, // ld iy,nn
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .load_iy, .write_word }, // ld (nn),iy
        &[_]Uop{ .load_iy, .inc_word, .mask_word_inst, .store_iy }, // inc iy
        &[_]Uop{ .load_iyh, .inc_byte, .store_iyh }, // inc iyh
        &[_]Uop{ .load_iyh, .dec_byte, .store_iyh }, // dec iyh
        &[_]Uop{ .fetch_byte, .store_iyh }, // ld iyh,n
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_word, .store_hl }, // ld hl,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_iy, .save, .add_words, .store_iy }, // add iy,iy
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_word, .store_iy }, // ld iy,(nn)
        &[_]Uop{ .load_iy, .dec_word, .mask_word_inst, .store_iy }, // dec iy
        &[_]Uop{ .load_iyl, .inc_byte, .store_iyl }, // inc iyl
        &[_]Uop{ .load_iyl, .dec_byte, .store_iyl }, // dec iyl
        &[_]Uop{ .fetch_byte, .store_iyl }, // ld iyl,n
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .load_hl, .write_word }, // ld (iy+d),hl
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_word, .store_ix }, // ld ix,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .inc_byte, .write_byte }, // inc (iy+d)
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .dec_byte, .write_byte }, // dec (iy+d)
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .fetch_byte, .write_byte }, // ld (iy+d),n
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_word, .store_iy }, // ld iy,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_sp, .load_iy, .add_words, .store_iy }, // add iy,sp
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .load_ix, .write_word }, // ld (iy+d),ix
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .load_iy, .write_word }, // ld (iy+d),iy
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_iyh, .store_b }, // ld b,iyh
        &[_]Uop{ .load_iyl, .store_b }, // ld b,iyl
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .store_b }, // ld b,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_iyh, .store_c }, // ld c,iyh
        &[_]Uop{ .load_iyl, .store_c }, // ld c,iyl
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .store_c }, // ld c,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_iyh, .store_d }, // ld d,iyh
        &[_]Uop{ .load_iyl, .store_d }, // ld d,iyl
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .store_d }, // ld d,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_iyh, .store_e }, // ld e,iyh
        &[_]Uop{ .load_iyl, .store_e }, // ld e,iyl
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .store_e }, // ld e,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_b, .store_iyh }, // ld iyh,b
        &[_]Uop{ .load_c, .store_iyh }, // ld iyh,c
        &[_]Uop{ .load_d, .store_iyh }, // ld iyh,d
        &[_]Uop{ .load_e, .store_iyh }, // ld iyh,e
        &[_]Uop{}, // ld iyh,iyh
        &[_]Uop{ .load_iyl, .store_iyh }, // ld iyh,iyl
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .store_h }, // ld h,(iy+d)
        &[_]Uop{ .load_a, .store_iyh }, // ld iyh,a
        &[_]Uop{ .load_b, .store_iyl }, // ld iyl,b
        &[_]Uop{ .load_c, .store_iyl }, // ld iyl,c
        &[_]Uop{ .load_d, .store_iyl }, // ld iyl,d
        &[_]Uop{ .load_e, .store_iyl }, // ld iyl,e
        &[_]Uop{ .load_iyh, .store_iyl }, // ld iyl,iyh
        &[_]Uop{}, // ld iyl,iyl
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .store_l }, // ld l,(iy+d)
        &[_]Uop{ .load_a, .store_iyl }, // ld iyl,a
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .load_b, .write_byte }, // ld (iy+d),b
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .load_c, .write_byte }, // ld (iy+d),c
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .load_d, .write_byte }, // ld (iy+d),d
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .load_e, .write_byte }, // ld (iy+d),e
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .load_h, .write_byte }, // ld (iy+d),h
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .load_l, .write_byte }, // ld (iy+d),l
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .load_a, .write_byte }, // ld (iy+d),a
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_iyh, .store_a }, // ld a,iyh
        &[_]Uop{ .load_iyl, .store_a }, // ld a,iyl
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .store_a }, // ld a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_iyh, .save, .load_a, .add_bytes, .store_a }, // add a,iyh
        &[_]Uop{ .load_iyl, .save, .load_a, .add_bytes, .store_a }, // add a,iyl
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .add_bytes, .store_a }, // add a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_iyh, .save, .load_a, .adc_bytes, .store_a }, // adc a,iyh
        &[_]Uop{ .load_iyl, .save, .load_a, .adc_bytes, .store_a }, // adc a,iyl
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .adc_bytes, .store_a }, // adc a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_iyh, .save, .load_a, .sub_bytes, .store_a }, // sub a,iyh
        &[_]Uop{ .load_iyl, .save, .load_a, .sub_bytes, .store_a }, // sub a,iyl
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .sub_bytes, .store_a }, // sub a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_iyh, .save, .load_a, .sbc_bytes, .store_a }, // sbc a,iyh
        &[_]Uop{ .load_iyl, .save, .load_a, .sbc_bytes, .store_a }, // sbc a,iyl
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .sbc_bytes, .store_a }, // sbc a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_iyh, .save, .load_a, .and_bytes, .pzs_byte, .store_a }, // and a,iyh
        &[_]Uop{ .load_iyl, .save, .load_a, .and_bytes, .pzs_byte, .store_a }, // and a,iyl
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .and_bytes, .pzs_byte, .store_a }, // and a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_iyh, .save, .load_a, .xor_bytes, .pzs_byte, .store_a }, // xor a,iyh
        &[_]Uop{ .load_iyl, .save, .load_a, .xor_bytes, .pzs_byte, .store_a }, // xor a,iyl
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .xor_bytes, .pzs_byte, .store_a }, // xor a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_iyh, .save, .load_a, .or_bytes, .pzs_byte, .store_a }, // or a,iyh
        &[_]Uop{ .load_iyl, .save, .load_a, .or_bytes, .pzs_byte, .store_a }, // or a,iyl
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .or_bytes, .pzs_byte, .store_a }, // or a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_iyh, .save, .load_a, .sub_bytes }, // cp a,iyh
        &[_]Uop{ .load_iyl, .save, .load_a, .sub_bytes }, // cp a,iyl
        &[_]Uop{ .fetch_byte, .save, .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .load_a, .sub_bytes }, // cp a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .fetch_byte, .dispatch_fdcb }, // FDCB
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_sp, .mask_addr_inst, .read_word, .store_iy, .inc_addr, .mask_addr_inst, .store_sp }, // pop iy
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_sp, .mask_addr_inst, .read_word, .ex_iy_inst, .write_word_rev }, // ex (sp),iy
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_sp, .dec_addr, .mask_addr_inst, .load_iy, .write_word_rev, .store_sp }, // push iy
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .load_iy, .mask_word_inst, .store_pc, .set_adl_inst }, // jp (iy)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .load_iy, .save, .store_sp }, // ld sp,iy
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
    };

    const fdcb = [_][]const Uop{
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rlc_byte, .pzs_byte, .write_byte }, // rlc (iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rrc_byte, .pzs_byte, .write_byte }, // rrc (iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rl_byte, .pzs_byte, .write_byte }, // rl (iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rr_byte, .pzs_byte, .write_byte }, // rr (iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sla_byte, .pzs_byte, .write_byte }, // sla (iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sra_byte, .pzs_byte, .write_byte }, // sra (iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .srl_byte, .pzs_byte, .write_byte }, // srl (iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_0_byte, .pzs_byte }, // bit 0,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_1_byte, .pzs_byte }, // bit 1,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_2_byte, .pzs_byte }, // bit 2,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_3_byte, .pzs_byte }, // bit 3,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_4_byte, .pzs_byte }, // bit 4,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_5_byte, .pzs_byte }, // bit 5,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_6_byte, .pzs_byte }, // bit 6,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_7_byte, .pzs_byte }, // bit 7,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_0_byte, .write_byte }, // res 0,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_1_byte, .write_byte }, // res 1,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_2_byte, .write_byte }, // res 2,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_3_byte, .write_byte }, // res 3,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_4_byte, .write_byte }, // res 4,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_5_byte, .write_byte }, // res 5,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_6_byte, .write_byte }, // res 6,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_7_byte, .write_byte }, // res 7,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_0_byte, .write_byte }, // set 0,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_1_byte, .write_byte }, // set 1,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_2_byte, .write_byte }, // set 2,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_3_byte, .write_byte }, // set 3,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_4_byte, .write_byte }, // set 4,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_5_byte, .write_byte }, // set 5,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_6_byte, .write_byte }, // set 6,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .load_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_7_byte, .write_byte }, // set 7,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
    };
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
    repeat: while (true) {
        inline for (uops) |uop| dispatch(impl, uop) catch |err| switch (err) {
            Error.RepeatInstruction => continue :repeat,
            else => return err,
        };
        return;
    }
}

fn dispatch(impl: anytype, comptime uop: Uop) anyerror!void {
    try @as(anyerror!void, switch (uop) {
        .reset => impl.reset(),

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

        .sub_r_2 => impl.addR(-2),
        .sub_r_1 => impl.addR(-1),
        .add_r_1 => impl.addR(1),
        .add_r_2 => impl.addR(2),
        .add_r_inst => impl.addRInstruction(),
        .add_cc_1 => impl.addCycles(1),
        .add_cc_4 => impl.addCycles(4),
        .add_cc_call => impl.addCycleCall(),

        .dispatch_base => impl.dispatch(dispatcherFor(&ez80.base)),
        .dispatch_cb => impl.dispatch(dispatcherFor(&ez80.cb)),
        .dispatch_dd => impl.dispatch(dispatcherFor(&ez80.dd)),
        .dispatch_ddcb => impl.dispatch(dispatcherFor(&ez80.ddcb)),
        .dispatch_ed => impl.dispatch(dispatcherFor(&ez80.ed)),
        .dispatch_fd => impl.dispatch(dispatcherFor(&ez80.fd)),
        .dispatch_fdcb => impl.dispatch(dispatcherFor(&ez80.fdcb)),

        .mode_sis => impl.setMode(.{ .suffix = true, .inst = .s, .imm = .is }),
        .mode_lis => impl.setMode(.{ .suffix = true, .inst = .l, .imm = .is }),
        .mode_sil => impl.setMode(.{ .suffix = true, .inst = .s, .imm = .il }),
        .mode_lil => impl.setMode(.{ .suffix = true, .inst = .l, .imm = .il }),
        .set_adl_inst => impl.setAdlInstruction(),
        .set_adl_imm => impl.setAdlImmediate(),
        .clear_madl, .set_madl => err: {
            try impl.set(@boolToInt(uop == .set_madl));
            break :err impl.storeShadowRegister(.adl);
        },
        .clear_ief, .set_ief => err: {
            try impl.set(@boolToInt(uop == .set_ief));
            try impl.storeRegister(.ief);
            break :err impl.storeShadowRegister(.ief);
        },
        .im_0 => impl.setInterruptMode(0),
        .im_1 => impl.setInterruptMode(1),
        .im_2 => impl.setInterruptMode(2),

        .halt => impl.halt(),

        .non_zero => impl.checkNonZero(),
        .adl => impl.checkAdl(),
        .nz => impl.checkCondition(.zf, false),
        .z => impl.checkCondition(.zf, true),
        .nc => impl.checkCondition(.cf, false),
        .c => impl.checkCondition(.cf, true),
        .po => impl.checkCondition(.pv, false),
        .pe => impl.checkCondition(.pv, true),
        .p => impl.checkCondition(.sf, false),
        .m => impl.checkCondition(.sf, true),

        .load_b => impl.loadRegister(.b),
        .load_c => impl.loadRegister(.c),
        .load_d => impl.loadRegister(.d),
        .load_e => impl.loadRegister(.e),
        .load_h => impl.loadRegister(.h),
        .load_l => impl.loadRegister(.l),
        .load_a => impl.loadRegister(.a),
        .load_a_high => impl.loadRegisterHigh(.a),
        .load_ixh => impl.loadRegister(.ixh),
        .load_ixl => impl.loadRegister(.ixl),
        .load_iyh => impl.loadRegister(.iyh),
        .load_iyl => impl.loadRegister(.iyl),
        .load_i => err: {
            try impl.loadRegister(.i);
            break :err impl.loadRegisterFlags();
        },
        .load_mbi => err: {
            try impl.loadRegister(.mbi);
            break :err impl.zeroFlags();
        },
        .load_r => err: {
            try impl.loadRegister(.r);
            break :err impl.loadRegisterFlags();
        },
        .load_mb => impl.loadRegister(.mb),
        .load_af => impl.loadRegister(.af),
        .load_bc => impl.loadRegister(.ubc),
        .load_de => impl.loadRegister(.ude),
        .load_hl => impl.loadRegister(.uhl),
        .load_ix => impl.loadRegister(.uix),
        .load_iy => impl.loadRegister(.uiy),
        .load_sp => impl.loadStackPointerInstruction(),
        .load_pc => impl.loadShadowRegister(.pc),

        .store_b => impl.storeRegister(.b),
        .store_c => impl.storeRegister(.c),
        .store_d => impl.storeRegister(.d),
        .store_e => impl.storeRegister(.e),
        .store_h => impl.storeRegister(.h),
        .store_l => impl.storeRegister(.l),
        .store_a => impl.storeRegister(.a),
        .store_a_high => impl.storeRegisterHigh(.a),
        .store_ixh => impl.storeRegister(.ixh),
        .store_ixl => impl.storeRegister(.ixl),
        .store_iyh => impl.storeRegister(.iyh),
        .store_iyl => impl.storeRegister(.iyl),
        .store_i => err: {
            try impl.truncate(u16);
            break :err impl.storeRegister(.ui);
        },
        .store_r => impl.storeRegister(.r),
        .store_mb => impl.storeRegister(.mb),
        .store_af => err: {
            try impl.truncate(u16);
            break :err impl.storeRegister(.af);
        },
        .store_bc => impl.storeRegister(.ubc),
        .store_bc_inst => impl.storeRegisterInstruction(.bc, .ubc),
        .store_de => impl.storeRegister(.ude),
        .store_hl => impl.storeRegister(.uhl),
        .store_ix => impl.storeRegister(.uix),
        .store_iy => impl.storeRegister(.uiy),
        .store_sp => impl.storeStackPointerInstruction(),
        .store_pc => impl.storeShadowRegister(.pc),

        .ex_hl => impl.exchangeRegister(.uhl),
        .ex_hl_inst => impl.exchangeRegisterInstruction(.hl, .uhl),
        .ex_ix_inst => impl.exchangeRegisterInstruction(.ix, .uix),
        .ex_iy_inst => impl.exchangeRegisterInstruction(.iy, .uiy),
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

        .in => impl.in(),
        .in_f => err: {
            try impl.in();
            break :err impl.inFlags();
        },
        .read_byte => impl.readMemoryByte(),
        .read_word => impl.readMemoryWord(),

        .out => impl.out(),
        .write_byte => impl.writeMemoryByte(),
        .write_word => impl.writeMemoryWord(.forward),
        .write_word_rev => impl.writeMemoryWord(.reverse),

        .interrupt => impl.interrupt(),
        .rst => impl.rst(),
        .call => impl.call(),
        .ret => impl.ret(),
        .copy_ief => impl.copyIef(),

        .rlc_byte => impl.rlcByte(),
        .rrc_byte => impl.rrcByte(),
        .rl_byte => impl.rlByte(),
        .rr_byte => impl.rrByte(),
        .sla_byte => impl.slaByte(),
        .sra_byte => impl.sraByte(),
        .srl_byte => impl.srlByte(),
        .daa_byte => impl.daaByte(),
        .cpl_byte => impl.cplByte(),
        .bit_0_byte => impl.bitByte(0),
        .bit_1_byte => impl.bitByte(1),
        .bit_2_byte => impl.bitByte(2),
        .bit_3_byte => impl.bitByte(3),
        .bit_4_byte => impl.bitByte(4),
        .bit_5_byte => impl.bitByte(5),
        .bit_6_byte => impl.bitByte(6),
        .bit_7_byte => impl.bitByte(7),
        .res_0_byte => impl.setByteBit(0, 0),
        .res_1_byte => impl.setByteBit(1, 0),
        .res_2_byte => impl.setByteBit(2, 0),
        .res_3_byte => impl.setByteBit(3, 0),
        .res_4_byte => impl.setByteBit(4, 0),
        .res_5_byte => impl.setByteBit(5, 0),
        .res_6_byte => impl.setByteBit(6, 0),
        .res_7_byte => impl.setByteBit(7, 0),
        .set_0_byte => impl.setByteBit(0, 1),
        .set_1_byte => impl.setByteBit(1, 1),
        .set_2_byte => impl.setByteBit(2, 1),
        .set_3_byte => impl.setByteBit(3, 1),
        .set_4_byte => impl.setByteBit(4, 1),
        .set_5_byte => impl.setByteBit(5, 1),
        .set_6_byte => impl.setByteBit(6, 1),
        .set_7_byte => impl.setByteBit(7, 1),
        .nf_byte_sign => impl.setSubtractByteSign(),
        .pzs_byte => err: {
            try impl.setParityByte();
            try impl.setZeroByte();
            break :err impl.setSignByte();
        },
        .zf_byte => impl.setZeroByte(),
        .zf_word => impl.setZeroWord(),
        .zs_byte => err: {
            try impl.setZeroByte();
            break :err impl.setSignByte();
        },
        .scf => impl.scf(),
        .ccf => impl.ccf(),
        .inc_byte => impl.addByte(1),
        .dec_byte => impl.addByte(-1),
        .dec_byte_hzs => impl.addByteHalfZeroSign(-1),
        .add_byte_1 => impl.offsetByte(1),
        .sub_byte_1 => impl.offsetByte(-1),
        .inc_word => impl.addWord(1),
        .dec_word => impl.addWord(-1),
        .sub_word_2 => impl.addWord(-2),
        .sub_word_3 => impl.addWord(-3),
        .sub_word_suffix => impl.subWordSuffix(),
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
        .mlt_bytes => impl.mltBytes(),
        .rrd_bytes => impl.rrdBytes(),
        .rld_bytes => impl.rldBytes(),

        .add_words => impl.addWords(),
        .adc_words => impl.adcWords(),
        .sbc_words => impl.sbcWords(),

        .ld_flags => impl.ldFlags(),
        .cp_flags => impl.cpFlags(),
        .repeat_flag => impl.repeatFlag(),
        .repeat => impl.repeat(),
    });
}
