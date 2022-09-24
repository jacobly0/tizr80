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

    get_b,
    get_c,
    get_d,
    get_e,
    get_h,
    get_l,
    get_a,
    get_a_high,
    get_ixh,
    get_ixl,
    get_iyh,
    get_iyl,
    get_i,
    get_mbi,
    get_r,
    get_mb,
    get_af,
    get_bc,
    get_de,
    get_hl,
    get_ix,
    get_iy,
    get_sp,
    get_pc,

    set_b,
    set_c,
    set_d,
    set_e,
    set_h,
    set_l,
    set_a,
    set_a_high,
    set_ixh,
    set_ixl,
    set_iyh,
    set_iyl,
    set_i,
    set_r,
    set_mb,
    set_af,
    set_bc,
    set_bc_inst,
    set_de,
    set_hl,
    set_ix,
    set_iy,
    set_sp,
    set_pc,

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
        &[_]Uop{ .fetch_word, .mask_word_inst, .set_bc }, // ld bc,nn
        &[_]Uop{ .get_bc, .save, .mask_addr_inst, .get_a, .write_byte }, // ld (bc),a
        &[_]Uop{ .get_bc, .inc_word, .mask_word_inst, .set_bc }, // inc bc
        &[_]Uop{ .get_b, .inc_byte, .set_b }, // inc b
        &[_]Uop{ .get_b, .dec_byte, .set_b }, // dec b
        &[_]Uop{ .fetch_byte, .set_b }, // ld b,n
        &[_]Uop{ .get_a, .rlc_byte, .set_a }, // rlca
        &[_]Uop{ .get_af, .@"ex_af'", .set_af }, // ex af,af'
        &[_]Uop{ .get_bc, .save, .get_hl, .add_words, .set_hl }, // add hl,bc
        &[_]Uop{ .get_bc, .save, .mask_addr_inst, .read_byte, .set_a }, // ld a,(bc)
        &[_]Uop{ .get_bc, .dec_word, .mask_word_inst, .set_bc }, // dec bc
        &[_]Uop{ .get_c, .inc_byte, .set_c }, // inc c
        &[_]Uop{ .get_c, .dec_byte, .set_c }, // dec c
        &[_]Uop{ .fetch_byte, .set_c }, // ld c,n
        &[_]Uop{ .get_a, .rrc_byte, .set_a }, // rrca
        &[_]Uop{ .fetch_byte, .save, .get_b, .sub_byte_1, .set_b, .non_zero, .flush, .add_cc_1, .get_pc, .dec_word, .add_offset, .mask_word_inst, .set_pc }, // djnz d
        &[_]Uop{ .fetch_word, .mask_word_inst, .set_de }, // ld de,nn
        &[_]Uop{ .get_de, .save, .mask_addr_inst, .get_a, .write_byte }, // ld (de),a
        &[_]Uop{ .get_de, .inc_word, .mask_word_inst, .set_de }, // inc de
        &[_]Uop{ .get_d, .inc_byte, .set_d }, // inc d
        &[_]Uop{ .get_d, .dec_byte, .set_d }, // dec d
        &[_]Uop{ .fetch_byte, .set_d }, // ld d,n
        &[_]Uop{ .get_a, .rl_byte, .set_a }, // rla
        &[_]Uop{ .fetch_byte, .save, .flush, .get_pc, .dec_word, .add_offset, .mask_word_inst, .set_pc }, // jr d
        &[_]Uop{ .get_de, .save, .get_hl, .add_words, .set_hl }, // add hl,de
        &[_]Uop{ .get_de, .save, .mask_addr_inst, .read_byte, .set_a }, // ld a,(de)
        &[_]Uop{ .get_de, .dec_word, .mask_word_inst, .set_de }, // dec de
        &[_]Uop{ .get_e, .inc_byte, .set_e }, // inc e
        &[_]Uop{ .get_e, .dec_byte, .set_e }, // dec e
        &[_]Uop{ .fetch_byte, .set_e }, // ld e,n
        &[_]Uop{ .get_a, .rr_byte, .set_a }, // rra
        &[_]Uop{ .fetch_byte, .nz, .flush, .add_cc_1, .save, .get_pc, .dec_word, .add_offset, .mask_word_inst, .set_pc }, // jr nz,d
        &[_]Uop{ .fetch_word, .mask_word_inst, .set_hl }, // ld hl,nn
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .get_hl, .write_word }, // ld (nn),hl
        &[_]Uop{ .get_hl, .inc_word, .mask_word_inst, .set_hl }, // inc hl
        &[_]Uop{ .get_h, .inc_byte, .set_h }, // inc h
        &[_]Uop{ .get_h, .dec_byte, .set_h }, // dec h
        &[_]Uop{ .fetch_byte, .set_h }, // ld h,n
        &[_]Uop{ .get_a, .daa_byte, .pzs_byte, .set_a }, // daa
        &[_]Uop{ .fetch_byte, .z, .flush, .add_cc_1, .save, .get_pc, .dec_word, .add_offset, .mask_word_inst, .set_pc }, // jr z,d
        &[_]Uop{ .get_hl, .save, .add_words, .set_hl }, // add hl,hl
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_word, .set_hl }, // ld hl,(nn)
        &[_]Uop{ .get_hl, .dec_word, .mask_word_inst, .set_hl }, // dec hl
        &[_]Uop{ .get_l, .inc_byte, .set_l }, // inc l
        &[_]Uop{ .get_l, .dec_byte, .set_l }, // dec l
        &[_]Uop{ .fetch_byte, .set_l }, // ld l,n
        &[_]Uop{ .get_a, .cpl_byte, .set_a }, // cpl
        &[_]Uop{ .fetch_byte, .nc, .flush, .add_cc_1, .save, .get_pc, .dec_word, .add_offset, .mask_word_inst, .set_pc }, // jr nc,d
        &[_]Uop{ .fetch_word, .save, .set_sp }, // ld sp,nn
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .get_a, .write_byte }, // ld (nn),a
        &[_]Uop{ .get_sp, .inc_addr, .set_sp }, // inc sp
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .inc_byte, .write_byte }, // inc (hl)
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .dec_byte, .write_byte }, // dec (hl)
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .fetch_byte, .write_byte }, // ld (hl),n
        &[_]Uop{.scf}, // scf
        &[_]Uop{ .fetch_byte, .c, .flush, .save, .add_cc_1, .get_pc, .dec_word, .add_offset, .mask_word_inst, .set_pc }, // jr c,d
        &[_]Uop{ .get_sp, .get_hl, .add_words, .set_hl }, // add hl,sp
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_byte, .set_a }, // ld a,(nn)
        &[_]Uop{ .get_sp, .dec_addr, .set_sp }, // dec sp
        &[_]Uop{ .get_a, .inc_byte, .set_a }, // inc a
        &[_]Uop{ .get_a, .dec_byte, .set_a }, // dec a
        &[_]Uop{ .fetch_byte, .set_a }, // ld a,n
        &[_]Uop{.ccf}, // ccf
        &[_]Uop{ .mode_sis, .add_r_1, .fetch_byte, .dispatch_base }, // .sis
        &[_]Uop{ .get_c, .set_b }, // ld b,c
        &[_]Uop{ .get_d, .set_b }, // ld b,d
        &[_]Uop{ .get_e, .set_b }, // ld b,e
        &[_]Uop{ .get_h, .set_b }, // ld b,h
        &[_]Uop{ .get_l, .set_b }, // ld b,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .set_b }, // ld b,(hl)
        &[_]Uop{ .get_a, .set_b }, // ld b,a
        &[_]Uop{ .get_b, .set_c }, // ld c,b
        &[_]Uop{ .mode_lis, .add_r_1, .fetch_byte, .dispatch_base }, // .lis
        &[_]Uop{ .get_d, .set_c }, // ld c,d
        &[_]Uop{ .get_e, .set_c }, // ld c,e
        &[_]Uop{ .get_h, .set_c }, // ld c,h
        &[_]Uop{ .get_l, .set_c }, // ld c,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .set_c }, // ld c,(hl)
        &[_]Uop{ .get_a, .set_c }, // ld c,a
        &[_]Uop{ .get_b, .set_d }, // ld d,b
        &[_]Uop{ .get_c, .set_d }, // ld d,c
        &[_]Uop{ .mode_sil, .add_r_1, .fetch_byte, .dispatch_base }, // .sil
        &[_]Uop{ .get_e, .set_d }, // ld d,e
        &[_]Uop{ .get_h, .set_d }, // ld d,h
        &[_]Uop{ .get_l, .set_d }, // ld d,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .set_d }, // ld d,(hl)
        &[_]Uop{ .get_a, .set_d }, // ld d,a
        &[_]Uop{ .get_b, .set_e }, // ld e,b
        &[_]Uop{ .get_c, .set_e }, // ld e,c
        &[_]Uop{ .get_d, .set_e }, // ld e,d
        &[_]Uop{ .mode_lil, .add_r_1, .fetch_byte, .dispatch_base }, // .lil
        &[_]Uop{ .get_h, .set_e }, // ld e,h
        &[_]Uop{ .get_l, .set_e }, // ld e,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .set_e }, // ld e,(hl)
        &[_]Uop{ .get_a, .set_e }, // ld e,a
        &[_]Uop{ .get_b, .set_h }, // ld h,b
        &[_]Uop{ .get_c, .set_h }, // ld h,c
        &[_]Uop{ .get_d, .set_h }, // ld h,d
        &[_]Uop{ .get_e, .set_h }, // ld h,e
        &[_]Uop{}, // ld h,h
        &[_]Uop{ .get_l, .set_h }, // ld h,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .set_h }, // ld h,(hl)
        &[_]Uop{ .get_a, .set_h }, // ld h,a
        &[_]Uop{ .get_b, .set_l }, // ld l,b
        &[_]Uop{ .get_c, .set_l }, // ld l,c
        &[_]Uop{ .get_d, .set_l }, // ld l,d
        &[_]Uop{ .get_e, .set_l }, // ld l,e
        &[_]Uop{ .get_h, .set_l }, // ld l,h
        &[_]Uop{}, // ld l,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .set_l }, // ld l,(hl)
        &[_]Uop{ .get_a, .set_l }, // ld a,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .get_b, .write_byte }, // ld (hl),b
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .get_c, .write_byte }, // ld (hl),c
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .get_d, .write_byte }, // ld (hl),d
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .get_e, .write_byte }, // ld (hl),e
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .get_h, .write_byte }, // ld (hl),h
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .get_l, .write_byte }, // ld (hl),l
        &[_]Uop{.halt}, // halt
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .get_a, .write_byte }, // ld (hl),a
        &[_]Uop{ .get_b, .set_a }, // ld a,b
        &[_]Uop{ .get_c, .set_a }, // ld a,c
        &[_]Uop{ .get_d, .set_a }, // ld a,d
        &[_]Uop{ .get_e, .set_a }, // ld a,e
        &[_]Uop{ .get_h, .set_a }, // ld a,h
        &[_]Uop{ .get_l, .set_a }, // ld a,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .set_a }, // ld a,(hl)
        &[_]Uop{}, // ld a,a
        &[_]Uop{ .get_b, .save, .get_a, .add_bytes, .set_a }, // add a,b
        &[_]Uop{ .get_c, .save, .get_a, .add_bytes, .set_a }, // add a,c
        &[_]Uop{ .get_d, .save, .get_a, .add_bytes, .set_a }, // add a,d
        &[_]Uop{ .get_e, .save, .get_a, .add_bytes, .set_a }, // add a,e
        &[_]Uop{ .get_h, .save, .get_a, .add_bytes, .set_a }, // add a,h
        &[_]Uop{ .get_l, .save, .get_a, .add_bytes, .set_a }, // add a,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .add_bytes, .set_a }, // add a,(hl)
        &[_]Uop{ .get_a, .save, .add_bytes, .set_a }, // add a,a
        &[_]Uop{ .get_b, .save, .get_a, .adc_bytes, .set_a }, // adc a,b
        &[_]Uop{ .get_c, .save, .get_a, .adc_bytes, .set_a }, // adc a,c
        &[_]Uop{ .get_d, .save, .get_a, .adc_bytes, .set_a }, // adc a,d
        &[_]Uop{ .get_e, .save, .get_a, .adc_bytes, .set_a }, // adc a,e
        &[_]Uop{ .get_h, .save, .get_a, .adc_bytes, .set_a }, // adc a,h
        &[_]Uop{ .get_l, .save, .get_a, .adc_bytes, .set_a }, // adc a,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .adc_bytes, .set_a }, // adc a,(hl)
        &[_]Uop{ .get_a, .save, .adc_bytes, .set_a }, // adc a,a
        &[_]Uop{ .get_b, .save, .get_a, .sub_bytes, .set_a }, // sub a,b
        &[_]Uop{ .get_c, .save, .get_a, .sub_bytes, .set_a }, // sub a,c
        &[_]Uop{ .get_d, .save, .get_a, .sub_bytes, .set_a }, // sub a,d
        &[_]Uop{ .get_e, .save, .get_a, .sub_bytes, .set_a }, // sub a,e
        &[_]Uop{ .get_h, .save, .get_a, .sub_bytes, .set_a }, // sub a,h
        &[_]Uop{ .get_l, .save, .get_a, .sub_bytes, .set_a }, // sub a,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sub_bytes, .set_a }, // sub a,(hl)
        &[_]Uop{ .get_a, .save, .sub_bytes, .set_a }, // sub a,a
        &[_]Uop{ .get_b, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,b
        &[_]Uop{ .get_c, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,c
        &[_]Uop{ .get_d, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,d
        &[_]Uop{ .get_e, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,e
        &[_]Uop{ .get_h, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,h
        &[_]Uop{ .get_l, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,(hl)
        &[_]Uop{ .get_a, .save, .sbc_bytes, .set_a }, // sbc a,a
        &[_]Uop{ .get_b, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,b
        &[_]Uop{ .get_c, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,c
        &[_]Uop{ .get_d, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,d
        &[_]Uop{ .get_e, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,e
        &[_]Uop{ .get_h, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,h
        &[_]Uop{ .get_l, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,(hl)
        &[_]Uop{ .get_a, .save, .and_bytes, .pzs_byte, .set_a }, // and a,a
        &[_]Uop{ .get_b, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,b
        &[_]Uop{ .get_c, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,c
        &[_]Uop{ .get_d, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,d
        &[_]Uop{ .get_e, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,e
        &[_]Uop{ .get_h, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,h
        &[_]Uop{ .get_l, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,(hl)
        &[_]Uop{ .get_a, .save, .xor_bytes, .pzs_byte, .set_a }, // xor a,a
        &[_]Uop{ .get_b, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,b
        &[_]Uop{ .get_c, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,c
        &[_]Uop{ .get_d, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,d
        &[_]Uop{ .get_e, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,e
        &[_]Uop{ .get_h, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,h
        &[_]Uop{ .get_l, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,(hl)
        &[_]Uop{ .get_a, .save, .or_bytes, .pzs_byte, .set_a }, // or a,a
        &[_]Uop{ .get_b, .save, .get_a, .sub_bytes }, // cp a,b
        &[_]Uop{ .get_c, .save, .get_a, .sub_bytes }, // cp a,c
        &[_]Uop{ .get_d, .save, .get_a, .sub_bytes }, // cp a,d
        &[_]Uop{ .get_e, .save, .get_a, .sub_bytes }, // cp a,e
        &[_]Uop{ .get_h, .save, .get_a, .sub_bytes }, // cp a,h
        &[_]Uop{ .get_l, .save, .get_a, .sub_bytes }, // cp a,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sub_bytes }, // cp a,(hl)
        &[_]Uop{ .get_a, .save, .sub_bytes }, // cp a,a
        &[_]Uop{ .add_cc_1, .nz, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret nz
        &[_]Uop{ .get_sp, .mask_addr_inst, .read_word, .set_bc, .inc_addr, .mask_addr_inst, .set_sp }, // pop bc
        &[_]Uop{ .fetch_word_flush, .nz, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp nz,nn
        &[_]Uop{ .fetch_word_flush, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp nn
        &[_]Uop{ .fetch_word_flush, .nz, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call nz,nn
        &[_]Uop{ .add_r_inst, .get_sp, .dec_addr, .mask_addr_inst, .get_bc, .write_word_rev, .set_sp }, // push bc
        &[_]Uop{ .fetch_byte, .save, .get_a, .add_bytes, .set_a }, // add a,n
        &[_]Uop{ .flush, .add_cc_1, .set_00h, .ex_pc, .rst, .set_adl_inst }, // rst 00h
        &[_]Uop{ .add_cc_1, .z, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret z
        &[_]Uop{ .flush, .ret, .set_pc, .add_cc_1 }, // ret
        &[_]Uop{ .fetch_word_flush, .z, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp z,nn
        &[_]Uop{ .add_r_1, .fetch_byte, .dispatch_cb }, // CB
        &[_]Uop{ .fetch_word_flush, .z, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call z,nn
        &[_]Uop{ .fetch_word_flush, .ex_pc, .call, .set_adl_imm }, // call nn
        &[_]Uop{ .fetch_byte, .save, .get_a, .adc_bytes, .set_a }, // adc a,n
        &[_]Uop{ .flush, .add_cc_1, .set_08h, .ex_pc, .rst, .set_adl_inst }, // rst 08h
        &[_]Uop{ .add_cc_1, .nc, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret nc
        &[_]Uop{ .get_sp, .mask_addr_inst, .read_word, .set_de, .inc_addr, .mask_addr_inst, .set_sp }, // pop de
        &[_]Uop{ .fetch_word_flush, .nc, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp nc,nn
        &[_]Uop{ .fetch_byte, .get_a_high, .save, .get_a, .out }, // out (n),a
        &[_]Uop{ .fetch_word_flush, .nc, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call nc,nn
        &[_]Uop{ .add_r_inst, .get_sp, .dec_addr, .mask_addr_inst, .get_de, .write_word_rev, .set_sp }, // push de
        &[_]Uop{ .fetch_byte, .save, .get_a, .sub_bytes, .set_a }, // sub a,n
        &[_]Uop{ .flush, .add_cc_1, .set_10h, .ex_pc, .rst, .set_adl_inst }, // rst 10h
        &[_]Uop{ .add_cc_1, .c, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret c
        &[_]Uop{ .get_bc, .@"ex_bc'", .set_bc, .get_de, .@"ex_de'", .set_de, .get_hl, .@"ex_hl'", .set_hl }, // exx
        &[_]Uop{ .fetch_word_flush, .c, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp c,nn
        &[_]Uop{ .fetch_byte, .get_a_high, .save, .in, .set_a }, // in a,(n)
        &[_]Uop{ .fetch_word_flush, .c, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call c,nn
        &[_]Uop{ .add_r_1, .fetch_byte, .dispatch_dd }, // DD
        &[_]Uop{ .fetch_byte, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,n
        &[_]Uop{ .flush, .add_cc_1, .set_18h, .ex_pc, .rst, .set_adl_inst }, // rst 18h
        &[_]Uop{ .add_cc_1, .po, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret po
        &[_]Uop{ .get_sp, .mask_addr_inst, .read_word, .set_hl, .inc_addr, .mask_addr_inst, .set_sp }, // pop hl
        &[_]Uop{ .fetch_word_flush, .po, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp po,nn
        &[_]Uop{ .get_sp, .mask_addr_inst, .read_word, .ex_hl_inst, .write_word_rev }, // ex (sp),hl
        &[_]Uop{ .fetch_word_flush, .po, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call po,nn
        &[_]Uop{ .add_r_inst, .get_sp, .dec_addr, .mask_addr_inst, .get_hl, .write_word_rev, .set_sp }, // push hl
        &[_]Uop{ .fetch_byte, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,n
        &[_]Uop{ .flush, .add_cc_1, .set_20h, .ex_pc, .rst, .set_adl_inst }, // rst 20h
        &[_]Uop{ .add_cc_1, .pe, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret pe
        &[_]Uop{ .flush, .fetch_byte, .get_hl, .mask_word_inst, .set_pc, .set_adl_inst }, // jp (hl)
        &[_]Uop{ .fetch_word_flush, .pe, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp pe,nn
        &[_]Uop{ .get_de, .mask_word_inst, .ex_hl, .mask_word_inst, .set_de }, // ex de,hl
        &[_]Uop{ .fetch_word_flush, .pe, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call pe,nn
        &[_]Uop{ .add_r_1, .fetch_byte, .dispatch_ed }, // ED
        &[_]Uop{ .fetch_byte, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,n
        &[_]Uop{ .flush, .add_cc_1, .set_28h, .ex_pc, .rst, .set_adl_inst }, // rst 28h
        &[_]Uop{ .add_cc_1, .p, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret p
        &[_]Uop{ .get_sp, .mask_addr_inst, .read_word, .set_af, .inc_addr, .mask_addr_inst, .set_sp }, // pop af
        &[_]Uop{ .fetch_word_flush, .p, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp p,nn
        &[_]Uop{.clear_ief}, // di
        &[_]Uop{ .fetch_word_flush, .p, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call p,nn
        &[_]Uop{ .add_r_inst, .get_sp, .dec_addr, .mask_addr_inst, .get_af, .write_word_rev, .set_sp }, // push af
        &[_]Uop{ .fetch_byte, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,n
        &[_]Uop{ .flush, .add_cc_1, .set_30h, .ex_pc, .rst, .set_adl_inst }, // rst 30h
        &[_]Uop{ .add_cc_1, .m, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret m
        &[_]Uop{ .get_hl, .save, .set_sp }, // ld sp,hl
        &[_]Uop{ .fetch_word_flush, .m, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp m,nn
        &[_]Uop{ .set_ief, .reset, .add_r_1, .fetch_byte, .dispatch_base }, // ei
        &[_]Uop{ .fetch_word_flush, .m, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call m,nn
        &[_]Uop{ .add_r_1, .fetch_byte, .dispatch_fd }, // FD
        &[_]Uop{ .fetch_byte, .save, .get_a, .sub_bytes }, // cp a,n
        &[_]Uop{ .flush, .add_cc_1, .set_38h, .ex_pc, .rst, .set_adl_inst }, // rst 38h
    };

    const cb = [_][]const Uop{
        &[_]Uop{ .get_b, .rlc_byte, .pzs_byte, .set_b }, // rlc b
        &[_]Uop{ .get_c, .rlc_byte, .pzs_byte, .set_c }, // rlc c
        &[_]Uop{ .get_d, .rlc_byte, .pzs_byte, .set_d }, // rlc d
        &[_]Uop{ .get_e, .rlc_byte, .pzs_byte, .set_e }, // rlc e
        &[_]Uop{ .get_h, .rlc_byte, .pzs_byte, .set_h }, // rlc h
        &[_]Uop{ .get_l, .rlc_byte, .pzs_byte, .set_l }, // rlc l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rlc_byte, .pzs_byte, .write_byte }, // rlc (hl)
        &[_]Uop{ .get_a, .rlc_byte, .pzs_byte, .set_a }, // rlc a
        &[_]Uop{ .get_b, .rrc_byte, .pzs_byte, .set_b }, // rrc b
        &[_]Uop{ .get_c, .rrc_byte, .pzs_byte, .set_c }, // rrc c
        &[_]Uop{ .get_d, .rrc_byte, .pzs_byte, .set_d }, // rrc d
        &[_]Uop{ .get_e, .rrc_byte, .pzs_byte, .set_e }, // rrc e
        &[_]Uop{ .get_h, .rrc_byte, .pzs_byte, .set_h }, // rrc h
        &[_]Uop{ .get_l, .rrc_byte, .pzs_byte, .set_l }, // rrc l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rrc_byte, .pzs_byte, .write_byte }, // rrc (hl)
        &[_]Uop{ .get_a, .rrc_byte, .pzs_byte, .set_a }, // rrc a
        &[_]Uop{ .get_b, .rl_byte, .pzs_byte, .set_b }, // rl b
        &[_]Uop{ .get_c, .rl_byte, .pzs_byte, .set_c }, // rl c
        &[_]Uop{ .get_d, .rl_byte, .pzs_byte, .set_d }, // rl d
        &[_]Uop{ .get_e, .rl_byte, .pzs_byte, .set_e }, // rl e
        &[_]Uop{ .get_h, .rl_byte, .pzs_byte, .set_h }, // rl h
        &[_]Uop{ .get_l, .rl_byte, .pzs_byte, .set_l }, // rl l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rl_byte, .pzs_byte, .write_byte }, // rl (hl)
        &[_]Uop{ .get_a, .rl_byte, .pzs_byte, .set_a }, // rl a
        &[_]Uop{ .get_b, .rr_byte, .pzs_byte, .set_b }, // rr b
        &[_]Uop{ .get_c, .rr_byte, .pzs_byte, .set_c }, // rr c
        &[_]Uop{ .get_d, .rr_byte, .pzs_byte, .set_d }, // rr d
        &[_]Uop{ .get_e, .rr_byte, .pzs_byte, .set_e }, // rr e
        &[_]Uop{ .get_h, .rr_byte, .pzs_byte, .set_h }, // rr h
        &[_]Uop{ .get_l, .rr_byte, .pzs_byte, .set_l }, // rr l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rr_byte, .pzs_byte, .write_byte }, // rr (hl)
        &[_]Uop{ .get_a, .rr_byte, .pzs_byte, .set_a }, // rr a
        &[_]Uop{ .get_b, .sla_byte, .pzs_byte, .set_b }, // sla b
        &[_]Uop{ .get_c, .sla_byte, .pzs_byte, .set_c }, // sla c
        &[_]Uop{ .get_d, .sla_byte, .pzs_byte, .set_d }, // sla d
        &[_]Uop{ .get_e, .sla_byte, .pzs_byte, .set_e }, // sla e
        &[_]Uop{ .get_h, .sla_byte, .pzs_byte, .set_h }, // sla h
        &[_]Uop{ .get_l, .sla_byte, .pzs_byte, .set_l }, // sla l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sla_byte, .pzs_byte, .write_byte }, // sla (hl)
        &[_]Uop{ .get_a, .sla_byte, .pzs_byte, .set_a }, // sla a
        &[_]Uop{ .get_b, .sra_byte, .pzs_byte, .set_b }, // sra b
        &[_]Uop{ .get_c, .sra_byte, .pzs_byte, .set_c }, // sra c
        &[_]Uop{ .get_d, .sra_byte, .pzs_byte, .set_d }, // sra d
        &[_]Uop{ .get_e, .sra_byte, .pzs_byte, .set_e }, // sra e
        &[_]Uop{ .get_h, .sra_byte, .pzs_byte, .set_h }, // sra h
        &[_]Uop{ .get_l, .sra_byte, .pzs_byte, .set_l }, // sra l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sra_byte, .pzs_byte, .write_byte }, // sra (hl)
        &[_]Uop{ .get_a, .sra_byte, .pzs_byte, .set_a }, // sra a
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_b, .srl_byte, .pzs_byte, .set_b }, // srl b
        &[_]Uop{ .get_c, .srl_byte, .pzs_byte, .set_c }, // srl c
        &[_]Uop{ .get_d, .srl_byte, .pzs_byte, .set_d }, // srl d
        &[_]Uop{ .get_e, .srl_byte, .pzs_byte, .set_e }, // srl e
        &[_]Uop{ .get_h, .srl_byte, .pzs_byte, .set_h }, // srl h
        &[_]Uop{ .get_l, .srl_byte, .pzs_byte, .set_l }, // srl l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .srl_byte, .pzs_byte, .write_byte }, // srl (hl)
        &[_]Uop{ .get_a, .srl_byte, .pzs_byte, .set_a }, // srl a
        &[_]Uop{ .get_b, .bit_0_byte, .pzs_byte }, // bit 0,b
        &[_]Uop{ .get_c, .bit_0_byte, .pzs_byte }, // bit 0,c
        &[_]Uop{ .get_d, .bit_0_byte, .pzs_byte }, // bit 0,d
        &[_]Uop{ .get_e, .bit_0_byte, .pzs_byte }, // bit 0,e
        &[_]Uop{ .get_h, .bit_0_byte, .pzs_byte }, // bit 0,h
        &[_]Uop{ .get_l, .bit_0_byte, .pzs_byte }, // bit 0,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_0_byte, .pzs_byte }, // bit 0,(hl)
        &[_]Uop{ .get_a, .bit_0_byte, .pzs_byte }, // bit 0,a
        &[_]Uop{ .get_b, .bit_1_byte, .pzs_byte }, // bit 1,b
        &[_]Uop{ .get_c, .bit_1_byte, .pzs_byte }, // bit 1,c
        &[_]Uop{ .get_d, .bit_1_byte, .pzs_byte }, // bit 1,d
        &[_]Uop{ .get_e, .bit_1_byte, .pzs_byte }, // bit 1,e
        &[_]Uop{ .get_h, .bit_1_byte, .pzs_byte }, // bit 1,h
        &[_]Uop{ .get_l, .bit_1_byte, .pzs_byte }, // bit 1,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_1_byte, .pzs_byte }, // bit 1,(hl)
        &[_]Uop{ .get_a, .bit_1_byte, .pzs_byte }, // bit 1,a
        &[_]Uop{ .get_b, .bit_2_byte, .pzs_byte }, // bit 2,b
        &[_]Uop{ .get_c, .bit_2_byte, .pzs_byte }, // bit 2,c
        &[_]Uop{ .get_d, .bit_2_byte, .pzs_byte }, // bit 2,d
        &[_]Uop{ .get_e, .bit_2_byte, .pzs_byte }, // bit 2,e
        &[_]Uop{ .get_h, .bit_2_byte, .pzs_byte }, // bit 2,h
        &[_]Uop{ .get_l, .bit_2_byte, .pzs_byte }, // bit 2,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_2_byte, .pzs_byte }, // bit 2,(hl)
        &[_]Uop{ .get_a, .bit_2_byte, .pzs_byte }, // bit 2,a
        &[_]Uop{ .get_b, .bit_3_byte, .pzs_byte }, // bit 3,b
        &[_]Uop{ .get_c, .bit_3_byte, .pzs_byte }, // bit 3,c
        &[_]Uop{ .get_d, .bit_3_byte, .pzs_byte }, // bit 3,d
        &[_]Uop{ .get_e, .bit_3_byte, .pzs_byte }, // bit 3,e
        &[_]Uop{ .get_h, .bit_3_byte, .pzs_byte }, // bit 3,h
        &[_]Uop{ .get_l, .bit_3_byte, .pzs_byte }, // bit 3,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_3_byte, .pzs_byte }, // bit 3,(hl)
        &[_]Uop{ .get_a, .bit_3_byte, .pzs_byte }, // bit 3,a
        &[_]Uop{ .get_b, .bit_4_byte, .pzs_byte }, // bit 4,b
        &[_]Uop{ .get_c, .bit_4_byte, .pzs_byte }, // bit 4,c
        &[_]Uop{ .get_d, .bit_4_byte, .pzs_byte }, // bit 4,d
        &[_]Uop{ .get_e, .bit_4_byte, .pzs_byte }, // bit 4,e
        &[_]Uop{ .get_h, .bit_4_byte, .pzs_byte }, // bit 4,h
        &[_]Uop{ .get_l, .bit_4_byte, .pzs_byte }, // bit 4,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_4_byte, .pzs_byte }, // bit 4,(hl)
        &[_]Uop{ .get_a, .bit_4_byte, .pzs_byte }, // bit 4,a
        &[_]Uop{ .get_b, .bit_5_byte, .pzs_byte }, // bit 5,b
        &[_]Uop{ .get_c, .bit_5_byte, .pzs_byte }, // bit 5,c
        &[_]Uop{ .get_d, .bit_5_byte, .pzs_byte }, // bit 5,d
        &[_]Uop{ .get_e, .bit_5_byte, .pzs_byte }, // bit 5,e
        &[_]Uop{ .get_h, .bit_5_byte, .pzs_byte }, // bit 5,h
        &[_]Uop{ .get_l, .bit_5_byte, .pzs_byte }, // bit 5,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_5_byte, .pzs_byte }, // bit 5,(hl)
        &[_]Uop{ .get_a, .bit_5_byte, .pzs_byte }, // bit 5,a
        &[_]Uop{ .get_b, .bit_6_byte, .pzs_byte }, // bit 6,b
        &[_]Uop{ .get_c, .bit_6_byte, .pzs_byte }, // bit 6,c
        &[_]Uop{ .get_d, .bit_6_byte, .pzs_byte }, // bit 6,d
        &[_]Uop{ .get_e, .bit_6_byte, .pzs_byte }, // bit 6,e
        &[_]Uop{ .get_h, .bit_6_byte, .pzs_byte }, // bit 6,h
        &[_]Uop{ .get_l, .bit_6_byte, .pzs_byte }, // bit 6,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_6_byte, .pzs_byte }, // bit 6,(hl)
        &[_]Uop{ .get_a, .bit_6_byte, .pzs_byte }, // bit 6,a
        &[_]Uop{ .get_b, .bit_7_byte, .pzs_byte }, // bit 7,b
        &[_]Uop{ .get_c, .bit_7_byte, .pzs_byte }, // bit 7,c
        &[_]Uop{ .get_d, .bit_7_byte, .pzs_byte }, // bit 7,d
        &[_]Uop{ .get_e, .bit_7_byte, .pzs_byte }, // bit 7,e
        &[_]Uop{ .get_h, .bit_7_byte, .pzs_byte }, // bit 7,h
        &[_]Uop{ .get_l, .bit_7_byte, .pzs_byte }, // bit 7,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_7_byte, .pzs_byte }, // bit 7,(hl)
        &[_]Uop{ .get_a, .bit_7_byte, .pzs_byte }, // bit 7,a
        &[_]Uop{ .get_b, .res_0_byte, .set_b }, // res 0,b
        &[_]Uop{ .get_c, .res_0_byte, .set_c }, // res 0,c
        &[_]Uop{ .get_d, .res_0_byte, .set_d }, // res 0,d
        &[_]Uop{ .get_e, .res_0_byte, .set_e }, // res 0,e
        &[_]Uop{ .get_h, .res_0_byte, .set_h }, // res 0,h
        &[_]Uop{ .get_l, .res_0_byte, .set_l }, // res 0,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_0_byte, .write_byte }, // res 0,(hl)
        &[_]Uop{ .get_a, .res_0_byte, .set_a }, // res 0,a
        &[_]Uop{ .get_b, .res_1_byte, .set_b }, // res 1,b
        &[_]Uop{ .get_c, .res_1_byte, .set_c }, // res 1,c
        &[_]Uop{ .get_d, .res_1_byte, .set_d }, // res 1,d
        &[_]Uop{ .get_e, .res_1_byte, .set_e }, // res 1,e
        &[_]Uop{ .get_h, .res_1_byte, .set_h }, // res 1,h
        &[_]Uop{ .get_l, .res_1_byte, .set_l }, // res 1,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_1_byte, .write_byte }, // res 1,(hl)
        &[_]Uop{ .get_a, .res_1_byte, .set_a }, // res 1,a
        &[_]Uop{ .get_b, .res_2_byte, .set_b }, // res 2,b
        &[_]Uop{ .get_c, .res_2_byte, .set_c }, // res 2,c
        &[_]Uop{ .get_d, .res_2_byte, .set_d }, // res 2,d
        &[_]Uop{ .get_e, .res_2_byte, .set_e }, // res 2,e
        &[_]Uop{ .get_h, .res_2_byte, .set_h }, // res 2,h
        &[_]Uop{ .get_l, .res_2_byte, .set_l }, // res 2,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_2_byte, .write_byte }, // res 2,(hl)
        &[_]Uop{ .get_a, .res_2_byte, .set_a }, // res 2,a
        &[_]Uop{ .get_b, .res_3_byte, .set_b }, // res 3,b
        &[_]Uop{ .get_c, .res_3_byte, .set_c }, // res 3,c
        &[_]Uop{ .get_d, .res_3_byte, .set_d }, // res 3,d
        &[_]Uop{ .get_e, .res_3_byte, .set_e }, // res 3,e
        &[_]Uop{ .get_h, .res_3_byte, .set_h }, // res 3,h
        &[_]Uop{ .get_l, .res_3_byte, .set_l }, // res 3,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_3_byte, .write_byte }, // res 3,(hl)
        &[_]Uop{ .get_a, .res_3_byte, .set_a }, // res 3,a
        &[_]Uop{ .get_b, .res_4_byte, .set_b }, // res 4,b
        &[_]Uop{ .get_c, .res_4_byte, .set_c }, // res 4,c
        &[_]Uop{ .get_d, .res_4_byte, .set_d }, // res 4,d
        &[_]Uop{ .get_e, .res_4_byte, .set_e }, // res 4,e
        &[_]Uop{ .get_h, .res_4_byte, .set_h }, // res 4,h
        &[_]Uop{ .get_l, .res_4_byte, .set_l }, // res 4,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_4_byte, .write_byte }, // res 4,(hl)
        &[_]Uop{ .get_a, .res_4_byte, .set_a }, // res 4,a
        &[_]Uop{ .get_b, .res_5_byte, .set_b }, // res 5,b
        &[_]Uop{ .get_c, .res_5_byte, .set_c }, // res 5,c
        &[_]Uop{ .get_d, .res_5_byte, .set_d }, // res 5,d
        &[_]Uop{ .get_e, .res_5_byte, .set_e }, // res 5,e
        &[_]Uop{ .get_h, .res_5_byte, .set_h }, // res 5,h
        &[_]Uop{ .get_l, .res_5_byte, .set_l }, // res 5,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_5_byte, .write_byte }, // res 5,(hl)
        &[_]Uop{ .get_a, .res_5_byte, .set_a }, // res 5,a
        &[_]Uop{ .get_b, .res_6_byte, .set_b }, // res 6,b
        &[_]Uop{ .get_c, .res_6_byte, .set_c }, // res 6,c
        &[_]Uop{ .get_d, .res_6_byte, .set_d }, // res 6,d
        &[_]Uop{ .get_e, .res_6_byte, .set_e }, // res 6,e
        &[_]Uop{ .get_h, .res_6_byte, .set_h }, // res 6,h
        &[_]Uop{ .get_l, .res_6_byte, .set_l }, // res 6,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_6_byte, .write_byte }, // res 6,(hl)
        &[_]Uop{ .get_a, .res_6_byte, .set_a }, // res 6,a
        &[_]Uop{ .get_b, .res_7_byte, .set_b }, // res 7,b
        &[_]Uop{ .get_c, .res_7_byte, .set_c }, // res 7,c
        &[_]Uop{ .get_d, .res_7_byte, .set_d }, // res 7,d
        &[_]Uop{ .get_e, .res_7_byte, .set_e }, // res 7,e
        &[_]Uop{ .get_h, .res_7_byte, .set_h }, // res 7,h
        &[_]Uop{ .get_l, .res_7_byte, .set_l }, // res 7,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_7_byte, .write_byte }, // res 7,(hl)
        &[_]Uop{ .get_a, .res_7_byte, .set_a }, // res 7,a
        &[_]Uop{ .get_b, .set_0_byte, .set_b }, // set 0,b
        &[_]Uop{ .get_c, .set_0_byte, .set_c }, // set 0,c
        &[_]Uop{ .get_d, .set_0_byte, .set_d }, // set 0,d
        &[_]Uop{ .get_e, .set_0_byte, .set_e }, // set 0,e
        &[_]Uop{ .get_h, .set_0_byte, .set_h }, // set 0,h
        &[_]Uop{ .get_l, .set_0_byte, .set_l }, // set 0,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_0_byte, .write_byte }, // set 0,(hl)
        &[_]Uop{ .get_a, .set_0_byte, .set_a }, // set 0,a
        &[_]Uop{ .get_b, .set_1_byte, .set_b }, // set 1,b
        &[_]Uop{ .get_c, .set_1_byte, .set_c }, // set 1,c
        &[_]Uop{ .get_d, .set_1_byte, .set_d }, // set 1,d
        &[_]Uop{ .get_e, .set_1_byte, .set_e }, // set 1,e
        &[_]Uop{ .get_h, .set_1_byte, .set_h }, // set 1,h
        &[_]Uop{ .get_l, .set_1_byte, .set_l }, // set 1,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_1_byte, .write_byte }, // set 1,(hl)
        &[_]Uop{ .get_a, .set_1_byte, .set_a }, // set 1,a
        &[_]Uop{ .get_b, .set_2_byte, .set_b }, // set 2,b
        &[_]Uop{ .get_c, .set_2_byte, .set_c }, // set 2,c
        &[_]Uop{ .get_d, .set_2_byte, .set_d }, // set 2,d
        &[_]Uop{ .get_e, .set_2_byte, .set_e }, // set 2,e
        &[_]Uop{ .get_h, .set_2_byte, .set_h }, // set 2,h
        &[_]Uop{ .get_l, .set_2_byte, .set_l }, // set 2,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_2_byte, .write_byte }, // set 2,(hl)
        &[_]Uop{ .get_a, .set_2_byte, .set_a }, // set 2,a
        &[_]Uop{ .get_b, .set_3_byte, .set_b }, // set 3,b
        &[_]Uop{ .get_c, .set_3_byte, .set_c }, // set 3,c
        &[_]Uop{ .get_d, .set_3_byte, .set_d }, // set 3,d
        &[_]Uop{ .get_e, .set_3_byte, .set_e }, // set 3,e
        &[_]Uop{ .get_h, .set_3_byte, .set_h }, // set 3,h
        &[_]Uop{ .get_l, .set_3_byte, .set_l }, // set 3,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_3_byte, .write_byte }, // set 3,(hl)
        &[_]Uop{ .get_a, .set_3_byte, .set_a }, // set 3,a
        &[_]Uop{ .get_b, .set_4_byte, .set_b }, // set 4,b
        &[_]Uop{ .get_c, .set_4_byte, .set_c }, // set 4,c
        &[_]Uop{ .get_d, .set_4_byte, .set_d }, // set 4,d
        &[_]Uop{ .get_e, .set_4_byte, .set_e }, // set 4,e
        &[_]Uop{ .get_h, .set_4_byte, .set_h }, // set 4,h
        &[_]Uop{ .get_l, .set_4_byte, .set_l }, // set 4,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_4_byte, .write_byte }, // set 4,(hl)
        &[_]Uop{ .get_a, .set_4_byte, .set_a }, // set 4,a
        &[_]Uop{ .get_b, .set_5_byte, .set_b }, // set 5,b
        &[_]Uop{ .get_c, .set_5_byte, .set_c }, // set 5,c
        &[_]Uop{ .get_d, .set_5_byte, .set_d }, // set 5,d
        &[_]Uop{ .get_e, .set_5_byte, .set_e }, // set 5,e
        &[_]Uop{ .get_h, .set_5_byte, .set_h }, // set 5,h
        &[_]Uop{ .get_l, .set_5_byte, .set_l }, // set 5,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_5_byte, .write_byte }, // set 5,(hl)
        &[_]Uop{ .get_a, .set_5_byte, .set_a }, // set 5,a
        &[_]Uop{ .get_b, .set_6_byte, .set_b }, // set 6,b
        &[_]Uop{ .get_c, .set_6_byte, .set_c }, // set 6,c
        &[_]Uop{ .get_d, .set_6_byte, .set_d }, // set 6,d
        &[_]Uop{ .get_e, .set_6_byte, .set_e }, // set 6,e
        &[_]Uop{ .get_h, .set_6_byte, .set_h }, // set 6,h
        &[_]Uop{ .get_l, .set_6_byte, .set_l }, // set 6,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_6_byte, .write_byte }, // set 6,(hl)
        &[_]Uop{ .get_a, .set_6_byte, .set_a }, // set 6,a
        &[_]Uop{ .get_b, .set_7_byte, .set_b }, // set 7,b
        &[_]Uop{ .get_c, .set_7_byte, .set_c }, // set 7,c
        &[_]Uop{ .get_d, .set_7_byte, .set_d }, // set 7,d
        &[_]Uop{ .get_e, .set_7_byte, .set_e }, // set 7,e
        &[_]Uop{ .get_h, .set_7_byte, .set_h }, // set 7,h
        &[_]Uop{ .get_l, .set_7_byte, .set_l }, // set 7,l
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_7_byte, .write_byte }, // set 7,(hl)
        &[_]Uop{ .get_a, .set_7_byte, .set_a }, // set 7,a
    };

    const dd = [_][]const Uop{
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_word, .set_bc }, // ld bc,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_bc, .save, .get_ix, .add_words, .set_ix }, // add ix,bc
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_bc, .write_word }, // ld (ix+d),bc
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_word, .set_de }, // ld de,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_de, .save, .get_ix, .add_words, .set_ix }, // add ix,de
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_de, .write_word }, // ld (ix+d),de
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_word, .mask_word_inst, .set_ix }, // ld ix,nn
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .get_ix, .write_word }, // ld (nn),ix
        &[_]Uop{ .get_ix, .inc_word, .mask_word_inst, .set_ix }, // inc ix
        &[_]Uop{ .get_ixh, .inc_byte, .set_ixh }, // inc ixh
        &[_]Uop{ .get_ixh, .dec_byte, .set_ixh }, // dec ixh
        &[_]Uop{ .fetch_byte, .set_ixh }, // ld ixh,n
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_word, .set_hl }, // ld hl,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_ix, .save, .add_words, .set_ix }, // add ix,ix
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_word, .set_ix }, // ld ix,(nn)
        &[_]Uop{ .get_ix, .dec_word, .mask_word_inst, .set_ix }, // dec ix
        &[_]Uop{ .get_ixl, .inc_byte, .set_ixl }, // inc ixl
        &[_]Uop{ .get_ixl, .dec_byte, .set_ixl }, // dec ixl
        &[_]Uop{ .fetch_byte, .set_ixl }, // ld ixl,n
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_hl, .write_word }, // ld (ix+d),hl
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_word, .set_iy }, // ld iy,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .inc_byte, .write_byte }, // inc (ix+d)
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .dec_byte, .write_byte }, // dec (ix+d)
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .fetch_byte, .write_byte }, // ld (ix+d),n
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_word, .set_ix }, // ld ix,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_sp, .get_ix, .add_words, .set_ix }, // add ix,sp
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_iy, .write_word }, // ld (ix+d),iy
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_ix, .write_word }, // ld (ix+d),ix
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_ixh, .set_b }, // ld b,ixh
        &[_]Uop{ .get_ixl, .set_b }, // ld b,ixl
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .set_b }, // ld b,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_ixh, .set_c }, // ld c,ixh
        &[_]Uop{ .get_ixl, .set_c }, // ld c,ixl
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .set_c }, // ld c,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_ixh, .set_d }, // ld d,ixh
        &[_]Uop{ .get_ixl, .set_d }, // ld d,ixl
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .set_d }, // ld d,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_ixh, .set_e }, // ld e,ixh
        &[_]Uop{ .get_ixl, .set_e }, // ld e,ixl
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .set_e }, // ld e,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_b, .set_ixh }, // ld ixh,b
        &[_]Uop{ .get_c, .set_ixh }, // ld ixh,c
        &[_]Uop{ .get_d, .set_ixh }, // ld ixh,d
        &[_]Uop{ .get_e, .set_ixh }, // ld ixh,e
        &[_]Uop{}, // ld ixh,ixh
        &[_]Uop{ .get_ixl, .set_ixh }, // ld ixh,ixl
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .set_h }, // ld h,(ix+d)
        &[_]Uop{ .get_a, .set_ixh }, // ld ixh,a
        &[_]Uop{ .get_b, .set_ixl }, // ld ixl,b
        &[_]Uop{ .get_c, .set_ixl }, // ld ixl,c
        &[_]Uop{ .get_d, .set_ixl }, // ld ixl,d
        &[_]Uop{ .get_e, .set_ixl }, // ld ixl,e
        &[_]Uop{ .get_ixh, .set_ixl }, // ld ixl,ixh
        &[_]Uop{}, // ld ixl,ixl
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .set_l }, // ld l,(ix+d)
        &[_]Uop{ .get_a, .set_ixl }, // ld ixl,a
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_b, .write_byte }, // ld (ix+d),b
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_c, .write_byte }, // ld (ix+d),c
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_d, .write_byte }, // ld (ix+d),d
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_e, .write_byte }, // ld (ix+d),e
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_h, .write_byte }, // ld (ix+d),h
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_l, .write_byte }, // ld (ix+d),l
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_a, .write_byte }, // ld (ix+d),a
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_ixh, .set_a }, // ld a,ixh
        &[_]Uop{ .get_ixl, .set_a }, // ld a,ixl
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .set_a }, // ld a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_ixh, .save, .get_a, .add_bytes, .set_a }, // add a,ixh
        &[_]Uop{ .get_ixl, .save, .get_a, .add_bytes, .set_a }, // add a,ixl
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .add_bytes, .set_a }, // add a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_ixh, .save, .get_a, .adc_bytes, .set_a }, // adc a,ixh
        &[_]Uop{ .get_ixl, .save, .get_a, .adc_bytes, .set_a }, // adc a,ixl
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .adc_bytes, .set_a }, // adc a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_ixh, .save, .get_a, .sub_bytes, .set_a }, // sub a,ixh
        &[_]Uop{ .get_ixl, .save, .get_a, .sub_bytes, .set_a }, // sub a,ixl
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sub_bytes, .set_a }, // sub a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_ixh, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,ixh
        &[_]Uop{ .get_ixl, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,ixl
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_ixh, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,ixh
        &[_]Uop{ .get_ixl, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,ixl
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_ixh, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,ixh
        &[_]Uop{ .get_ixl, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,ixl
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_ixh, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,ixh
        &[_]Uop{ .get_ixl, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,ixl
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_ixh, .save, .get_a, .sub_bytes }, // cp a,ixh
        &[_]Uop{ .get_ixl, .save, .get_a, .sub_bytes }, // cp a,ixl
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sub_bytes }, // cp a,(ix+d)
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
        &[_]Uop{ .get_sp, .mask_addr_inst, .read_word, .set_ix, .inc_addr, .mask_addr_inst, .set_sp }, // pop ix
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_sp, .mask_addr_inst, .read_word, .ex_ix_inst, .write_word_rev }, // ex (sp),ix
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_sp, .dec_addr, .mask_addr_inst, .get_ix, .write_word_rev, .set_sp }, // push ix
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .get_ix, .mask_word_inst, .set_pc, .set_adl_inst }, // jp (ix)
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
        &[_]Uop{ .get_ix, .save, .set_sp }, // ld sp,ix
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
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rlc_byte, .pzs_byte, .write_byte }, // rlc (ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rrc_byte, .pzs_byte, .write_byte }, // rrc (ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rl_byte, .pzs_byte, .write_byte }, // rl (ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rr_byte, .pzs_byte, .write_byte }, // rr (ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sla_byte, .pzs_byte, .write_byte }, // sla (ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sra_byte, .pzs_byte, .write_byte }, // sra (ix+d)
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
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .srl_byte, .pzs_byte, .write_byte }, // srl (ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_0_byte, .pzs_byte }, // bit 0,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_1_byte, .pzs_byte }, // bit 1,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_2_byte, .pzs_byte }, // bit 2,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_3_byte, .pzs_byte }, // bit 3,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_4_byte, .pzs_byte }, // bit 4,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_5_byte, .pzs_byte }, // bit 5,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_6_byte, .pzs_byte }, // bit 6,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_7_byte, .pzs_byte }, // bit 7,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_0_byte, .write_byte }, // res 0,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_1_byte, .write_byte }, // res 1,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_2_byte, .write_byte }, // res 2,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_3_byte, .write_byte }, // res 3,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_4_byte, .write_byte }, // res 4,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_5_byte, .write_byte }, // res 5,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_6_byte, .write_byte }, // res 6,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_7_byte, .write_byte }, // res 7,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_0_byte, .write_byte }, // set 0,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_1_byte, .write_byte }, // set 1,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_2_byte, .write_byte }, // set 2,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_3_byte, .write_byte }, // set 3,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_4_byte, .write_byte }, // set 4,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_5_byte, .write_byte }, // set 5,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_6_byte, .write_byte }, // set 6,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_7_byte, .write_byte }, // set 7,(ix+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
    };

    const ed = [_][]const Uop{
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte, .set_b }, // in0 b,(n)
        &[_]Uop{ .fetch_byte, .save, .get_b, .out }, // out0 (n),b
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .mask_word_inst, .set_bc }, // lea bc,ix+d
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .mask_word_inst, .set_bc }, // lea bc,iy+d
        &[_]Uop{ .get_b, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,b
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_word, .set_bc }, // ld bc,(hl)
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte, .set_c }, // in0 c,(n)
        &[_]Uop{ .fetch_byte, .save, .get_c, .out }, // out0 (n),c
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_c, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,c
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .get_bc, .write_word }, // ld (hl),bc
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte, .set_d }, // in0 d,(n)
        &[_]Uop{ .fetch_byte, .save, .get_d, .out }, // out0 (n),d
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .mask_word_inst, .set_de }, // lea de,ix+d
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .mask_word_inst, .set_de }, // lea de,iy+d
        &[_]Uop{ .get_d, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,d
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_word, .set_de }, // ld de,(hl)
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte, .set_e }, // in0 e,(n)
        &[_]Uop{ .fetch_byte, .save, .get_e, .out }, // out0 (n),e
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_e, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,e
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .get_de, .write_word }, // ld (hl),de
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte, .set_h }, // in0 h,(n)
        &[_]Uop{ .fetch_byte, .save, .get_h, .out }, // out0 (n),h
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .mask_word_inst, .set_hl }, // lea hl,ix+d
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .mask_word_inst, .set_hl }, // lea hl,iy+d
        &[_]Uop{ .get_h, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,h
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_word, .set_hl }, // ld hl,(hl)
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte, .set_l }, // in0 l,(n)
        &[_]Uop{ .fetch_byte, .save, .get_l, .out }, // out0 (n),l
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_l, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,l
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .write_word }, // ld (hl),hl
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte }, // in0 (n)
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_word, .set_iy }, // ld iy,(hl)
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .mask_word_inst, .set_ix }, // lea ix,ix+d
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .mask_word_inst, .set_iy }, // lea iy,iy+d
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,(hl)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_word, .set_ix }, // ld ix,(hl)
        &[_]Uop{ .fetch_byte, .save, .in_f, .pzs_byte, .set_a }, // in0 a,(n)
        &[_]Uop{ .fetch_byte, .save, .get_a, .out }, // out0 (n),a
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_a, .save, .and_bytes, .pzs_byte }, // tst a,a
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .get_iy, .write_word }, // ld (hl),iy
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .get_ix, .write_word }, // ld (hl),ix
        &[_]Uop{ .get_bc, .save, .in_f, .pzs_byte, .set_b }, // in b,(bc)
        &[_]Uop{ .get_bc, .save, .get_b, .out }, // out (bc),b
        &[_]Uop{ .get_bc, .save, .get_hl, .sbc_words, .set_hl }, // sbc hl,bc
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .get_bc, .write_word }, // ld (nn),bc
        &[_]Uop{ .get_a, .save, .set_00h, .sub_bytes, .set_a }, // neg
        &[_]Uop{ .flush, .ret, .set_pc, .add_cc_1, .copy_ief }, // retn
        &[_]Uop{.im_0}, // im 0
        &[_]Uop{ .get_a, .set_i }, // ld i,a
        &[_]Uop{ .get_bc, .save, .in_f, .pzs_byte, .set_c }, // in c,(bc)
        &[_]Uop{ .get_bc, .save, .get_c, .out }, // out (bc),c
        &[_]Uop{ .get_bc, .save, .get_hl, .adc_words, .set_hl }, // adc hl,bc
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_word, .set_bc }, // ld bc,(nn)
        &[_]Uop{ .get_bc, .add_cc_4, .mlt_bytes, .set_bc }, // mlt bc
        &[_]Uop{ .flush, .ret, .set_pc, .add_cc_1, .copy_ief }, // reti
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_a, .set_r }, // ld r,a
        &[_]Uop{ .get_bc, .save, .in_f, .pzs_byte, .set_d }, // in d,(bc)
        &[_]Uop{ .get_bc, .save, .get_d, .out }, // out (bc),d
        &[_]Uop{ .get_de, .save, .get_hl, .sbc_words, .set_hl }, // sbc hl,de
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .get_de, .write_word }, // ld (nn),de
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .mask_word_inst, .set_ix }, // lea ix,iy+d
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .mask_word_inst, .set_iy }, // lea iy,ix+d
        &[_]Uop{.im_1}, // im 1
        &[_]Uop{ .get_i, .set_a }, // ld a,i
        &[_]Uop{ .get_bc, .save, .in_f, .pzs_byte, .set_e }, // in e,(bc)
        &[_]Uop{ .get_bc, .save, .get_e, .out }, // out (bc),e
        &[_]Uop{ .get_de, .save, .get_hl, .adc_words, .set_hl }, // adc hl,de
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_word, .set_de }, // ld de,(nn)
        &[_]Uop{ .get_de, .add_cc_4, .mlt_bytes, .set_de }, // mlt de
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{.im_2}, // im 2
        &[_]Uop{ .get_r, .set_a }, // ld a,r
        &[_]Uop{ .get_bc, .save, .in_f, .pzs_byte, .set_h }, // in h,(bc)
        &[_]Uop{ .get_bc, .save, .get_h, .out }, // out (bc),h
        &[_]Uop{ .get_hl, .save, .sbc_words, .set_hl }, // sbc hl,hl
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .get_hl, .write_word }, // ld (nn),hl
        &[_]Uop{ .fetch_byte, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,n
        &[_]Uop{ .fetch_byte, .save, .get_ix, .add_offset, .get_sp, .dec_addr, .mask_addr_inst, .write_word_rev, .set_sp }, // pea ix+d
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .get_sp, .dec_addr, .mask_addr_inst, .write_word_rev, .set_sp }, // pea iy+d
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .get_a_high, .add_cc_1, .rrd_bytes, .set_a_high, .write_byte }, // rrd
        &[_]Uop{ .get_bc, .save, .in_f, .pzs_byte, .set_l }, // in l,(bc)
        &[_]Uop{ .get_bc, .save, .get_l, .out }, // out (bc),l
        &[_]Uop{ .get_hl, .save, .adc_words, .set_hl }, // adc hl,hl
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_word, .set_hl }, // ld hl,(nn)
        &[_]Uop{ .get_hl, .add_cc_4, .mlt_bytes, .set_hl }, // mlt hl
        &[_]Uop{ .adl, .get_a, .set_mb }, // ld mb,a
        &[_]Uop{ .get_mb, .set_a }, // ld a,mb
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .get_a_high, .add_cc_1, .rld_bytes, .set_a_high, .write_byte }, // rld
        &[_]Uop{ .get_bc, .save, .in_f, .pzs_byte }, // in (bc)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_sp, .get_hl, .sbc_words, .set_hl }, // sbc hl,sp
        &[_]Uop{ .fetch_word, .get_sp, .swap, .mask_addr_inst, .write_word }, // ld (nn),sp
        &[_]Uop{ .get_c, .save, .in, .save, .fetch_byte, .swap, .and_bytes, .pzs_byte }, // tstio n
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{.halt}, // slp
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_bc, .save, .in_f, .pzs_byte, .set_a }, // in a,(bc)
        &[_]Uop{ .get_bc, .save, .get_a, .out }, // out (bc),a
        &[_]Uop{ .get_sp, .get_hl, .adc_words, .set_hl }, // adc hl,sp
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_word, .save, .set_sp }, // ld sp,(nn)
        &[_]Uop{ .get_sp, .restore, .add_cc_4, .mlt_bytes, .save, .set_sp }, // mlt sp
        &[_]Uop{.set_madl}, // stmix
        &[_]Uop{.clear_madl}, // rsmix
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_c, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_hl, .get_c, .add_byte_1, .set_c, .get_b, .dec_byte_hzs, .set_b }, // inim
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_c, .swap, .add_cc_1, .out, .nf_byte_sign, .get_c, .add_byte_1, .set_c, .get_b, .dec_byte_hzs, .set_b }, // otim
        &[_]Uop{ .get_bc, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_hl, .get_c, .add_byte_1, .set_c, .get_b, .sub_byte_1, .zf_byte, .set_b }, // ini2
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_c, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_hl, .get_c, .sub_byte_1, .set_c, .get_b, .dec_byte_hzs, .set_b }, // indm
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_c, .swap, .add_cc_1, .out, .nf_byte_sign, .get_c, .sub_byte_1, .set_c, .get_b, .dec_byte_hzs, .set_b }, // otdm
        &[_]Uop{ .get_bc, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_hl, .get_c, .sub_byte_1, .set_c, .get_b, .sub_byte_1, .zf_byte, .set_b }, // ind2
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .sub_r_1, .get_c, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_hl, .get_c, .add_byte_1, .set_c, .get_b, .sub_byte_1, .zf_byte, .set_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // inimr
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_c, .swap, .add_cc_1, .out, .nf_byte_sign, .get_c, .add_byte_1, .set_c, .get_b, .dec_byte_hzs, .set_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // otimr
        &[_]Uop{ .get_de, .save, .in, .inc_addr, .swap, .mask_word_inst, .set_de, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_hl, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // ini2r
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .sub_r_1, .get_c, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_hl, .get_c, .sub_byte_1, .set_c, .get_b, .sub_byte_1, .zf_byte, .set_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // indmr
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_c, .swap, .add_cc_1, .out, .nf_byte_sign, .get_c, .sub_byte_1, .set_c, .get_b, .dec_byte_hzs, .set_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // otdmr
        &[_]Uop{ .get_de, .save, .in, .dec_addr, .swap, .mask_word_inst, .set_de, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_hl, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_1, .repeat, .sub_r_1, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // ind2r
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .inc_addr, .restore, .mask_word_inst, .set_de, .ld_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst }, // ldi
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_a, .cp_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst }, // cpi
        &[_]Uop{ .get_bc, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_hl, .get_b, .sub_byte_1, .zf_byte, .set_b }, // ini
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .get_b, .sub_byte_1, .zf_byte, .set_b }, // outi
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .get_c, .add_byte_1, .set_c, .get_b, .sub_byte_1, .zf_byte, .set_b }, // outi2
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .dec_addr, .restore, .mask_word_inst, .set_de, .ld_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst }, // ldd
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_a, .cp_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst }, // cpd
        &[_]Uop{ .get_bc, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_hl, .get_b, .sub_byte_1, .zf_byte, .set_b }, // ind
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .get_b, .sub_byte_1, .zf_byte, .set_b }, // outd
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .get_c, .sub_byte_1, .set_c, .get_b, .sub_byte_1, .set_b, .zf_byte }, // outd2
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .inc_addr, .restore, .mask_word_inst, .set_de, .ld_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst, .pe, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // ldir
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_a, .cp_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst, .add_cc_1, .nz, .pe, .add_r_2, .add_cc_1, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // cpir
        &[_]Uop{ .get_bc, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_hl, .get_b, .sub_byte_1, .zf_byte, .set_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // inir
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .get_b, .sub_byte_1, .zf_byte, .set_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // otir
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .add_cc_1, .out, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_de, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // oti2r
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .dec_addr, .restore, .mask_word_inst, .set_de, .ld_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst, .pe, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // lddr
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_a, .cp_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst, .add_cc_1, .nz, .pe, .add_r_2, .add_cc_1, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // cpdr
        &[_]Uop{ .get_bc, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_hl, .get_b, .sub_byte_1, .zf_byte, .set_b, .nz, .add_r_1, .repeat, .sub_r_1, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // indr
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .get_b, .sub_byte_1, .zf_byte, .set_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // otdr
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .add_cc_1, .out, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_de, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // otd2r
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_de, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_hl, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // inirx
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .add_cc_1, .out, .nf_byte_sign, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // otirx
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_hl, .set_i }, // ld i,hl
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_de, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_hl, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_1, .repeat, .sub_r_1, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // indrx
        &[_]Uop{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .add_cc_1, .out, .nf_byte_sign, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // otdrx
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
        &[_]Uop{ .get_mbi, .mask_word_inst, .set_hl }, // ld hl,i
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
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_word, .set_bc }, // ld bc,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_bc, .save, .get_iy, .add_words, .set_iy }, // add iy,bc
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_bc, .write_word }, // ld (iy+d),bc
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_word, .set_de }, // ld de,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_de, .save, .get_iy, .add_words, .set_iy }, // add iy,de
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_de, .write_word }, // ld (iy+d),de
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_word, .mask_word_inst, .set_iy }, // ld iy,nn
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .get_iy, .write_word }, // ld (nn),iy
        &[_]Uop{ .get_iy, .inc_word, .mask_word_inst, .set_iy }, // inc iy
        &[_]Uop{ .get_iyh, .inc_byte, .set_iyh }, // inc iyh
        &[_]Uop{ .get_iyh, .dec_byte, .set_iyh }, // dec iyh
        &[_]Uop{ .fetch_byte, .set_iyh }, // ld iyh,n
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_word, .set_hl }, // ld hl,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_iy, .save, .add_words, .set_iy }, // add iy,iy
        &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_word, .set_iy }, // ld iy,(nn)
        &[_]Uop{ .get_iy, .dec_word, .mask_word_inst, .set_iy }, // dec iy
        &[_]Uop{ .get_iyl, .inc_byte, .set_iyl }, // inc iyl
        &[_]Uop{ .get_iyl, .dec_byte, .set_iyl }, // dec iyl
        &[_]Uop{ .fetch_byte, .set_iyl }, // ld iyl,n
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_hl, .write_word }, // ld (iy+d),hl
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_word, .set_ix }, // ld ix,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .inc_byte, .write_byte }, // inc (iy+d)
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .dec_byte, .write_byte }, // dec (iy+d)
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .fetch_byte, .write_byte }, // ld (iy+d),n
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_word, .set_iy }, // ld iy,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_sp, .get_iy, .add_words, .set_iy }, // add iy,sp
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_ix, .write_word }, // ld (iy+d),ix
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_iy, .write_word }, // ld (iy+d),iy
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_iyh, .set_b }, // ld b,iyh
        &[_]Uop{ .get_iyl, .set_b }, // ld b,iyl
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .set_b }, // ld b,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_iyh, .set_c }, // ld c,iyh
        &[_]Uop{ .get_iyl, .set_c }, // ld c,iyl
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .set_c }, // ld c,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_iyh, .set_d }, // ld d,iyh
        &[_]Uop{ .get_iyl, .set_d }, // ld d,iyl
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .set_d }, // ld d,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_iyh, .set_e }, // ld e,iyh
        &[_]Uop{ .get_iyl, .set_e }, // ld e,iyl
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .set_e }, // ld e,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_b, .set_iyh }, // ld iyh,b
        &[_]Uop{ .get_c, .set_iyh }, // ld iyh,c
        &[_]Uop{ .get_d, .set_iyh }, // ld iyh,d
        &[_]Uop{ .get_e, .set_iyh }, // ld iyh,e
        &[_]Uop{}, // ld iyh,iyh
        &[_]Uop{ .get_iyl, .set_iyh }, // ld iyh,iyl
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .set_h }, // ld h,(iy+d)
        &[_]Uop{ .get_a, .set_iyh }, // ld iyh,a
        &[_]Uop{ .get_b, .set_iyl }, // ld iyl,b
        &[_]Uop{ .get_c, .set_iyl }, // ld iyl,c
        &[_]Uop{ .get_d, .set_iyl }, // ld iyl,d
        &[_]Uop{ .get_e, .set_iyl }, // ld iyl,e
        &[_]Uop{ .get_iyh, .set_iyl }, // ld iyl,iyh
        &[_]Uop{}, // ld iyl,iyl
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .set_l }, // ld l,(iy+d)
        &[_]Uop{ .get_a, .set_iyl }, // ld iyl,a
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_b, .write_byte }, // ld (iy+d),b
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_c, .write_byte }, // ld (iy+d),c
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_d, .write_byte }, // ld (iy+d),d
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_e, .write_byte }, // ld (iy+d),e
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_h, .write_byte }, // ld (iy+d),h
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_l, .write_byte }, // ld (iy+d),l
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_a, .write_byte }, // ld (iy+d),a
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_iyh, .set_a }, // ld a,iyh
        &[_]Uop{ .get_iyl, .set_a }, // ld a,iyl
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .set_a }, // ld a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_iyh, .save, .get_a, .add_bytes, .set_a }, // add a,iyh
        &[_]Uop{ .get_iyl, .save, .get_a, .add_bytes, .set_a }, // add a,iyl
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .add_bytes, .set_a }, // add a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_iyh, .save, .get_a, .adc_bytes, .set_a }, // adc a,iyh
        &[_]Uop{ .get_iyl, .save, .get_a, .adc_bytes, .set_a }, // adc a,iyl
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .adc_bytes, .set_a }, // adc a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_iyh, .save, .get_a, .sub_bytes, .set_a }, // sub a,iyh
        &[_]Uop{ .get_iyl, .save, .get_a, .sub_bytes, .set_a }, // sub a,iyl
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sub_bytes, .set_a }, // sub a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_iyh, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,iyh
        &[_]Uop{ .get_iyl, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,iyl
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_iyh, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,iyh
        &[_]Uop{ .get_iyl, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,iyl
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_iyh, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,iyh
        &[_]Uop{ .get_iyl, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,iyl
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_iyh, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,iyh
        &[_]Uop{ .get_iyl, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,iyl
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_iyh, .save, .get_a, .sub_bytes }, // cp a,iyh
        &[_]Uop{ .get_iyl, .save, .get_a, .sub_bytes }, // cp a,iyl
        &[_]Uop{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sub_bytes }, // cp a,(iy+d)
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
        &[_]Uop{ .get_sp, .mask_addr_inst, .read_word, .set_iy, .inc_addr, .mask_addr_inst, .set_sp }, // pop iy
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_sp, .mask_addr_inst, .read_word, .ex_iy_inst, .write_word_rev }, // ex (sp),iy
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .get_sp, .dec_addr, .mask_addr_inst, .get_iy, .write_word_rev, .set_sp }, // push iy
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .get_iy, .mask_word_inst, .set_pc, .set_adl_inst }, // jp (iy)
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
        &[_]Uop{ .get_iy, .save, .set_sp }, // ld sp,iy
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
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rlc_byte, .pzs_byte, .write_byte }, // rlc (iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rrc_byte, .pzs_byte, .write_byte }, // rrc (iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rl_byte, .pzs_byte, .write_byte }, // rl (iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rr_byte, .pzs_byte, .write_byte }, // rr (iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sla_byte, .pzs_byte, .write_byte }, // sla (iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sra_byte, .pzs_byte, .write_byte }, // sra (iy+d)
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
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .srl_byte, .pzs_byte, .write_byte }, // srl (iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_0_byte, .pzs_byte }, // bit 0,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_1_byte, .pzs_byte }, // bit 1,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_2_byte, .pzs_byte }, // bit 2,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_3_byte, .pzs_byte }, // bit 3,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_4_byte, .pzs_byte }, // bit 4,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_5_byte, .pzs_byte }, // bit 5,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_6_byte, .pzs_byte }, // bit 6,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_7_byte, .pzs_byte }, // bit 7,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_0_byte, .write_byte }, // res 0,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_1_byte, .write_byte }, // res 1,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_2_byte, .write_byte }, // res 2,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_3_byte, .write_byte }, // res 3,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_4_byte, .write_byte }, // res 4,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_5_byte, .write_byte }, // res 5,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_6_byte, .write_byte }, // res 6,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_7_byte, .write_byte }, // res 7,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_0_byte, .write_byte }, // set 0,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_1_byte, .write_byte }, // set 1,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_2_byte, .write_byte }, // set 2,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_3_byte, .write_byte }, // set 3,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_4_byte, .write_byte }, // set 4,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_5_byte, .write_byte }, // set 5,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_6_byte, .write_byte }, // set 6,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &[_]Uop{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_7_byte, .write_byte }, // set 7,(iy+d)
        &[_]Uop{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
    };
};

pub fn Decoder(comptime T: type) type {
    return struct {
        const Error = switch (@typeInfo(T)) {
            .Struct => T,
            .Pointer => |pointer| switch (pointer.size) {
                .One => pointer.child,
                else => unreachable,
            },
            else => unreachable,
        }.Error;
        const DispatchError = error{RepeatInstruction} || Error;

        pub fn decode(impl: T) Error!void {
            try dispatchAll(impl, &[_]Uop{ .add_r_1, .fetch_byte, .dispatch_base });
        }

        fn dispatcherFor(comptime table: *const [1 << 8][]const Uop) fn (T, comptime u8) Error!void {
            return struct {
                fn dispatcher(impl: T, comptime opcode: u8) Error!void {
                    try dispatchAll(impl, table[opcode]);
                }
            }.dispatcher;
        }

        fn dispatchAll(impl: T, comptime uops: []const Uop) Error!void {
            repeat: while (true) {
                inline for (uops) |uop| dispatch(impl, uop) catch |err| switch (err) {
                    DispatchError.RepeatInstruction => continue :repeat,
                    else => |e| return e,
                };
                return;
            }
        }

        fn dispatch(impl: T, comptime uop: Uop) DispatchError!void {
            try switch (uop) {
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
                    break :err impl.setShadowRegister(.adl);
                },
                .clear_ief, .set_ief => err: {
                    try impl.set(@boolToInt(uop == .set_ief));
                    try impl.setRegister(.ief);
                    break :err impl.setShadowRegister(.ief);
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

                .get_b => impl.getRegister(.b),
                .get_c => impl.getRegister(.c),
                .get_d => impl.getRegister(.d),
                .get_e => impl.getRegister(.e),
                .get_h => impl.getRegister(.h),
                .get_l => impl.getRegister(.l),
                .get_a => impl.getRegister(.a),
                .get_a_high => impl.getRegisterHigh(.a),
                .get_ixh => impl.getRegister(.ixh),
                .get_ixl => impl.getRegister(.ixl),
                .get_iyh => impl.getRegister(.iyh),
                .get_iyl => impl.getRegister(.iyl),
                .get_i => err: {
                    try impl.getRegister(.i);
                    break :err impl.getRegisterFlags();
                },
                .get_mbi => err: {
                    try impl.getRegister(.mbaseui);
                    break :err impl.zeroFlags();
                },
                .get_r => err: {
                    try impl.getRegister(.r);
                    break :err impl.getRegisterFlags();
                },
                .get_mb => impl.getRegister(.mbase),
                .get_af => impl.getRegister(.af),
                .get_bc => impl.getRegister(.ubc),
                .get_de => impl.getRegister(.ude),
                .get_hl => impl.getRegister(.uhl),
                .get_ix => impl.getRegister(.uix),
                .get_iy => impl.getRegister(.uiy),
                .get_sp => impl.getStackPointerInstruction(),
                .get_pc => impl.getShadowRegister(.pc),

                .set_b => impl.setRegister(.b),
                .set_c => impl.setRegister(.c),
                .set_d => impl.setRegister(.d),
                .set_e => impl.setRegister(.e),
                .set_h => impl.setRegister(.h),
                .set_l => impl.setRegister(.l),
                .set_a => impl.setRegister(.a),
                .set_a_high => impl.setRegisterHigh(.a),
                .set_ixh => impl.setRegister(.ixh),
                .set_ixl => impl.setRegister(.ixl),
                .set_iyh => impl.setRegister(.iyh),
                .set_iyl => impl.setRegister(.iyl),
                .set_i => err: {
                    try impl.truncate(u16);
                    break :err impl.setRegister(.ui);
                },
                .set_r => impl.setRegister(.r),
                .set_mb => impl.setRegister(.mbase),
                .set_af => err: {
                    try impl.truncate(u16);
                    break :err impl.setRegister(.af);
                },
                .set_bc => impl.setRegister(.ubc),
                .set_bc_inst => impl.setRegisterInstruction(.bc, .ubc),
                .set_de => impl.setRegister(.ude),
                .set_hl => impl.setRegister(.uhl),
                .set_ix => impl.setRegister(.uix),
                .set_iy => impl.setRegister(.uiy),
                .set_sp => impl.setStackPointerInstruction(),
                .set_pc => impl.setShadowRegister(.pc),

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
            };
        }
    };
}
