const std = @import("std");

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
        &.{}, // nop
        &.{ .fetch_word, .mask_word_inst, .set_bc }, // ld bc,nn
        &.{ .get_bc, .save, .mask_addr_inst, .get_a, .write_byte }, // ld (bc),a
        &.{ .get_bc, .inc_word, .mask_word_inst, .set_bc }, // inc bc
        &.{ .get_b, .inc_byte, .set_b }, // inc b
        &.{ .get_b, .dec_byte, .set_b }, // dec b
        &.{ .fetch_byte, .set_b }, // ld b,n
        &.{ .get_a, .rlc_byte, .set_a }, // rlca
        &.{ .get_af, .@"ex_af'", .set_af }, // ex af,af'
        &.{ .get_bc, .save, .get_hl, .add_words, .set_hl }, // add hl,bc
        &.{ .get_bc, .save, .mask_addr_inst, .read_byte, .set_a }, // ld a,(bc)
        &.{ .get_bc, .dec_word, .mask_word_inst, .set_bc }, // dec bc
        &.{ .get_c, .inc_byte, .set_c }, // inc c
        &.{ .get_c, .dec_byte, .set_c }, // dec c
        &.{ .fetch_byte, .set_c }, // ld c,n
        &.{ .get_a, .rrc_byte, .set_a }, // rrca
        &.{ .fetch_byte, .save, .get_b, .sub_byte_1, .set_b, .non_zero, .flush, .add_cc_1, .get_pc, .dec_word, .add_offset, .mask_word_inst, .set_pc }, // djnz d
        &.{ .fetch_word, .mask_word_inst, .set_de }, // ld de,nn
        &.{ .get_de, .save, .mask_addr_inst, .get_a, .write_byte }, // ld (de),a
        &.{ .get_de, .inc_word, .mask_word_inst, .set_de }, // inc de
        &.{ .get_d, .inc_byte, .set_d }, // inc d
        &.{ .get_d, .dec_byte, .set_d }, // dec d
        &.{ .fetch_byte, .set_d }, // ld d,n
        &.{ .get_a, .rl_byte, .set_a }, // rla
        &.{ .fetch_byte, .save, .flush, .get_pc, .dec_word, .add_offset, .mask_word_inst, .set_pc }, // jr d
        &.{ .get_de, .save, .get_hl, .add_words, .set_hl }, // add hl,de
        &.{ .get_de, .save, .mask_addr_inst, .read_byte, .set_a }, // ld a,(de)
        &.{ .get_de, .dec_word, .mask_word_inst, .set_de }, // dec de
        &.{ .get_e, .inc_byte, .set_e }, // inc e
        &.{ .get_e, .dec_byte, .set_e }, // dec e
        &.{ .fetch_byte, .set_e }, // ld e,n
        &.{ .get_a, .rr_byte, .set_a }, // rra
        &.{ .fetch_byte, .nz, .flush, .add_cc_1, .save, .get_pc, .dec_word, .add_offset, .mask_word_inst, .set_pc }, // jr nz,d
        &.{ .fetch_word, .mask_word_inst, .set_hl }, // ld hl,nn
        &.{ .fetch_word, .save, .mask_addr_inst, .get_hl, .write_word }, // ld (nn),hl
        &.{ .get_hl, .inc_word, .mask_word_inst, .set_hl }, // inc hl
        &.{ .get_h, .inc_byte, .set_h }, // inc h
        &.{ .get_h, .dec_byte, .set_h }, // dec h
        &.{ .fetch_byte, .set_h }, // ld h,n
        &.{ .get_a, .daa_byte, .pzs_byte, .set_a }, // daa
        &.{ .fetch_byte, .z, .flush, .add_cc_1, .save, .get_pc, .dec_word, .add_offset, .mask_word_inst, .set_pc }, // jr z,d
        &.{ .get_hl, .save, .add_words, .set_hl }, // add hl,hl
        &.{ .fetch_word, .save, .mask_addr_inst, .read_word, .set_hl }, // ld hl,(nn)
        &.{ .get_hl, .dec_word, .mask_word_inst, .set_hl }, // dec hl
        &.{ .get_l, .inc_byte, .set_l }, // inc l
        &.{ .get_l, .dec_byte, .set_l }, // dec l
        &.{ .fetch_byte, .set_l }, // ld l,n
        &.{ .get_a, .cpl_byte, .set_a }, // cpl
        &.{ .fetch_byte, .nc, .flush, .add_cc_1, .save, .get_pc, .dec_word, .add_offset, .mask_word_inst, .set_pc }, // jr nc,d
        &.{ .fetch_word, .save, .set_sp }, // ld sp,nn
        &.{ .fetch_word, .save, .mask_addr_inst, .get_a, .write_byte }, // ld (nn),a
        &.{ .get_sp, .inc_addr, .set_sp }, // inc sp
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .inc_byte, .write_byte }, // inc (hl)
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .dec_byte, .write_byte }, // dec (hl)
        &.{ .get_hl, .save, .mask_addr_inst, .fetch_byte, .write_byte }, // ld (hl),n
        &.{.scf}, // scf
        &.{ .fetch_byte, .c, .flush, .save, .add_cc_1, .get_pc, .dec_word, .add_offset, .mask_word_inst, .set_pc }, // jr c,d
        &.{ .get_sp, .get_hl, .add_words, .set_hl }, // add hl,sp
        &.{ .fetch_word, .save, .mask_addr_inst, .read_byte, .set_a }, // ld a,(nn)
        &.{ .get_sp, .dec_addr, .set_sp }, // dec sp
        &.{ .get_a, .inc_byte, .set_a }, // inc a
        &.{ .get_a, .dec_byte, .set_a }, // dec a
        &.{ .fetch_byte, .set_a }, // ld a,n
        &.{.ccf}, // ccf
        &.{ .mode_sis, .add_r_1, .fetch_byte, .dispatch_base }, // .sis
        &.{ .get_c, .set_b }, // ld b,c
        &.{ .get_d, .set_b }, // ld b,d
        &.{ .get_e, .set_b }, // ld b,e
        &.{ .get_h, .set_b }, // ld b,h
        &.{ .get_l, .set_b }, // ld b,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .set_b }, // ld b,(hl)
        &.{ .get_a, .set_b }, // ld b,a
        &.{ .get_b, .set_c }, // ld c,b
        &.{ .mode_lis, .add_r_1, .fetch_byte, .dispatch_base }, // .lis
        &.{ .get_d, .set_c }, // ld c,d
        &.{ .get_e, .set_c }, // ld c,e
        &.{ .get_h, .set_c }, // ld c,h
        &.{ .get_l, .set_c }, // ld c,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .set_c }, // ld c,(hl)
        &.{ .get_a, .set_c }, // ld c,a
        &.{ .get_b, .set_d }, // ld d,b
        &.{ .get_c, .set_d }, // ld d,c
        &.{ .mode_sil, .add_r_1, .fetch_byte, .dispatch_base }, // .sil
        &.{ .get_e, .set_d }, // ld d,e
        &.{ .get_h, .set_d }, // ld d,h
        &.{ .get_l, .set_d }, // ld d,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .set_d }, // ld d,(hl)
        &.{ .get_a, .set_d }, // ld d,a
        &.{ .get_b, .set_e }, // ld e,b
        &.{ .get_c, .set_e }, // ld e,c
        &.{ .get_d, .set_e }, // ld e,d
        &.{ .mode_lil, .add_r_1, .fetch_byte, .dispatch_base }, // .lil
        &.{ .get_h, .set_e }, // ld e,h
        &.{ .get_l, .set_e }, // ld e,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .set_e }, // ld e,(hl)
        &.{ .get_a, .set_e }, // ld e,a
        &.{ .get_b, .set_h }, // ld h,b
        &.{ .get_c, .set_h }, // ld h,c
        &.{ .get_d, .set_h }, // ld h,d
        &.{ .get_e, .set_h }, // ld h,e
        &.{}, // ld h,h
        &.{ .get_l, .set_h }, // ld h,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .set_h }, // ld h,(hl)
        &.{ .get_a, .set_h }, // ld h,a
        &.{ .get_b, .set_l }, // ld l,b
        &.{ .get_c, .set_l }, // ld l,c
        &.{ .get_d, .set_l }, // ld l,d
        &.{ .get_e, .set_l }, // ld l,e
        &.{ .get_h, .set_l }, // ld l,h
        &.{}, // ld l,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .set_l }, // ld l,(hl)
        &.{ .get_a, .set_l }, // ld a,l
        &.{ .get_hl, .save, .mask_addr_inst, .get_b, .write_byte }, // ld (hl),b
        &.{ .get_hl, .save, .mask_addr_inst, .get_c, .write_byte }, // ld (hl),c
        &.{ .get_hl, .save, .mask_addr_inst, .get_d, .write_byte }, // ld (hl),d
        &.{ .get_hl, .save, .mask_addr_inst, .get_e, .write_byte }, // ld (hl),e
        &.{ .get_hl, .save, .mask_addr_inst, .get_h, .write_byte }, // ld (hl),h
        &.{ .get_hl, .save, .mask_addr_inst, .get_l, .write_byte }, // ld (hl),l
        &.{.halt}, // halt
        &.{ .get_hl, .save, .mask_addr_inst, .get_a, .write_byte }, // ld (hl),a
        &.{ .get_b, .set_a }, // ld a,b
        &.{ .get_c, .set_a }, // ld a,c
        &.{ .get_d, .set_a }, // ld a,d
        &.{ .get_e, .set_a }, // ld a,e
        &.{ .get_h, .set_a }, // ld a,h
        &.{ .get_l, .set_a }, // ld a,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .set_a }, // ld a,(hl)
        &.{}, // ld a,a
        &.{ .get_b, .save, .get_a, .add_bytes, .set_a }, // add a,b
        &.{ .get_c, .save, .get_a, .add_bytes, .set_a }, // add a,c
        &.{ .get_d, .save, .get_a, .add_bytes, .set_a }, // add a,d
        &.{ .get_e, .save, .get_a, .add_bytes, .set_a }, // add a,e
        &.{ .get_h, .save, .get_a, .add_bytes, .set_a }, // add a,h
        &.{ .get_l, .save, .get_a, .add_bytes, .set_a }, // add a,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .add_bytes, .set_a }, // add a,(hl)
        &.{ .get_a, .save, .add_bytes, .set_a }, // add a,a
        &.{ .get_b, .save, .get_a, .adc_bytes, .set_a }, // adc a,b
        &.{ .get_c, .save, .get_a, .adc_bytes, .set_a }, // adc a,c
        &.{ .get_d, .save, .get_a, .adc_bytes, .set_a }, // adc a,d
        &.{ .get_e, .save, .get_a, .adc_bytes, .set_a }, // adc a,e
        &.{ .get_h, .save, .get_a, .adc_bytes, .set_a }, // adc a,h
        &.{ .get_l, .save, .get_a, .adc_bytes, .set_a }, // adc a,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .adc_bytes, .set_a }, // adc a,(hl)
        &.{ .get_a, .save, .adc_bytes, .set_a }, // adc a,a
        &.{ .get_b, .save, .get_a, .sub_bytes, .set_a }, // sub a,b
        &.{ .get_c, .save, .get_a, .sub_bytes, .set_a }, // sub a,c
        &.{ .get_d, .save, .get_a, .sub_bytes, .set_a }, // sub a,d
        &.{ .get_e, .save, .get_a, .sub_bytes, .set_a }, // sub a,e
        &.{ .get_h, .save, .get_a, .sub_bytes, .set_a }, // sub a,h
        &.{ .get_l, .save, .get_a, .sub_bytes, .set_a }, // sub a,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sub_bytes, .set_a }, // sub a,(hl)
        &.{ .get_a, .save, .sub_bytes, .set_a }, // sub a,a
        &.{ .get_b, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,b
        &.{ .get_c, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,c
        &.{ .get_d, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,d
        &.{ .get_e, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,e
        &.{ .get_h, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,h
        &.{ .get_l, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,(hl)
        &.{ .get_a, .save, .sbc_bytes, .set_a }, // sbc a,a
        &.{ .get_b, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,b
        &.{ .get_c, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,c
        &.{ .get_d, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,d
        &.{ .get_e, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,e
        &.{ .get_h, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,h
        &.{ .get_l, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,(hl)
        &.{ .get_a, .save, .and_bytes, .pzs_byte, .set_a }, // and a,a
        &.{ .get_b, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,b
        &.{ .get_c, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,c
        &.{ .get_d, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,d
        &.{ .get_e, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,e
        &.{ .get_h, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,h
        &.{ .get_l, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,(hl)
        &.{ .get_a, .save, .xor_bytes, .pzs_byte, .set_a }, // xor a,a
        &.{ .get_b, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,b
        &.{ .get_c, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,c
        &.{ .get_d, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,d
        &.{ .get_e, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,e
        &.{ .get_h, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,h
        &.{ .get_l, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,(hl)
        &.{ .get_a, .save, .or_bytes, .pzs_byte, .set_a }, // or a,a
        &.{ .get_b, .save, .get_a, .sub_bytes }, // cp a,b
        &.{ .get_c, .save, .get_a, .sub_bytes }, // cp a,c
        &.{ .get_d, .save, .get_a, .sub_bytes }, // cp a,d
        &.{ .get_e, .save, .get_a, .sub_bytes }, // cp a,e
        &.{ .get_h, .save, .get_a, .sub_bytes }, // cp a,h
        &.{ .get_l, .save, .get_a, .sub_bytes }, // cp a,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sub_bytes }, // cp a,(hl)
        &.{ .get_a, .save, .sub_bytes }, // cp a,a
        &.{ .add_cc_1, .nz, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret nz
        &.{ .get_sp, .mask_addr_inst, .read_word, .set_bc, .inc_addr, .mask_addr_inst, .set_sp }, // pop bc
        &.{ .fetch_word_flush, .nz, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp nz,nn
        &.{ .fetch_word_flush, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp nn
        &.{ .fetch_word_flush, .nz, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call nz,nn
        &.{ .add_r_inst, .get_sp, .dec_addr, .mask_addr_inst, .get_bc, .write_word_rev, .set_sp }, // push bc
        &.{ .fetch_byte, .save, .get_a, .add_bytes, .set_a }, // add a,n
        &.{ .flush, .add_cc_1, .set_00h, .ex_pc, .rst, .set_adl_inst }, // rst 00h
        &.{ .add_cc_1, .z, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret z
        &.{ .flush, .ret, .set_pc, .add_cc_1 }, // ret
        &.{ .fetch_word_flush, .z, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp z,nn
        &.{ .add_r_1, .fetch_byte, .dispatch_cb }, // CB
        &.{ .fetch_word_flush, .z, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call z,nn
        &.{ .fetch_word_flush, .ex_pc, .call, .set_adl_imm }, // call nn
        &.{ .fetch_byte, .save, .get_a, .adc_bytes, .set_a }, // adc a,n
        &.{ .flush, .add_cc_1, .set_08h, .ex_pc, .rst, .set_adl_inst }, // rst 08h
        &.{ .add_cc_1, .nc, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret nc
        &.{ .get_sp, .mask_addr_inst, .read_word, .set_de, .inc_addr, .mask_addr_inst, .set_sp }, // pop de
        &.{ .fetch_word_flush, .nc, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp nc,nn
        &.{ .fetch_byte, .get_a_high, .save, .get_a, .out }, // out (n),a
        &.{ .fetch_word_flush, .nc, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call nc,nn
        &.{ .add_r_inst, .get_sp, .dec_addr, .mask_addr_inst, .get_de, .write_word_rev, .set_sp }, // push de
        &.{ .fetch_byte, .save, .get_a, .sub_bytes, .set_a }, // sub a,n
        &.{ .flush, .add_cc_1, .set_10h, .ex_pc, .rst, .set_adl_inst }, // rst 10h
        &.{ .add_cc_1, .c, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret c
        &.{ .get_bc, .@"ex_bc'", .set_bc, .get_de, .@"ex_de'", .set_de, .get_hl, .@"ex_hl'", .set_hl }, // exx
        &.{ .fetch_word_flush, .c, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp c,nn
        &.{ .fetch_byte, .get_a_high, .save, .in, .set_a }, // in a,(n)
        &.{ .fetch_word_flush, .c, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call c,nn
        &.{ .add_r_1, .fetch_byte, .dispatch_dd }, // DD
        &.{ .fetch_byte, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,n
        &.{ .flush, .add_cc_1, .set_18h, .ex_pc, .rst, .set_adl_inst }, // rst 18h
        &.{ .add_cc_1, .po, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret po
        &.{ .get_sp, .mask_addr_inst, .read_word, .set_hl, .inc_addr, .mask_addr_inst, .set_sp }, // pop hl
        &.{ .fetch_word_flush, .po, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp po,nn
        &.{ .get_sp, .mask_addr_inst, .read_word, .ex_hl_inst, .write_word_rev }, // ex (sp),hl
        &.{ .fetch_word_flush, .po, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call po,nn
        &.{ .add_r_inst, .get_sp, .dec_addr, .mask_addr_inst, .get_hl, .write_word_rev, .set_sp }, // push hl
        &.{ .fetch_byte, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,n
        &.{ .flush, .add_cc_1, .set_20h, .ex_pc, .rst, .set_adl_inst }, // rst 20h
        &.{ .add_cc_1, .pe, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret pe
        &.{ .flush, .fetch_byte, .get_hl, .mask_word_inst, .set_pc, .set_adl_inst }, // jp (hl)
        &.{ .fetch_word_flush, .pe, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp pe,nn
        &.{ .get_de, .mask_word_inst, .ex_hl, .mask_word_inst, .set_de }, // ex de,hl
        &.{ .fetch_word_flush, .pe, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call pe,nn
        &.{ .add_r_1, .fetch_byte, .dispatch_ed }, // ED
        &.{ .fetch_byte, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,n
        &.{ .flush, .add_cc_1, .set_28h, .ex_pc, .rst, .set_adl_inst }, // rst 28h
        &.{ .add_cc_1, .p, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret p
        &.{ .get_sp, .mask_addr_inst, .read_word, .set_af, .inc_addr, .mask_addr_inst, .set_sp }, // pop af
        &.{ .fetch_word_flush, .p, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp p,nn
        &.{.clear_ief}, // di
        &.{ .fetch_word_flush, .p, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call p,nn
        &.{ .add_r_inst, .get_sp, .dec_addr, .mask_addr_inst, .get_af, .write_word_rev, .set_sp }, // push af
        &.{ .fetch_byte, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,n
        &.{ .flush, .add_cc_1, .set_30h, .ex_pc, .rst, .set_adl_inst }, // rst 30h
        &.{ .add_cc_1, .m, .flush, .add_r_1, .ret, .set_pc, .add_cc_1 }, // ret m
        &.{ .get_hl, .save, .set_sp }, // ld sp,hl
        &.{ .fetch_word_flush, .m, .add_cc_1, .mask_word_inst, .set_pc, .set_adl_inst }, // jp m,nn
        &.{ .set_ief, .reset, .add_r_1, .fetch_byte, .dispatch_base }, // ei
        &.{ .fetch_word_flush, .m, .add_cc_call, .ex_pc, .call, .set_adl_imm }, // call m,nn
        &.{ .add_r_1, .fetch_byte, .dispatch_fd }, // FD
        &.{ .fetch_byte, .save, .get_a, .sub_bytes }, // cp a,n
        &.{ .flush, .add_cc_1, .set_38h, .ex_pc, .rst, .set_adl_inst }, // rst 38h
    };

    const cb = [_][]const Uop{
        &.{ .get_b, .rlc_byte, .pzs_byte, .set_b }, // rlc b
        &.{ .get_c, .rlc_byte, .pzs_byte, .set_c }, // rlc c
        &.{ .get_d, .rlc_byte, .pzs_byte, .set_d }, // rlc d
        &.{ .get_e, .rlc_byte, .pzs_byte, .set_e }, // rlc e
        &.{ .get_h, .rlc_byte, .pzs_byte, .set_h }, // rlc h
        &.{ .get_l, .rlc_byte, .pzs_byte, .set_l }, // rlc l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rlc_byte, .pzs_byte, .write_byte }, // rlc (hl)
        &.{ .get_a, .rlc_byte, .pzs_byte, .set_a }, // rlc a
        &.{ .get_b, .rrc_byte, .pzs_byte, .set_b }, // rrc b
        &.{ .get_c, .rrc_byte, .pzs_byte, .set_c }, // rrc c
        &.{ .get_d, .rrc_byte, .pzs_byte, .set_d }, // rrc d
        &.{ .get_e, .rrc_byte, .pzs_byte, .set_e }, // rrc e
        &.{ .get_h, .rrc_byte, .pzs_byte, .set_h }, // rrc h
        &.{ .get_l, .rrc_byte, .pzs_byte, .set_l }, // rrc l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rrc_byte, .pzs_byte, .write_byte }, // rrc (hl)
        &.{ .get_a, .rrc_byte, .pzs_byte, .set_a }, // rrc a
        &.{ .get_b, .rl_byte, .pzs_byte, .set_b }, // rl b
        &.{ .get_c, .rl_byte, .pzs_byte, .set_c }, // rl c
        &.{ .get_d, .rl_byte, .pzs_byte, .set_d }, // rl d
        &.{ .get_e, .rl_byte, .pzs_byte, .set_e }, // rl e
        &.{ .get_h, .rl_byte, .pzs_byte, .set_h }, // rl h
        &.{ .get_l, .rl_byte, .pzs_byte, .set_l }, // rl l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rl_byte, .pzs_byte, .write_byte }, // rl (hl)
        &.{ .get_a, .rl_byte, .pzs_byte, .set_a }, // rl a
        &.{ .get_b, .rr_byte, .pzs_byte, .set_b }, // rr b
        &.{ .get_c, .rr_byte, .pzs_byte, .set_c }, // rr c
        &.{ .get_d, .rr_byte, .pzs_byte, .set_d }, // rr d
        &.{ .get_e, .rr_byte, .pzs_byte, .set_e }, // rr e
        &.{ .get_h, .rr_byte, .pzs_byte, .set_h }, // rr h
        &.{ .get_l, .rr_byte, .pzs_byte, .set_l }, // rr l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rr_byte, .pzs_byte, .write_byte }, // rr (hl)
        &.{ .get_a, .rr_byte, .pzs_byte, .set_a }, // rr a
        &.{ .get_b, .sla_byte, .pzs_byte, .set_b }, // sla b
        &.{ .get_c, .sla_byte, .pzs_byte, .set_c }, // sla c
        &.{ .get_d, .sla_byte, .pzs_byte, .set_d }, // sla d
        &.{ .get_e, .sla_byte, .pzs_byte, .set_e }, // sla e
        &.{ .get_h, .sla_byte, .pzs_byte, .set_h }, // sla h
        &.{ .get_l, .sla_byte, .pzs_byte, .set_l }, // sla l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sla_byte, .pzs_byte, .write_byte }, // sla (hl)
        &.{ .get_a, .sla_byte, .pzs_byte, .set_a }, // sla a
        &.{ .get_b, .sra_byte, .pzs_byte, .set_b }, // sra b
        &.{ .get_c, .sra_byte, .pzs_byte, .set_c }, // sra c
        &.{ .get_d, .sra_byte, .pzs_byte, .set_d }, // sra d
        &.{ .get_e, .sra_byte, .pzs_byte, .set_e }, // sra e
        &.{ .get_h, .sra_byte, .pzs_byte, .set_h }, // sra h
        &.{ .get_l, .sra_byte, .pzs_byte, .set_l }, // sra l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sra_byte, .pzs_byte, .write_byte }, // sra (hl)
        &.{ .get_a, .sra_byte, .pzs_byte, .set_a }, // sra a
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_b, .srl_byte, .pzs_byte, .set_b }, // srl b
        &.{ .get_c, .srl_byte, .pzs_byte, .set_c }, // srl c
        &.{ .get_d, .srl_byte, .pzs_byte, .set_d }, // srl d
        &.{ .get_e, .srl_byte, .pzs_byte, .set_e }, // srl e
        &.{ .get_h, .srl_byte, .pzs_byte, .set_h }, // srl h
        &.{ .get_l, .srl_byte, .pzs_byte, .set_l }, // srl l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .srl_byte, .pzs_byte, .write_byte }, // srl (hl)
        &.{ .get_a, .srl_byte, .pzs_byte, .set_a }, // srl a
        &.{ .get_b, .bit_0_byte, .pzs_byte }, // bit 0,b
        &.{ .get_c, .bit_0_byte, .pzs_byte }, // bit 0,c
        &.{ .get_d, .bit_0_byte, .pzs_byte }, // bit 0,d
        &.{ .get_e, .bit_0_byte, .pzs_byte }, // bit 0,e
        &.{ .get_h, .bit_0_byte, .pzs_byte }, // bit 0,h
        &.{ .get_l, .bit_0_byte, .pzs_byte }, // bit 0,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_0_byte, .pzs_byte }, // bit 0,(hl)
        &.{ .get_a, .bit_0_byte, .pzs_byte }, // bit 0,a
        &.{ .get_b, .bit_1_byte, .pzs_byte }, // bit 1,b
        &.{ .get_c, .bit_1_byte, .pzs_byte }, // bit 1,c
        &.{ .get_d, .bit_1_byte, .pzs_byte }, // bit 1,d
        &.{ .get_e, .bit_1_byte, .pzs_byte }, // bit 1,e
        &.{ .get_h, .bit_1_byte, .pzs_byte }, // bit 1,h
        &.{ .get_l, .bit_1_byte, .pzs_byte }, // bit 1,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_1_byte, .pzs_byte }, // bit 1,(hl)
        &.{ .get_a, .bit_1_byte, .pzs_byte }, // bit 1,a
        &.{ .get_b, .bit_2_byte, .pzs_byte }, // bit 2,b
        &.{ .get_c, .bit_2_byte, .pzs_byte }, // bit 2,c
        &.{ .get_d, .bit_2_byte, .pzs_byte }, // bit 2,d
        &.{ .get_e, .bit_2_byte, .pzs_byte }, // bit 2,e
        &.{ .get_h, .bit_2_byte, .pzs_byte }, // bit 2,h
        &.{ .get_l, .bit_2_byte, .pzs_byte }, // bit 2,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_2_byte, .pzs_byte }, // bit 2,(hl)
        &.{ .get_a, .bit_2_byte, .pzs_byte }, // bit 2,a
        &.{ .get_b, .bit_3_byte, .pzs_byte }, // bit 3,b
        &.{ .get_c, .bit_3_byte, .pzs_byte }, // bit 3,c
        &.{ .get_d, .bit_3_byte, .pzs_byte }, // bit 3,d
        &.{ .get_e, .bit_3_byte, .pzs_byte }, // bit 3,e
        &.{ .get_h, .bit_3_byte, .pzs_byte }, // bit 3,h
        &.{ .get_l, .bit_3_byte, .pzs_byte }, // bit 3,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_3_byte, .pzs_byte }, // bit 3,(hl)
        &.{ .get_a, .bit_3_byte, .pzs_byte }, // bit 3,a
        &.{ .get_b, .bit_4_byte, .pzs_byte }, // bit 4,b
        &.{ .get_c, .bit_4_byte, .pzs_byte }, // bit 4,c
        &.{ .get_d, .bit_4_byte, .pzs_byte }, // bit 4,d
        &.{ .get_e, .bit_4_byte, .pzs_byte }, // bit 4,e
        &.{ .get_h, .bit_4_byte, .pzs_byte }, // bit 4,h
        &.{ .get_l, .bit_4_byte, .pzs_byte }, // bit 4,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_4_byte, .pzs_byte }, // bit 4,(hl)
        &.{ .get_a, .bit_4_byte, .pzs_byte }, // bit 4,a
        &.{ .get_b, .bit_5_byte, .pzs_byte }, // bit 5,b
        &.{ .get_c, .bit_5_byte, .pzs_byte }, // bit 5,c
        &.{ .get_d, .bit_5_byte, .pzs_byte }, // bit 5,d
        &.{ .get_e, .bit_5_byte, .pzs_byte }, // bit 5,e
        &.{ .get_h, .bit_5_byte, .pzs_byte }, // bit 5,h
        &.{ .get_l, .bit_5_byte, .pzs_byte }, // bit 5,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_5_byte, .pzs_byte }, // bit 5,(hl)
        &.{ .get_a, .bit_5_byte, .pzs_byte }, // bit 5,a
        &.{ .get_b, .bit_6_byte, .pzs_byte }, // bit 6,b
        &.{ .get_c, .bit_6_byte, .pzs_byte }, // bit 6,c
        &.{ .get_d, .bit_6_byte, .pzs_byte }, // bit 6,d
        &.{ .get_e, .bit_6_byte, .pzs_byte }, // bit 6,e
        &.{ .get_h, .bit_6_byte, .pzs_byte }, // bit 6,h
        &.{ .get_l, .bit_6_byte, .pzs_byte }, // bit 6,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_6_byte, .pzs_byte }, // bit 6,(hl)
        &.{ .get_a, .bit_6_byte, .pzs_byte }, // bit 6,a
        &.{ .get_b, .bit_7_byte, .pzs_byte }, // bit 7,b
        &.{ .get_c, .bit_7_byte, .pzs_byte }, // bit 7,c
        &.{ .get_d, .bit_7_byte, .pzs_byte }, // bit 7,d
        &.{ .get_e, .bit_7_byte, .pzs_byte }, // bit 7,e
        &.{ .get_h, .bit_7_byte, .pzs_byte }, // bit 7,h
        &.{ .get_l, .bit_7_byte, .pzs_byte }, // bit 7,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .bit_7_byte, .pzs_byte }, // bit 7,(hl)
        &.{ .get_a, .bit_7_byte, .pzs_byte }, // bit 7,a
        &.{ .get_b, .res_0_byte, .set_b }, // res 0,b
        &.{ .get_c, .res_0_byte, .set_c }, // res 0,c
        &.{ .get_d, .res_0_byte, .set_d }, // res 0,d
        &.{ .get_e, .res_0_byte, .set_e }, // res 0,e
        &.{ .get_h, .res_0_byte, .set_h }, // res 0,h
        &.{ .get_l, .res_0_byte, .set_l }, // res 0,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_0_byte, .write_byte }, // res 0,(hl)
        &.{ .get_a, .res_0_byte, .set_a }, // res 0,a
        &.{ .get_b, .res_1_byte, .set_b }, // res 1,b
        &.{ .get_c, .res_1_byte, .set_c }, // res 1,c
        &.{ .get_d, .res_1_byte, .set_d }, // res 1,d
        &.{ .get_e, .res_1_byte, .set_e }, // res 1,e
        &.{ .get_h, .res_1_byte, .set_h }, // res 1,h
        &.{ .get_l, .res_1_byte, .set_l }, // res 1,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_1_byte, .write_byte }, // res 1,(hl)
        &.{ .get_a, .res_1_byte, .set_a }, // res 1,a
        &.{ .get_b, .res_2_byte, .set_b }, // res 2,b
        &.{ .get_c, .res_2_byte, .set_c }, // res 2,c
        &.{ .get_d, .res_2_byte, .set_d }, // res 2,d
        &.{ .get_e, .res_2_byte, .set_e }, // res 2,e
        &.{ .get_h, .res_2_byte, .set_h }, // res 2,h
        &.{ .get_l, .res_2_byte, .set_l }, // res 2,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_2_byte, .write_byte }, // res 2,(hl)
        &.{ .get_a, .res_2_byte, .set_a }, // res 2,a
        &.{ .get_b, .res_3_byte, .set_b }, // res 3,b
        &.{ .get_c, .res_3_byte, .set_c }, // res 3,c
        &.{ .get_d, .res_3_byte, .set_d }, // res 3,d
        &.{ .get_e, .res_3_byte, .set_e }, // res 3,e
        &.{ .get_h, .res_3_byte, .set_h }, // res 3,h
        &.{ .get_l, .res_3_byte, .set_l }, // res 3,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_3_byte, .write_byte }, // res 3,(hl)
        &.{ .get_a, .res_3_byte, .set_a }, // res 3,a
        &.{ .get_b, .res_4_byte, .set_b }, // res 4,b
        &.{ .get_c, .res_4_byte, .set_c }, // res 4,c
        &.{ .get_d, .res_4_byte, .set_d }, // res 4,d
        &.{ .get_e, .res_4_byte, .set_e }, // res 4,e
        &.{ .get_h, .res_4_byte, .set_h }, // res 4,h
        &.{ .get_l, .res_4_byte, .set_l }, // res 4,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_4_byte, .write_byte }, // res 4,(hl)
        &.{ .get_a, .res_4_byte, .set_a }, // res 4,a
        &.{ .get_b, .res_5_byte, .set_b }, // res 5,b
        &.{ .get_c, .res_5_byte, .set_c }, // res 5,c
        &.{ .get_d, .res_5_byte, .set_d }, // res 5,d
        &.{ .get_e, .res_5_byte, .set_e }, // res 5,e
        &.{ .get_h, .res_5_byte, .set_h }, // res 5,h
        &.{ .get_l, .res_5_byte, .set_l }, // res 5,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_5_byte, .write_byte }, // res 5,(hl)
        &.{ .get_a, .res_5_byte, .set_a }, // res 5,a
        &.{ .get_b, .res_6_byte, .set_b }, // res 6,b
        &.{ .get_c, .res_6_byte, .set_c }, // res 6,c
        &.{ .get_d, .res_6_byte, .set_d }, // res 6,d
        &.{ .get_e, .res_6_byte, .set_e }, // res 6,e
        &.{ .get_h, .res_6_byte, .set_h }, // res 6,h
        &.{ .get_l, .res_6_byte, .set_l }, // res 6,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_6_byte, .write_byte }, // res 6,(hl)
        &.{ .get_a, .res_6_byte, .set_a }, // res 6,a
        &.{ .get_b, .res_7_byte, .set_b }, // res 7,b
        &.{ .get_c, .res_7_byte, .set_c }, // res 7,c
        &.{ .get_d, .res_7_byte, .set_d }, // res 7,d
        &.{ .get_e, .res_7_byte, .set_e }, // res 7,e
        &.{ .get_h, .res_7_byte, .set_h }, // res 7,h
        &.{ .get_l, .res_7_byte, .set_l }, // res 7,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_7_byte, .write_byte }, // res 7,(hl)
        &.{ .get_a, .res_7_byte, .set_a }, // res 7,a
        &.{ .get_b, .set_0_byte, .set_b }, // set 0,b
        &.{ .get_c, .set_0_byte, .set_c }, // set 0,c
        &.{ .get_d, .set_0_byte, .set_d }, // set 0,d
        &.{ .get_e, .set_0_byte, .set_e }, // set 0,e
        &.{ .get_h, .set_0_byte, .set_h }, // set 0,h
        &.{ .get_l, .set_0_byte, .set_l }, // set 0,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_0_byte, .write_byte }, // set 0,(hl)
        &.{ .get_a, .set_0_byte, .set_a }, // set 0,a
        &.{ .get_b, .set_1_byte, .set_b }, // set 1,b
        &.{ .get_c, .set_1_byte, .set_c }, // set 1,c
        &.{ .get_d, .set_1_byte, .set_d }, // set 1,d
        &.{ .get_e, .set_1_byte, .set_e }, // set 1,e
        &.{ .get_h, .set_1_byte, .set_h }, // set 1,h
        &.{ .get_l, .set_1_byte, .set_l }, // set 1,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_1_byte, .write_byte }, // set 1,(hl)
        &.{ .get_a, .set_1_byte, .set_a }, // set 1,a
        &.{ .get_b, .set_2_byte, .set_b }, // set 2,b
        &.{ .get_c, .set_2_byte, .set_c }, // set 2,c
        &.{ .get_d, .set_2_byte, .set_d }, // set 2,d
        &.{ .get_e, .set_2_byte, .set_e }, // set 2,e
        &.{ .get_h, .set_2_byte, .set_h }, // set 2,h
        &.{ .get_l, .set_2_byte, .set_l }, // set 2,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_2_byte, .write_byte }, // set 2,(hl)
        &.{ .get_a, .set_2_byte, .set_a }, // set 2,a
        &.{ .get_b, .set_3_byte, .set_b }, // set 3,b
        &.{ .get_c, .set_3_byte, .set_c }, // set 3,c
        &.{ .get_d, .set_3_byte, .set_d }, // set 3,d
        &.{ .get_e, .set_3_byte, .set_e }, // set 3,e
        &.{ .get_h, .set_3_byte, .set_h }, // set 3,h
        &.{ .get_l, .set_3_byte, .set_l }, // set 3,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_3_byte, .write_byte }, // set 3,(hl)
        &.{ .get_a, .set_3_byte, .set_a }, // set 3,a
        &.{ .get_b, .set_4_byte, .set_b }, // set 4,b
        &.{ .get_c, .set_4_byte, .set_c }, // set 4,c
        &.{ .get_d, .set_4_byte, .set_d }, // set 4,d
        &.{ .get_e, .set_4_byte, .set_e }, // set 4,e
        &.{ .get_h, .set_4_byte, .set_h }, // set 4,h
        &.{ .get_l, .set_4_byte, .set_l }, // set 4,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_4_byte, .write_byte }, // set 4,(hl)
        &.{ .get_a, .set_4_byte, .set_a }, // set 4,a
        &.{ .get_b, .set_5_byte, .set_b }, // set 5,b
        &.{ .get_c, .set_5_byte, .set_c }, // set 5,c
        &.{ .get_d, .set_5_byte, .set_d }, // set 5,d
        &.{ .get_e, .set_5_byte, .set_e }, // set 5,e
        &.{ .get_h, .set_5_byte, .set_h }, // set 5,h
        &.{ .get_l, .set_5_byte, .set_l }, // set 5,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_5_byte, .write_byte }, // set 5,(hl)
        &.{ .get_a, .set_5_byte, .set_a }, // set 5,a
        &.{ .get_b, .set_6_byte, .set_b }, // set 6,b
        &.{ .get_c, .set_6_byte, .set_c }, // set 6,c
        &.{ .get_d, .set_6_byte, .set_d }, // set 6,d
        &.{ .get_e, .set_6_byte, .set_e }, // set 6,e
        &.{ .get_h, .set_6_byte, .set_h }, // set 6,h
        &.{ .get_l, .set_6_byte, .set_l }, // set 6,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_6_byte, .write_byte }, // set 6,(hl)
        &.{ .get_a, .set_6_byte, .set_a }, // set 6,a
        &.{ .get_b, .set_7_byte, .set_b }, // set 7,b
        &.{ .get_c, .set_7_byte, .set_c }, // set 7,c
        &.{ .get_d, .set_7_byte, .set_d }, // set 7,d
        &.{ .get_e, .set_7_byte, .set_e }, // set 7,e
        &.{ .get_h, .set_7_byte, .set_h }, // set 7,h
        &.{ .get_l, .set_7_byte, .set_l }, // set 7,l
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_7_byte, .write_byte }, // set 7,(hl)
        &.{ .get_a, .set_7_byte, .set_a }, // set 7,a
    };

    const dd = [_][]const Uop{
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_word, .set_bc }, // ld bc,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_bc, .save, .get_ix, .add_words, .set_ix }, // add ix,bc
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_bc, .write_word }, // ld (ix+d),bc
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_word, .set_de }, // ld de,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_de, .save, .get_ix, .add_words, .set_ix }, // add ix,de
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_de, .write_word }, // ld (ix+d),de
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_word, .mask_word_inst, .set_ix }, // ld ix,nn
        &.{ .fetch_word, .save, .mask_addr_inst, .get_ix, .write_word }, // ld (nn),ix
        &.{ .get_ix, .inc_word, .mask_word_inst, .set_ix }, // inc ix
        &.{ .get_ixh, .inc_byte, .set_ixh }, // inc ixh
        &.{ .get_ixh, .dec_byte, .set_ixh }, // dec ixh
        &.{ .fetch_byte, .set_ixh }, // ld ixh,n
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_word, .set_hl }, // ld hl,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_ix, .save, .add_words, .set_ix }, // add ix,ix
        &.{ .fetch_word, .save, .mask_addr_inst, .read_word, .set_ix }, // ld ix,(nn)
        &.{ .get_ix, .dec_word, .mask_word_inst, .set_ix }, // dec ix
        &.{ .get_ixl, .inc_byte, .set_ixl }, // inc ixl
        &.{ .get_ixl, .dec_byte, .set_ixl }, // dec ixl
        &.{ .fetch_byte, .set_ixl }, // ld ixl,n
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_hl, .write_word }, // ld (ix+d),hl
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_word, .set_iy }, // ld iy,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .inc_byte, .write_byte }, // inc (ix+d)
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .dec_byte, .write_byte }, // dec (ix+d)
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .fetch_byte, .write_byte }, // ld (ix+d),n
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_word, .set_ix }, // ld ix,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_sp, .get_ix, .add_words, .set_ix }, // add ix,sp
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_iy, .write_word }, // ld (ix+d),iy
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_ix, .write_word }, // ld (ix+d),ix
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_ixh, .set_b }, // ld b,ixh
        &.{ .get_ixl, .set_b }, // ld b,ixl
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .set_b }, // ld b,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_ixh, .set_c }, // ld c,ixh
        &.{ .get_ixl, .set_c }, // ld c,ixl
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .set_c }, // ld c,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_ixh, .set_d }, // ld d,ixh
        &.{ .get_ixl, .set_d }, // ld d,ixl
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .set_d }, // ld d,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_ixh, .set_e }, // ld e,ixh
        &.{ .get_ixl, .set_e }, // ld e,ixl
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .set_e }, // ld e,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_b, .set_ixh }, // ld ixh,b
        &.{ .get_c, .set_ixh }, // ld ixh,c
        &.{ .get_d, .set_ixh }, // ld ixh,d
        &.{ .get_e, .set_ixh }, // ld ixh,e
        &.{}, // ld ixh,ixh
        &.{ .get_ixl, .set_ixh }, // ld ixh,ixl
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .set_h }, // ld h,(ix+d)
        &.{ .get_a, .set_ixh }, // ld ixh,a
        &.{ .get_b, .set_ixl }, // ld ixl,b
        &.{ .get_c, .set_ixl }, // ld ixl,c
        &.{ .get_d, .set_ixl }, // ld ixl,d
        &.{ .get_e, .set_ixl }, // ld ixl,e
        &.{ .get_ixh, .set_ixl }, // ld ixl,ixh
        &.{}, // ld ixl,ixl
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .set_l }, // ld l,(ix+d)
        &.{ .get_a, .set_ixl }, // ld ixl,a
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_b, .write_byte }, // ld (ix+d),b
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_c, .write_byte }, // ld (ix+d),c
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_d, .write_byte }, // ld (ix+d),d
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_e, .write_byte }, // ld (ix+d),e
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_h, .write_byte }, // ld (ix+d),h
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_l, .write_byte }, // ld (ix+d),l
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .get_a, .write_byte }, // ld (ix+d),a
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_ixh, .set_a }, // ld a,ixh
        &.{ .get_ixl, .set_a }, // ld a,ixl
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .set_a }, // ld a,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_ixh, .save, .get_a, .add_bytes, .set_a }, // add a,ixh
        &.{ .get_ixl, .save, .get_a, .add_bytes, .set_a }, // add a,ixl
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .add_bytes, .set_a }, // add a,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_ixh, .save, .get_a, .adc_bytes, .set_a }, // adc a,ixh
        &.{ .get_ixl, .save, .get_a, .adc_bytes, .set_a }, // adc a,ixl
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .adc_bytes, .set_a }, // adc a,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_ixh, .save, .get_a, .sub_bytes, .set_a }, // sub a,ixh
        &.{ .get_ixl, .save, .get_a, .sub_bytes, .set_a }, // sub a,ixl
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sub_bytes, .set_a }, // sub a,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_ixh, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,ixh
        &.{ .get_ixl, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,ixl
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_ixh, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,ixh
        &.{ .get_ixl, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,ixl
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_ixh, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,ixh
        &.{ .get_ixl, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,ixl
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_ixh, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,ixh
        &.{ .get_ixl, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,ixl
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_ixh, .save, .get_a, .sub_bytes }, // cp a,ixh
        &.{ .get_ixl, .save, .get_a, .sub_bytes }, // cp a,ixl
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sub_bytes }, // cp a,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .fetch_byte, .dispatch_ddcb }, // DDCB
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_sp, .mask_addr_inst, .read_word, .set_ix, .inc_addr, .mask_addr_inst, .set_sp }, // pop ix
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_sp, .mask_addr_inst, .read_word, .ex_ix_inst, .write_word_rev }, // ex (sp),ix
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_sp, .dec_addr, .mask_addr_inst, .get_ix, .write_word_rev, .set_sp }, // push ix
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .get_ix, .mask_word_inst, .set_pc, .set_adl_inst }, // jp (ix)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_ix, .save, .set_sp }, // ld sp,ix
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
    };

    const ddcb = [_][]const Uop{
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rlc_byte, .pzs_byte, .write_byte }, // rlc (ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rrc_byte, .pzs_byte, .write_byte }, // rrc (ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rl_byte, .pzs_byte, .write_byte }, // rl (ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rr_byte, .pzs_byte, .write_byte }, // rr (ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sla_byte, .pzs_byte, .write_byte }, // sla (ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sra_byte, .pzs_byte, .write_byte }, // sra (ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .srl_byte, .pzs_byte, .write_byte }, // srl (ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_0_byte, .pzs_byte }, // bit 0,(ix+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_1_byte, .pzs_byte }, // bit 1,(ix+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_2_byte, .pzs_byte }, // bit 2,(ix+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_3_byte, .pzs_byte }, // bit 3,(ix+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_4_byte, .pzs_byte }, // bit 4,(ix+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_5_byte, .pzs_byte }, // bit 5,(ix+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_6_byte, .pzs_byte }, // bit 6,(ix+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_7_byte, .pzs_byte }, // bit 7,(ix+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_0_byte, .write_byte }, // res 0,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_1_byte, .write_byte }, // res 1,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_2_byte, .write_byte }, // res 2,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_3_byte, .write_byte }, // res 3,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_4_byte, .write_byte }, // res 4,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_5_byte, .write_byte }, // res 5,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_6_byte, .write_byte }, // res 6,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_7_byte, .write_byte }, // res 7,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_0_byte, .write_byte }, // set 0,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_1_byte, .write_byte }, // set 1,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_2_byte, .write_byte }, // set 2,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_3_byte, .write_byte }, // set 3,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_4_byte, .write_byte }, // set 4,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_5_byte, .write_byte }, // set 5,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_6_byte, .write_byte }, // set 6,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_ix, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_7_byte, .write_byte }, // set 7,(ix+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
    };

    const ed = [_][]const Uop{
        &.{ .fetch_byte, .save, .in_f, .pzs_byte, .set_b }, // in0 b,(n)
        &.{ .fetch_byte, .save, .get_b, .out }, // out0 (n),b
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .mask_word_inst, .set_bc }, // lea bc,ix+d
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .mask_word_inst, .set_bc }, // lea bc,iy+d
        &.{ .get_b, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,b
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_hl, .save, .mask_addr_inst, .read_word, .set_bc }, // ld bc,(hl)
        &.{ .fetch_byte, .save, .in_f, .pzs_byte, .set_c }, // in0 c,(n)
        &.{ .fetch_byte, .save, .get_c, .out }, // out0 (n),c
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_c, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,c
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_hl, .save, .mask_addr_inst, .get_bc, .write_word }, // ld (hl),bc
        &.{ .fetch_byte, .save, .in_f, .pzs_byte, .set_d }, // in0 d,(n)
        &.{ .fetch_byte, .save, .get_d, .out }, // out0 (n),d
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .mask_word_inst, .set_de }, // lea de,ix+d
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .mask_word_inst, .set_de }, // lea de,iy+d
        &.{ .get_d, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,d
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_hl, .save, .mask_addr_inst, .read_word, .set_de }, // ld de,(hl)
        &.{ .fetch_byte, .save, .in_f, .pzs_byte, .set_e }, // in0 e,(n)
        &.{ .fetch_byte, .save, .get_e, .out }, // out0 (n),e
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_e, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,e
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_hl, .save, .mask_addr_inst, .get_de, .write_word }, // ld (hl),de
        &.{ .fetch_byte, .save, .in_f, .pzs_byte, .set_h }, // in0 h,(n)
        &.{ .fetch_byte, .save, .get_h, .out }, // out0 (n),h
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .mask_word_inst, .set_hl }, // lea hl,ix+d
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .mask_word_inst, .set_hl }, // lea hl,iy+d
        &.{ .get_h, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,h
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_hl, .save, .mask_addr_inst, .read_word, .set_hl }, // ld hl,(hl)
        &.{ .fetch_byte, .save, .in_f, .pzs_byte, .set_l }, // in0 l,(n)
        &.{ .fetch_byte, .save, .get_l, .out }, // out0 (n),l
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_l, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,l
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_hl, .save, .mask_addr_inst, .write_word }, // ld (hl),hl
        &.{ .fetch_byte, .save, .in_f, .pzs_byte }, // in0 (n)
        &.{ .get_hl, .save, .mask_addr_inst, .read_word, .set_iy }, // ld iy,(hl)
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .mask_word_inst, .set_ix }, // lea ix,ix+d
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .mask_word_inst, .set_iy }, // lea iy,iy+d
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,(hl)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_hl, .save, .mask_addr_inst, .read_word, .set_ix }, // ld ix,(hl)
        &.{ .fetch_byte, .save, .in_f, .pzs_byte, .set_a }, // in0 a,(n)
        &.{ .fetch_byte, .save, .get_a, .out }, // out0 (n),a
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_a, .save, .and_bytes, .pzs_byte }, // tst a,a
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_hl, .save, .mask_addr_inst, .get_iy, .write_word }, // ld (hl),iy
        &.{ .get_hl, .save, .mask_addr_inst, .get_ix, .write_word }, // ld (hl),ix
        &.{ .get_bc, .save, .in_f, .pzs_byte, .set_b }, // in b,(bc)
        &.{ .get_bc, .save, .get_b, .out }, // out (bc),b
        &.{ .get_bc, .save, .get_hl, .sbc_words, .set_hl }, // sbc hl,bc
        &.{ .fetch_word, .save, .mask_addr_inst, .get_bc, .write_word }, // ld (nn),bc
        &.{ .get_a, .save, .set_00h, .sub_bytes, .set_a }, // neg
        &.{ .flush, .ret, .set_pc, .add_cc_1, .copy_ief }, // retn
        &.{.im_0}, // im 0
        &.{ .get_a, .set_i }, // ld i,a
        &.{ .get_bc, .save, .in_f, .pzs_byte, .set_c }, // in c,(bc)
        &.{ .get_bc, .save, .get_c, .out }, // out (bc),c
        &.{ .get_bc, .save, .get_hl, .adc_words, .set_hl }, // adc hl,bc
        &.{ .fetch_word, .save, .mask_addr_inst, .read_word, .set_bc }, // ld bc,(nn)
        &.{ .get_bc, .add_cc_4, .mlt_bytes, .set_bc }, // mlt bc
        &.{ .flush, .ret, .set_pc, .add_cc_1, .copy_ief }, // reti
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_a, .set_r }, // ld r,a
        &.{ .get_bc, .save, .in_f, .pzs_byte, .set_d }, // in d,(bc)
        &.{ .get_bc, .save, .get_d, .out }, // out (bc),d
        &.{ .get_de, .save, .get_hl, .sbc_words, .set_hl }, // sbc hl,de
        &.{ .fetch_word, .save, .mask_addr_inst, .get_de, .write_word }, // ld (nn),de
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .mask_word_inst, .set_ix }, // lea ix,iy+d
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .mask_word_inst, .set_iy }, // lea iy,ix+d
        &.{.im_1}, // im 1
        &.{ .get_i, .set_a }, // ld a,i
        &.{ .get_bc, .save, .in_f, .pzs_byte, .set_e }, // in e,(bc)
        &.{ .get_bc, .save, .get_e, .out }, // out (bc),e
        &.{ .get_de, .save, .get_hl, .adc_words, .set_hl }, // adc hl,de
        &.{ .fetch_word, .save, .mask_addr_inst, .read_word, .set_de }, // ld de,(nn)
        &.{ .get_de, .add_cc_4, .mlt_bytes, .set_de }, // mlt de
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{.im_2}, // im 2
        &.{ .get_r, .set_a }, // ld a,r
        &.{ .get_bc, .save, .in_f, .pzs_byte, .set_h }, // in h,(bc)
        &.{ .get_bc, .save, .get_h, .out }, // out (bc),h
        &.{ .get_hl, .save, .sbc_words, .set_hl }, // sbc hl,hl
        &.{ .fetch_word, .save, .mask_addr_inst, .get_hl, .write_word }, // ld (nn),hl
        &.{ .fetch_byte, .save, .get_a, .and_bytes, .pzs_byte }, // tst a,n
        &.{ .fetch_byte, .save, .get_ix, .add_offset, .get_sp, .dec_addr, .mask_addr_inst, .write_word_rev, .set_sp }, // pea ix+d
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .get_sp, .dec_addr, .mask_addr_inst, .write_word_rev, .set_sp }, // pea iy+d
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .get_a_high, .add_cc_1, .rrd_bytes, .set_a_high, .write_byte }, // rrd
        &.{ .get_bc, .save, .in_f, .pzs_byte, .set_l }, // in l,(bc)
        &.{ .get_bc, .save, .get_l, .out }, // out (bc),l
        &.{ .get_hl, .save, .adc_words, .set_hl }, // adc hl,hl
        &.{ .fetch_word, .save, .mask_addr_inst, .read_word, .set_hl }, // ld hl,(nn)
        &.{ .get_hl, .add_cc_4, .mlt_bytes, .set_hl }, // mlt hl
        &.{ .adl, .get_a, .set_mb }, // ld mb,a
        &.{ .get_mb, .set_a }, // ld a,mb
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .get_a_high, .add_cc_1, .rld_bytes, .set_a_high, .write_byte }, // rld
        &.{ .get_bc, .save, .in_f, .pzs_byte }, // in (bc)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_sp, .get_hl, .sbc_words, .set_hl }, // sbc hl,sp
        &.{ .fetch_word, .get_sp, .swap, .mask_addr_inst, .write_word }, // ld (nn),sp
        &.{ .get_c, .save, .in, .save, .fetch_byte, .swap, .and_bytes, .pzs_byte }, // tstio n
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{.halt}, // slp
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_bc, .save, .in_f, .pzs_byte, .set_a }, // in a,(bc)
        &.{ .get_bc, .save, .get_a, .out }, // out (bc),a
        &.{ .get_sp, .get_hl, .adc_words, .set_hl }, // adc hl,sp
        &.{ .fetch_word, .save, .mask_addr_inst, .read_word, .save, .set_sp }, // ld sp,(nn)
        &.{ .get_sp, .restore, .add_cc_4, .mlt_bytes, .save, .set_sp }, // mlt sp
        &.{.set_madl}, // stmix
        &.{.clear_madl}, // rsmix
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_c, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_hl, .get_c, .add_byte_1, .set_c, .get_b, .dec_byte_hzs, .set_b }, // inim
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_c, .swap, .add_cc_1, .out, .nf_byte_sign, .get_c, .add_byte_1, .set_c, .get_b, .dec_byte_hzs, .set_b }, // otim
        &.{ .get_bc, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_hl, .get_c, .add_byte_1, .set_c, .get_b, .sub_byte_1, .zf_byte, .set_b }, // ini2
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_c, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_hl, .get_c, .sub_byte_1, .set_c, .get_b, .dec_byte_hzs, .set_b }, // indm
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_c, .swap, .add_cc_1, .out, .nf_byte_sign, .get_c, .sub_byte_1, .set_c, .get_b, .dec_byte_hzs, .set_b }, // otdm
        &.{ .get_bc, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_hl, .get_c, .sub_byte_1, .set_c, .get_b, .sub_byte_1, .zf_byte, .set_b }, // ind2
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .sub_r_1, .get_c, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_hl, .get_c, .add_byte_1, .set_c, .get_b, .sub_byte_1, .zf_byte, .set_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // inimr
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_c, .swap, .add_cc_1, .out, .nf_byte_sign, .get_c, .add_byte_1, .set_c, .get_b, .dec_byte_hzs, .set_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // otimr
        &.{ .get_de, .save, .in, .inc_addr, .swap, .mask_word_inst, .set_de, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_hl, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // ini2r
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .sub_r_1, .get_c, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_hl, .get_c, .sub_byte_1, .set_c, .get_b, .sub_byte_1, .zf_byte, .set_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // indmr
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_c, .swap, .add_cc_1, .out, .nf_byte_sign, .get_c, .sub_byte_1, .set_c, .get_b, .dec_byte_hzs, .set_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // otdmr
        &.{ .get_de, .save, .in, .dec_addr, .swap, .mask_word_inst, .set_de, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_hl, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_1, .repeat, .sub_r_1, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // ind2r
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .inc_addr, .restore, .mask_word_inst, .set_de, .ld_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst }, // ldi
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_a, .cp_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst }, // cpi
        &.{ .get_bc, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_hl, .get_b, .sub_byte_1, .zf_byte, .set_b }, // ini
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .get_b, .sub_byte_1, .zf_byte, .set_b }, // outi
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .get_c, .add_byte_1, .set_c, .get_b, .sub_byte_1, .zf_byte, .set_b }, // outi2
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .dec_addr, .restore, .mask_word_inst, .set_de, .ld_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst }, // ldd
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_a, .cp_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst }, // cpd
        &.{ .get_bc, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_hl, .get_b, .sub_byte_1, .zf_byte, .set_b }, // ind
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .get_b, .sub_byte_1, .zf_byte, .set_b }, // outd
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .get_c, .sub_byte_1, .set_c, .get_b, .sub_byte_1, .set_b, .zf_byte }, // outd2
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .inc_addr, .restore, .mask_word_inst, .set_de, .ld_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst, .pe, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // ldir
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_a, .cp_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst, .add_cc_1, .nz, .pe, .add_r_2, .add_cc_1, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // cpir
        &.{ .get_bc, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_hl, .get_b, .sub_byte_1, .zf_byte, .set_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // inir
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .get_b, .sub_byte_1, .zf_byte, .set_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // otir
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .add_cc_1, .out, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_de, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // oti2r
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .dec_addr, .restore, .mask_word_inst, .set_de, .ld_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst, .pe, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // lddr
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_a, .cp_flags, .get_bc, .dec_word, .mask_word_inst, .repeat_flag, .set_bc_inst, .add_cc_1, .nz, .pe, .add_r_2, .add_cc_1, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // cpdr
        &.{ .get_bc, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_hl, .get_b, .sub_byte_1, .zf_byte, .set_b, .nz, .add_r_1, .repeat, .sub_r_1, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // indr
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_bc, .swap, .add_cc_1, .out, .nf_byte_sign, .get_b, .sub_byte_1, .zf_byte, .set_b, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // otdr
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .add_cc_1, .out, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_de, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // otd2r
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_de, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .inc_addr, .restore, .mask_word_inst, .set_hl, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // inirx
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .inc_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .add_cc_1, .out, .nf_byte_sign, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // otirx
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_hl, .set_i }, // ld i,hl
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_de, .save, .in, .save, .get_hl, .swap, .mask_addr_inst, .add_cc_1, .write_byte, .nf_byte_sign, .dec_addr, .restore, .mask_word_inst, .set_hl, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_1, .repeat, .sub_r_1, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // indrx
        &.{ .get_hl, .save, .mask_addr_inst, .read_byte, .dec_addr, .swap, .mask_word_inst, .set_hl, .get_de, .swap, .add_cc_1, .out, .nf_byte_sign, .get_bc, .dec_word, .mask_word_inst, .zf_word, .set_bc_inst, .nz, .add_r_2, .repeat, .sub_r_2, .flush, .get_pc, .sub_word_3, .sub_word_suffix, .set_pc }, // otdrx
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_mbi, .mask_word_inst, .set_hl }, // ld hl,i
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
    };

    const fd = [_][]const Uop{
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_word, .set_bc }, // ld bc,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_bc, .save, .get_iy, .add_words, .set_iy }, // add iy,bc
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_bc, .write_word }, // ld (iy+d),bc
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_word, .set_de }, // ld de,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_de, .save, .get_iy, .add_words, .set_iy }, // add iy,de
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_de, .write_word }, // ld (iy+d),de
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_word, .mask_word_inst, .set_iy }, // ld iy,nn
        &.{ .fetch_word, .save, .mask_addr_inst, .get_iy, .write_word }, // ld (nn),iy
        &.{ .get_iy, .inc_word, .mask_word_inst, .set_iy }, // inc iy
        &.{ .get_iyh, .inc_byte, .set_iyh }, // inc iyh
        &.{ .get_iyh, .dec_byte, .set_iyh }, // dec iyh
        &.{ .fetch_byte, .set_iyh }, // ld iyh,n
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_word, .set_hl }, // ld hl,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_iy, .save, .add_words, .set_iy }, // add iy,iy
        &.{ .fetch_word, .save, .mask_addr_inst, .read_word, .set_iy }, // ld iy,(nn)
        &.{ .get_iy, .dec_word, .mask_word_inst, .set_iy }, // dec iy
        &.{ .get_iyl, .inc_byte, .set_iyl }, // inc iyl
        &.{ .get_iyl, .dec_byte, .set_iyl }, // dec iyl
        &.{ .fetch_byte, .set_iyl }, // ld iyl,n
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_hl, .write_word }, // ld (iy+d),hl
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_word, .set_ix }, // ld ix,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .inc_byte, .write_byte }, // inc (iy+d)
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .dec_byte, .write_byte }, // dec (iy+d)
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .fetch_byte, .write_byte }, // ld (iy+d),n
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_word, .set_iy }, // ld iy,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_sp, .get_iy, .add_words, .set_iy }, // add iy,sp
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_ix, .write_word }, // ld (iy+d),ix
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_iy, .write_word }, // ld (iy+d),iy
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_iyh, .set_b }, // ld b,iyh
        &.{ .get_iyl, .set_b }, // ld b,iyl
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .set_b }, // ld b,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_iyh, .set_c }, // ld c,iyh
        &.{ .get_iyl, .set_c }, // ld c,iyl
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .set_c }, // ld c,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_iyh, .set_d }, // ld d,iyh
        &.{ .get_iyl, .set_d }, // ld d,iyl
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .set_d }, // ld d,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_iyh, .set_e }, // ld e,iyh
        &.{ .get_iyl, .set_e }, // ld e,iyl
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .set_e }, // ld e,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_b, .set_iyh }, // ld iyh,b
        &.{ .get_c, .set_iyh }, // ld iyh,c
        &.{ .get_d, .set_iyh }, // ld iyh,d
        &.{ .get_e, .set_iyh }, // ld iyh,e
        &.{}, // ld iyh,iyh
        &.{ .get_iyl, .set_iyh }, // ld iyh,iyl
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .set_h }, // ld h,(iy+d)
        &.{ .get_a, .set_iyh }, // ld iyh,a
        &.{ .get_b, .set_iyl }, // ld iyl,b
        &.{ .get_c, .set_iyl }, // ld iyl,c
        &.{ .get_d, .set_iyl }, // ld iyl,d
        &.{ .get_e, .set_iyl }, // ld iyl,e
        &.{ .get_iyh, .set_iyl }, // ld iyl,iyh
        &.{}, // ld iyl,iyl
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .set_l }, // ld l,(iy+d)
        &.{ .get_a, .set_iyl }, // ld iyl,a
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_b, .write_byte }, // ld (iy+d),b
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_c, .write_byte }, // ld (iy+d),c
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_d, .write_byte }, // ld (iy+d),d
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_e, .write_byte }, // ld (iy+d),e
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_h, .write_byte }, // ld (iy+d),h
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_l, .write_byte }, // ld (iy+d),l
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .get_a, .write_byte }, // ld (iy+d),a
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_iyh, .set_a }, // ld a,iyh
        &.{ .get_iyl, .set_a }, // ld a,iyl
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .set_a }, // ld a,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_iyh, .save, .get_a, .add_bytes, .set_a }, // add a,iyh
        &.{ .get_iyl, .save, .get_a, .add_bytes, .set_a }, // add a,iyl
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .add_bytes, .set_a }, // add a,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_iyh, .save, .get_a, .adc_bytes, .set_a }, // adc a,iyh
        &.{ .get_iyl, .save, .get_a, .adc_bytes, .set_a }, // adc a,iyl
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .adc_bytes, .set_a }, // adc a,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_iyh, .save, .get_a, .sub_bytes, .set_a }, // sub a,iyh
        &.{ .get_iyl, .save, .get_a, .sub_bytes, .set_a }, // sub a,iyl
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sub_bytes, .set_a }, // sub a,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_iyh, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,iyh
        &.{ .get_iyl, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,iyl
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sbc_bytes, .set_a }, // sbc a,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_iyh, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,iyh
        &.{ .get_iyl, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,iyl
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .and_bytes, .pzs_byte, .set_a }, // and a,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_iyh, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,iyh
        &.{ .get_iyl, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,iyl
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .xor_bytes, .pzs_byte, .set_a }, // xor a,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_iyh, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,iyh
        &.{ .get_iyl, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,iyl
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .or_bytes, .pzs_byte, .set_a }, // or a,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_iyh, .save, .get_a, .sub_bytes }, // cp a,iyh
        &.{ .get_iyl, .save, .get_a, .sub_bytes }, // cp a,iyl
        &.{ .fetch_byte, .save, .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .save, .get_a, .sub_bytes }, // cp a,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .fetch_byte, .save, .fetch_byte, .dispatch_fdcb }, // FDCB
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_sp, .mask_addr_inst, .read_word, .set_iy, .inc_addr, .mask_addr_inst, .set_sp }, // pop iy
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_sp, .mask_addr_inst, .read_word, .ex_iy_inst, .write_word_rev }, // ex (sp),iy
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_sp, .dec_addr, .mask_addr_inst, .get_iy, .write_word_rev, .set_sp }, // push iy
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .get_iy, .mask_word_inst, .set_pc, .set_adl_inst }, // jp (iy)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .get_iy, .save, .set_sp }, // ld sp,iy
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_2, .interrupt }, // trap
    };

    const fdcb = [_][]const Uop{
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rlc_byte, .pzs_byte, .write_byte }, // rlc (iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rrc_byte, .pzs_byte, .write_byte }, // rrc (iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rl_byte, .pzs_byte, .write_byte }, // rl (iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .rr_byte, .pzs_byte, .write_byte }, // rr (iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sla_byte, .pzs_byte, .write_byte }, // sla (iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .sra_byte, .pzs_byte, .write_byte }, // sra (iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .srl_byte, .pzs_byte, .write_byte }, // srl (iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_0_byte, .pzs_byte }, // bit 0,(iy+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_1_byte, .pzs_byte }, // bit 1,(iy+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_2_byte, .pzs_byte }, // bit 2,(iy+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_3_byte, .pzs_byte }, // bit 3,(iy+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_4_byte, .pzs_byte }, // bit 4,(iy+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_5_byte, .pzs_byte }, // bit 5,(iy+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_6_byte, .pzs_byte }, // bit 6,(iy+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .bit_7_byte, .pzs_byte }, // bit 7,(iy+d)
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_0_byte, .write_byte }, // res 0,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_1_byte, .write_byte }, // res 1,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_2_byte, .write_byte }, // res 2,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_3_byte, .write_byte }, // res 3,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_4_byte, .write_byte }, // res 4,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_5_byte, .write_byte }, // res 5,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_6_byte, .write_byte }, // res 6,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .res_7_byte, .write_byte }, // res 7,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_0_byte, .write_byte }, // set 0,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_1_byte, .write_byte }, // set 1,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_2_byte, .write_byte }, // set 2,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_3_byte, .write_byte }, // set 3,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_4_byte, .write_byte }, // set 4,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_5_byte, .write_byte }, // set 5,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_6_byte, .write_byte }, // set 6,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
        &.{ .get_iy, .add_offset, .save, .mask_addr_inst, .read_byte, .add_cc_1, .set_7_byte, .write_byte }, // set 7,(iy+d)
        &.{ .flush, .fetch_byte, .add_cc_1, .set_00h, .ex_pc, .sub_word_3, .interrupt }, // trap
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
            try dispatchAll(impl, &.{ .add_r_1, .fetch_byte, .dispatch_base });
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
