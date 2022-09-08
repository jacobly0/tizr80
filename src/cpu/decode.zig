const std = @import("std");

const CEmuCore = @import("../cemucore.zig");

const Uop = enum {
    unimplemented,

    save,
    restore,
    swap,

    mask_word_inst,
    mask_addr_adl,
    mask_addr_inst,

    fetch_byte,
    fetch_word,

    add_r_1,
    add_cc_1,

    dispatch_base,

    mode_sis,
    mode_lis,
    mode_sil,
    mode_lil,

    halt,

    read_b,
    read_c,
    read_d,
    read_e,
    read_h,
    read_l,
    read_a,
    read_af,
    read_bc,
    read_de,
    read_hl,
    read_sp,
    @"read_af'",
    @"read_bc'",
    @"read_de'",
    @"read_hl'",

    write_b,
    write_c,
    write_d,
    write_e,
    write_h,
    write_l,
    write_a,
    write_af,
    write_bc,
    write_de,
    write_hl,
    write_sp,
    @"write_af'",
    @"write_bc'",
    @"write_de'",
    @"write_hl'",

    load_byte,
    load_word,

    store_byte,
    store_word,

    inc_byte,
    dec_byte,
    inc_word,
    dec_word,
    add_words,
};

const base = [_][]const Uop{
    &[_]Uop{}, // nop
    &[_]Uop{ .fetch_word, .mask_word_inst, .write_bc }, // ld bc,nn
    &[_]Uop{ .read_bc, .save, .mask_addr_inst, .read_a, .store_byte }, // ld (bc),a
    &[_]Uop{ .read_bc, .inc_word, .mask_word_inst, .write_bc }, // inc bc
    &[_]Uop{ .read_b, .inc_byte, .write_b }, // inc b
    &[_]Uop{ .read_b, .dec_byte, .write_b }, // dec b
    &[_]Uop{ .fetch_byte, .write_b }, // ld b,n
    &[_]Uop{.unimplemented}, // rlca
    &[_]Uop{ .read_af, .save, .@"read_af'", .write_af, .restore, .@"write_af'" }, // ex af,af'
    &[_]Uop{ .read_bc, .save, .read_hl, .add_words, .write_hl }, // add hl,bc
    &[_]Uop{ .read_bc, .save, .mask_addr_inst, .load_byte, .write_a }, // ld a,(bc)
    &[_]Uop{ .read_bc, .dec_word, .mask_word_inst, .write_bc }, // dec bc
    &[_]Uop{ .read_c, .inc_byte, .write_c }, // inc c
    &[_]Uop{ .read_c, .dec_byte, .write_c }, // dec c
    &[_]Uop{ .fetch_byte, .write_c }, // ld c,n
    &[_]Uop{.unimplemented}, // rrca
    &[_]Uop{.unimplemented}, // djnz d
    &[_]Uop{ .fetch_word, .mask_word_inst, .write_de }, // ld de,nn
    &[_]Uop{ .read_de, .save, .mask_addr_inst, .read_a, .store_byte }, // ld (de),a
    &[_]Uop{ .read_de, .inc_word, .mask_word_inst, .write_de }, // inc de
    &[_]Uop{ .read_d, .inc_byte, .write_d }, // inc d
    &[_]Uop{ .read_d, .dec_byte, .write_d }, // dec d
    &[_]Uop{ .fetch_byte, .write_d }, // ld d,n
    &[_]Uop{.unimplemented}, // rla
    &[_]Uop{.unimplemented}, // jr d
    &[_]Uop{ .read_de, .save, .read_hl, .add_words, .write_hl }, // add hl,de
    &[_]Uop{ .read_de, .save, .mask_addr_inst, .load_byte, .write_a }, // ld a,(de)
    &[_]Uop{ .read_de, .dec_word, .mask_word_inst, .write_de }, // dec de
    &[_]Uop{ .read_e, .inc_byte, .write_e }, // inc e
    &[_]Uop{ .read_e, .dec_byte, .write_e }, // dec e
    &[_]Uop{ .fetch_byte, .write_e }, // ld e,n
    &[_]Uop{.unimplemented}, // rra
    &[_]Uop{.unimplemented}, // jr nz,d
    &[_]Uop{ .fetch_word, .mask_word_inst, .write_hl }, // ld hl,nn
    &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_hl, .store_word }, // ld (nn),hl
    &[_]Uop{ .read_hl, .inc_word, .mask_word_inst, .write_hl }, // inc hl
    &[_]Uop{ .read_h, .inc_byte, .write_h }, // inc h
    &[_]Uop{ .read_h, .dec_byte, .write_h }, // dec h
    &[_]Uop{ .fetch_byte, .write_h }, // ld h,n
    &[_]Uop{.unimplemented}, // daa
    &[_]Uop{.unimplemented}, // jr z,d
    &[_]Uop{ .read_hl, .save, .add_words, .write_hl }, // add hl,hl
    &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .load_word, .write_hl }, // ld hl,(nn)
    &[_]Uop{ .read_hl, .dec_word, .mask_word_inst, .write_hl }, // dec hl
    &[_]Uop{ .read_l, .inc_byte, .write_l }, // inc l
    &[_]Uop{ .read_l, .dec_byte, .write_l }, // dec l
    &[_]Uop{ .fetch_byte, .write_l }, // ld l,n
    &[_]Uop{.unimplemented}, // cpl
    &[_]Uop{.unimplemented}, // jr nc,d
    &[_]Uop{ .fetch_word, .write_sp }, // ld sp,nn
    &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .read_a, .store_byte }, // ld (nn),a
    &[_]Uop{ .read_sp, .inc_word, .write_sp }, // inc sp
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .load_byte, .add_cc_1, .inc_byte, .store_byte }, // inc (hl)
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .load_byte, .add_cc_1, .dec_byte, .store_byte }, // dec (hl)
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .fetch_byte, .store_byte }, // ld (hl),n
    &[_]Uop{.unimplemented}, // scf
    &[_]Uop{.unimplemented}, // jr c,d
    &[_]Uop{ .read_sp, .save, .read_hl, .add_words, .write_hl }, // add hl,sp
    &[_]Uop{ .fetch_word, .save, .mask_addr_inst, .load_byte, .write_a }, // ld a,(nn)
    &[_]Uop{ .read_sp, .dec_word, .write_sp }, // dec sp
    &[_]Uop{ .read_a, .inc_byte, .write_a }, // inc a
    &[_]Uop{ .read_a, .dec_byte, .write_a }, // dec a
    &[_]Uop{ .fetch_byte, .write_a }, // ld a,n
    &[_]Uop{.unimplemented}, // ccf
    &[_]Uop{ .mode_sis, .add_r_1, .fetch_byte, .dispatch_base }, // .sis
    &[_]Uop{ .read_c, .write_b }, // ld b,c
    &[_]Uop{ .read_d, .write_b }, // ld b,d
    &[_]Uop{ .read_e, .write_b }, // ld b,e
    &[_]Uop{ .read_h, .write_b }, // ld b,h
    &[_]Uop{ .read_l, .write_b }, // ld b,l
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .load_byte, .write_b }, // ld b,(hl)
    &[_]Uop{ .read_a, .write_b }, // ld b,a
    &[_]Uop{ .read_b, .write_c }, // ld c,b
    &[_]Uop{ .mode_lis, .add_r_1, .fetch_byte, .dispatch_base }, // .lis
    &[_]Uop{ .read_d, .write_c }, // ld c,d
    &[_]Uop{ .read_e, .write_c }, // ld c,e
    &[_]Uop{ .read_h, .write_c }, // ld c,h
    &[_]Uop{ .read_l, .write_c }, // ld c,l
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .load_byte, .write_c }, // ld c,(hl)
    &[_]Uop{ .read_a, .write_c }, // ld c,a
    &[_]Uop{ .read_b, .write_d }, // ld d,b
    &[_]Uop{ .read_c, .write_d }, // ld d,c
    &[_]Uop{ .mode_sil, .add_r_1, .fetch_byte, .dispatch_base }, // .sil
    &[_]Uop{ .read_e, .write_d }, // ld d,e
    &[_]Uop{ .read_h, .write_d }, // ld d,h
    &[_]Uop{ .read_l, .write_d }, // ld d,l
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .load_byte, .write_d }, // ld d,(hl)
    &[_]Uop{ .read_a, .write_d }, // ld d,a
    &[_]Uop{ .read_b, .write_e }, // ld e,b
    &[_]Uop{ .read_c, .write_e }, // ld e,c
    &[_]Uop{ .read_d, .write_e }, // ld e,d
    &[_]Uop{ .mode_lil, .add_r_1, .fetch_byte, .dispatch_base }, // .lil
    &[_]Uop{ .read_h, .write_e }, // ld e,h
    &[_]Uop{ .read_l, .write_e }, // ld e,l
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .load_byte, .write_e }, // ld e,(hl)
    &[_]Uop{ .read_a, .write_e }, // ld e,a
    &[_]Uop{ .read_b, .write_h }, // ld h,b
    &[_]Uop{ .read_c, .write_h }, // ld h,c
    &[_]Uop{ .read_d, .write_h }, // ld h,d
    &[_]Uop{ .read_e, .write_h }, // ld h,e
    &[_]Uop{}, // ld h,h
    &[_]Uop{ .read_l, .write_h }, // ld h,l
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .load_byte, .write_h }, // ld h,(hl)
    &[_]Uop{ .read_a, .write_h }, // ld h,a
    &[_]Uop{ .read_b, .write_l }, // ld l,b
    &[_]Uop{ .read_c, .write_l }, // ld l,c
    &[_]Uop{ .read_d, .write_l }, // ld l,d
    &[_]Uop{ .read_e, .write_l }, // ld l,e
    &[_]Uop{ .read_h, .write_l }, // ld l,h
    &[_]Uop{}, // ld l,l
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .load_byte, .write_l }, // ld l,(hl)
    &[_]Uop{ .read_a, .write_l }, // ld a,l
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .read_b, .store_byte }, // ld (hl),b
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .read_c, .store_byte }, // ld (hl),c
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .read_d, .store_byte }, // ld (hl),d
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .read_e, .store_byte }, // ld (hl),e
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .read_h, .store_byte }, // ld (hl),h
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .read_l, .store_byte }, // ld (hl),l
    &[_]Uop{.halt}, // halt
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .read_a, .store_byte }, // ld (hl),a
    &[_]Uop{ .read_b, .write_a }, // ld a,b
    &[_]Uop{ .read_c, .write_a }, // ld a,c
    &[_]Uop{ .read_d, .write_a }, // ld a,d
    &[_]Uop{ .read_e, .write_a }, // ld a,e
    &[_]Uop{ .read_h, .write_a }, // ld a,h
    &[_]Uop{ .read_l, .write_a }, // ld a,l
    &[_]Uop{ .read_hl, .save, .mask_addr_inst, .load_byte, .write_a }, // ld a,(hl)
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
    &[_]Uop{.unimplemented}, // jp nz,nn
    &[_]Uop{.unimplemented}, // jp nn
    &[_]Uop{.unimplemented}, // call nz,nn
    &[_]Uop{.unimplemented}, // push bc
    &[_]Uop{ .fetch_byte, .unimplemented }, // add a,n
    &[_]Uop{.unimplemented}, // rst 00h
    &[_]Uop{.unimplemented}, // ret z
    &[_]Uop{.unimplemented}, // ret
    &[_]Uop{.unimplemented}, // jp z,nn
    &[_]Uop{.unimplemented}, // CB
    &[_]Uop{.unimplemented}, // call z,nn
    &[_]Uop{.unimplemented}, // call nn
    &[_]Uop{ .fetch_byte, .unimplemented }, // adc a,n
    &[_]Uop{.unimplemented}, // rst 08h
    &[_]Uop{.unimplemented}, // ret nc
    &[_]Uop{.unimplemented}, // pop de
    &[_]Uop{.unimplemented}, // jp nc,nn
    &[_]Uop{.unimplemented}, // out (n),a
    &[_]Uop{.unimplemented}, // call nc,nn
    &[_]Uop{.unimplemented}, // push de
    &[_]Uop{ .fetch_byte, .unimplemented }, // sub a,n
    &[_]Uop{.unimplemented}, // rst 10h
    &[_]Uop{.unimplemented}, // ret c
    &[_]Uop{
        .read_bc, .save, .@"read_bc'", .write_bc, .restore, .@"write_bc'",
        .read_de, .save, .@"read_de'", .write_de, .restore, .@"write_de'",
        .read_hl, .save, .@"read_hl'", .write_hl, .restore, .@"write_hl'",
    }, // exx
    &[_]Uop{.unimplemented}, // jp c,nn
    &[_]Uop{.unimplemented}, // in a,(n)
    &[_]Uop{.unimplemented}, // call c,nn
    &[_]Uop{.unimplemented}, // DD
    &[_]Uop{ .fetch_byte, .unimplemented }, // sbc a,n
    &[_]Uop{.unimplemented}, // rst 18h
    &[_]Uop{.unimplemented}, // ret po
    &[_]Uop{.unimplemented}, // pop hl
    &[_]Uop{.unimplemented}, // jp po,nn
    &[_]Uop{.unimplemented}, // ex (sp),hl
    &[_]Uop{.unimplemented}, // call po,nn
    &[_]Uop{.unimplemented}, // push hl
    &[_]Uop{ .fetch_byte, .unimplemented }, // and a,n
    &[_]Uop{.unimplemented}, // rst 20h
    &[_]Uop{.unimplemented}, // ret pe
    &[_]Uop{.unimplemented}, // jp (hl)
    &[_]Uop{.unimplemented}, // jp pe,nn
    &[_]Uop{.unimplemented}, // ex de,hl
    &[_]Uop{.unimplemented}, // call pe,nn
    &[_]Uop{.unimplemented}, // ED
    &[_]Uop{ .fetch_byte, .unimplemented }, // xor a,n
    &[_]Uop{.unimplemented}, // rst 28h
    &[_]Uop{.unimplemented}, // ret p
    &[_]Uop{.unimplemented}, // pop af
    &[_]Uop{.unimplemented}, // jp p,nn
    &[_]Uop{.unimplemented}, // di
    &[_]Uop{.unimplemented}, // call p,nn
    &[_]Uop{.unimplemented}, // push af
    &[_]Uop{ .fetch_byte, .unimplemented }, // or a,n
    &[_]Uop{.unimplemented}, // rst 30h
    &[_]Uop{.unimplemented}, // ret m
    &[_]Uop{ .read_hl, .write_sp }, // ld sp,hl
    &[_]Uop{.unimplemented}, // jp m,nn
    &[_]Uop{.unimplemented}, // ei
    &[_]Uop{.unimplemented}, // call m,nn
    &[_]Uop{.unimplemented}, // FD
    &[_]Uop{ .fetch_byte, .unimplemented }, // cp a,n
    &[_]Uop{.unimplemented}, // rst 38h
};

pub fn decode(impl: anytype) void {
    dispatchAll(impl, &[_]Uop{ .add_r_1, .fetch_byte, .dispatch_base });
}

fn dispatcherFor(comptime table: *const [1 << 8][]const Uop) fn (anytype, comptime u8) void {
    return struct {
        fn dispatcher(impl: anytype, comptime opcode: u8) void {
            dispatchAll(impl, table[opcode]);
        }
    }.dispatcher;
}

fn dispatchAll(impl: anytype, comptime uops: []const Uop) void {
    inline for (uops) |uop| dispatch(impl, uop);
}

fn dispatch(impl: anytype, comptime uop: Uop) void {
    switch (uop) {
        .unimplemented => if (true)
            impl.skip()
        else
            std.debug.todo(@tagName(uop) ++ " opcode"),

        .save => impl.save(),
        .restore => impl.restore(),
        .swap => impl.swap(),

        .mask_word_inst => impl.maskWordInst(),
        .mask_addr_adl => impl.maskAddressAdl(),
        .mask_addr_inst => impl.maskAddressInst(),

        .fetch_byte => impl.fetchByte(),
        .fetch_word => impl.fetchWord(),

        .add_r_1 => impl.addR(1),
        .add_cc_1 => impl.addCycles(1),

        .dispatch_base => impl.dispatch(dispatcherFor(&base)),

        .mode_sis => impl.setMode(.s, .is),
        .mode_lis => impl.setMode(.l, .is),
        .mode_sil => impl.setMode(.s, .il),
        .mode_lil => impl.setMode(.l, .il),

        .halt => impl.halt(),

        .read_b => impl.readRegister(.b),
        .read_c => impl.readRegister(.c),
        .read_d => impl.readRegister(.d),
        .read_e => impl.readRegister(.e),
        .read_h => impl.readRegister(.h),
        .read_l => impl.readRegister(.l),
        .read_a => impl.readRegister(.a),
        .read_af => impl.readRegister(.af),
        .read_bc => impl.readRegister(.ubc),
        .read_de => impl.readRegister(.ude),
        .read_hl => impl.readRegister(.uhl),
        .read_sp => impl.readStackPointer(),
        .@"read_af'" => impl.readShadowRegister(.af),
        .@"read_bc'" => impl.readShadowRegister(.bc),
        .@"read_de'" => impl.readShadowRegister(.de),
        .@"read_hl'" => impl.readShadowRegister(.hl),

        .write_b => impl.writeRegister(.b),
        .write_c => impl.writeRegister(.c),
        .write_d => impl.writeRegister(.d),
        .write_e => impl.writeRegister(.e),
        .write_h => impl.writeRegister(.h),
        .write_l => impl.writeRegister(.l),
        .write_a => impl.writeRegister(.a),
        .write_af => impl.writeRegister(.af),
        .write_bc => impl.writeRegister(.ubc),
        .write_de => impl.writeRegister(.ude),
        .write_hl => impl.writeRegister(.uhl),
        .write_sp => impl.writeStackPointer(),
        .@"write_af'" => impl.writeShadowRegister(.af),
        .@"write_bc'" => impl.writeShadowRegister(.bc),
        .@"write_de'" => impl.writeShadowRegister(.de),
        .@"write_hl'" => impl.writeShadowRegister(.hl),

        .load_byte => impl.loadByte(),
        .load_word => impl.loadWord(),

        .store_byte => impl.storeByte(),
        .store_word => impl.storeWord(),

        .inc_byte => impl.addByte(1),
        .dec_byte => impl.addByte(-1),
        .inc_word => impl.addWord(1),
        .dec_word => impl.addWord(-1),
        .add_words => impl.addWords(),
    }
}
