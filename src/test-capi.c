#include <tizr80.h>

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

__attribute__((__cold__, __noreturn__)) static void
expect_fail(const char *file, int line, const char *function, const char *condition) {
    fprintf(stderr, "%s:%d:%s: expected \"%s\"\n", file, line, function, condition);
    abort();
}
#define expect(condition)                                                                          \
    do                                                                                             \
        if (condition)                                                                             \
            ;                                                                                      \
        else                                                                                       \
            expect_fail(__FILE__, __LINE__, __FUNCTION__, #condition);                             \
    while (0)

static void test_cpu_registers(tizr80_t *core) {
    tizr80_set(core, TIZR80_PROP_REG, TIZR80_REG_AF, 0x0123);
    tizr80_set(core, TIZR80_PROP_REG, TIZR80_REG_UBC, 0x456789);
    tizr80_set(core, TIZR80_PROP_REG, TIZR80_REG_UDE, 0xABCDEF);
    tizr80_set(core, TIZR80_PROP_REG, TIZR80_REG_UHL, 0x02468A);
    tizr80_set(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_AF, 0xCE13);
    tizr80_set(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_UBC, 0x579ACE);
    tizr80_set(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_UDE, 0x0369CF);
    tizr80_set(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_UHL, 0x147AD2);
    tizr80_set(core, TIZR80_PROP_REG, TIZR80_REG_UIX, 0x58BE04);
    tizr80_set(core, TIZR80_PROP_REG, TIZR80_REG_UIY, 0x8C159D);
    tizr80_set(core, TIZR80_PROP_REG, TIZR80_REG_SPS, 0x26AE);
    tizr80_set(core, TIZR80_PROP_REG, TIZR80_REG_SPL, 0x37BF05);
    tizr80_set(core, TIZR80_PROP_REG, TIZR80_REG_PC, 0xAF16B2);
    tizr80_set(core, TIZR80_PROP_REG, TIZR80_REG_UI, 0x7C38);
    tizr80_set(core, TIZR80_PROP_REG, TIZR80_REG_R, 0xD4);
    tizr80_set(core, TIZR80_PROP_REG, TIZR80_REG_MBASE, 0x9E);

    expect(1 == tizr80_get(core, TIZR80_PROP_REG, TIZR80_FLAG_C));
    expect(1 == tizr80_get(core, TIZR80_PROP_REG, TIZR80_FLAG_N));
    expect(0 == tizr80_get(core, TIZR80_PROP_REG, TIZR80_FLAG_PV));
    expect(0 == tizr80_get(core, TIZR80_PROP_REG, TIZR80_FLAG_X));
    expect(0 == tizr80_get(core, TIZR80_PROP_REG, TIZR80_FLAG_H));
    expect(1 == tizr80_get(core, TIZR80_PROP_REG, TIZR80_FLAG_Y));
    expect(0 == tizr80_get(core, TIZR80_PROP_REG, TIZR80_FLAG_Z));
    expect(0 == tizr80_get(core, TIZR80_PROP_REG, TIZR80_FLAG_S));

    expect(1 == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_FLAG_C));
    expect(1 == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_FLAG_N));
    expect(0 == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_FLAG_PV));
    expect(0 == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_FLAG_X));
    expect(1 == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_FLAG_H));
    expect(0 == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_FLAG_Y));
    expect(0 == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_FLAG_Z));
    expect(0 == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_FLAG_S));

    expect(UINT8_C(0x23) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_F));
    expect(UINT8_C(0x01) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_A));
    expect(UINT8_C(0x89) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_C));
    expect(UINT8_C(0x67) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_B));
    expect(UINT8_C(0x45) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_BCU));
    expect(UINT8_C(0xEF) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_E));
    expect(UINT8_C(0xCD) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_D));
    expect(UINT8_C(0xAB) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_DEU));
    expect(UINT8_C(0x8A) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_L));
    expect(UINT8_C(0x46) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_H));
    expect(UINT8_C(0x02) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_HLU));
    expect(UINT8_C(0x04) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_IXL));
    expect(UINT8_C(0xBE) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_IXH));
    expect(UINT8_C(0x58) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_IXU));
    expect(UINT8_C(0x9D) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_IYL));
    expect(UINT8_C(0x15) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_IYH));
    expect(UINT8_C(0x8C) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_IYU));
    expect(UINT8_C(0xD4) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_R));
    expect(UINT8_C(0x9E) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_MBASE));

    expect(UINT8_C(0x13) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_F));
    expect(UINT8_C(0xCE) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_A));
    expect(UINT8_C(0xCE) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_C));
    expect(UINT8_C(0x9A) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_B));
    expect(UINT8_C(0x57) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_BCU));
    expect(UINT8_C(0xCF) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_E));
    expect(UINT8_C(0x69) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_D));
    expect(UINT8_C(0x03) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_DEU));
    expect(UINT8_C(0xD2) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_L));
    expect(UINT8_C(0x7A) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_H));
    expect(UINT8_C(0x14) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_HLU));

    expect(UINT16_C(0x0123) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_AF));
    expect(UINT16_C(0x6789) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_BC));
    expect(UINT16_C(0xCDEF) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_DE));
    expect(UINT16_C(0x468A) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_HL));
    expect(UINT16_C(0xBE04) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_IX));
    expect(UINT16_C(0x159D) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_IY));
    expect(UINT16_C(0x26AE) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_SPS));
    expect(UINT16_C(0x7C38) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_UI));

    expect(UINT16_C(0xCE13) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_AF));
    expect(UINT16_C(0x9ACE) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_BC));
    expect(UINT16_C(0x69CF) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_DE));
    expect(UINT16_C(0x7AD2) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_HL));

    expect(UINT32_C(0x456789) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_UBC));
    expect(UINT32_C(0xABCDEF) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_UDE));
    expect(UINT32_C(0x02468A) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_UHL));
    expect(UINT32_C(0x58BE04) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_UIX));
    expect(UINT32_C(0x8C159D) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_UIY));
    expect(UINT32_C(0x37BF05) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_SPL));
    expect(UINT32_C(0xAF16B2) == tizr80_get(core, TIZR80_PROP_REG, TIZR80_REG_PC));

    expect(UINT32_C(0x579ACE) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_UBC));
    expect(UINT32_C(0x0369CF) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_UDE));
    expect(UINT32_C(0x147AD2) == tizr80_get(core, TIZR80_PROP_REG_SHADOW, TIZR80_REG_UHL));
}

static void test_memory(tizr80_t *core) {
    tizr80_set(core, TIZR80_PROP_RAM, 0, 0x42);
    tizr80_set(core, TIZR80_PROP_RAM, 1, 0x24);
    expect(0x24 == tizr80_get(core, TIZR80_PROP_RAM, 1));
    expect(0x42 == tizr80_get(core, TIZR80_PROP_RAM, 0));
}

int main(int argc, char *argv[]) {
    fprintf(stderr, "Running %s tests.\n", strrchr(argv[0], '/') + 1);

    tizr80_t *core = tizr80_create(TIZR80_CREATE_FLAG_THREADED, NULL, NULL);
    test_cpu_registers(core);

    test_memory(core);

    expect(-EINVAL == tizr80_command(core, (const char *[]){NULL}));

    uint8_t program[] = {0x00, 0x41, 0x53, 0x69, 0x7B, 0x76, 0x18, -2};
    for (uint32_t address = 0; address != sizeof(program); ++address)
        tizr80_set(core, TIZR80_PROP_RAM, address, program[address]);
    tizr80_set(core, TIZR80_PROP_REG, TIZR80_REG_PC, 0);
    tizr80_set(core, TIZR80_PROP_REG, TIZR80_REG_MBASE, 0xD0);

    expect(!tizr80_sleep(core));
    expect(tizr80_wake(core));
    expect(!tizr80_wake(core));
    sleep(1);
    expect(tizr80_sleep(core));
    expect(!tizr80_sleep(core));
    sleep(1);

    tizr80_destroy(core);
    core = NULL;

    fprintf(stderr, "All tests passed.\n");
}
