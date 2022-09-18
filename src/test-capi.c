#include <cemucore.h>

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

static void test_cpu_registers(cemucore_t *core) {
    cemucore_set(core, CEMUCORE_PROP_REG, CEMUCORE_REG_AF, 0x0123);
    cemucore_set(core, CEMUCORE_PROP_REG, CEMUCORE_REG_UBC, 0x456789);
    cemucore_set(core, CEMUCORE_PROP_REG, CEMUCORE_REG_UDE, 0xABCDEF);
    cemucore_set(core, CEMUCORE_PROP_REG, CEMUCORE_REG_UHL, 0x02468A);
    cemucore_set(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_AF, 0xCE13);
    cemucore_set(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_UBC, 0x579ACE);
    cemucore_set(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_UDE, 0x0369CF);
    cemucore_set(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_UHL, 0x147AD2);
    cemucore_set(core, CEMUCORE_PROP_REG, CEMUCORE_REG_UIX, 0x58BE04);
    cemucore_set(core, CEMUCORE_PROP_REG, CEMUCORE_REG_UIY, 0x8C159D);
    cemucore_set(core, CEMUCORE_PROP_REG, CEMUCORE_REG_SPS, 0x26AE);
    cemucore_set(core, CEMUCORE_PROP_REG, CEMUCORE_REG_SPL, 0x37BF05);
    cemucore_set(core, CEMUCORE_PROP_REG, CEMUCORE_REG_PC, 0xAF16B2);
    cemucore_set(core, CEMUCORE_PROP_REG, CEMUCORE_REG_UI, 0x7C38);
    cemucore_set(core, CEMUCORE_PROP_REG, CEMUCORE_REG_R, 0xD4);
    cemucore_set(core, CEMUCORE_PROP_REG, CEMUCORE_REG_MB, 0x9E);

    expect(1 == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_FLAG_C));
    expect(1 == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_FLAG_N));
    expect(0 == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_FLAG_PV));
    expect(0 == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_FLAG_X));
    expect(0 == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_FLAG_HC));
    expect(1 == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_FLAG_Y));
    expect(0 == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_FLAG_Z));
    expect(0 == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_FLAG_S));

    expect(1 == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_FLAG_C));
    expect(1 == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_FLAG_N));
    expect(0 == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_FLAG_PV));
    expect(0 == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_FLAG_X));
    expect(1 == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_FLAG_HC));
    expect(0 == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_FLAG_Y));
    expect(0 == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_FLAG_Z));
    expect(0 == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_FLAG_S));

    expect(UINT8_C(0x23) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_F));
    expect(UINT8_C(0x01) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_A));
    expect(UINT8_C(0x89) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_C));
    expect(UINT8_C(0x67) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_B));
    expect(UINT8_C(0x45) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_BCU));
    expect(UINT8_C(0xEF) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_E));
    expect(UINT8_C(0xCD) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_D));
    expect(UINT8_C(0xAB) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_DEU));
    expect(UINT8_C(0x8A) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_L));
    expect(UINT8_C(0x46) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_H));
    expect(UINT8_C(0x02) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_HLU));
    expect(UINT8_C(0x04) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_IXL));
    expect(UINT8_C(0xBE) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_IXH));
    expect(UINT8_C(0x58) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_IXU));
    expect(UINT8_C(0x9D) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_IYL));
    expect(UINT8_C(0x15) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_IYH));
    expect(UINT8_C(0x8C) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_IYU));
    expect(UINT8_C(0xD4) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_R));
    expect(UINT8_C(0x9E) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_MB));

    expect(UINT8_C(0x13) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_F));
    expect(UINT8_C(0xCE) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_A));
    expect(UINT8_C(0xCE) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_C));
    expect(UINT8_C(0x9A) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_B));
    expect(UINT8_C(0x57) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_BCU));
    expect(UINT8_C(0xCF) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_E));
    expect(UINT8_C(0x69) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_D));
    expect(UINT8_C(0x03) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_DEU));
    expect(UINT8_C(0xD2) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_L));
    expect(UINT8_C(0x7A) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_H));
    expect(UINT8_C(0x14) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_HLU));

    expect(UINT16_C(0x0123) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_AF));
    expect(UINT16_C(0x6789) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_BC));
    expect(UINT16_C(0xCDEF) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_DE));
    expect(UINT16_C(0x468A) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_HL));
    expect(UINT16_C(0xBE04) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_IX));
    expect(UINT16_C(0x159D) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_IY));
    expect(UINT16_C(0x26AE) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_SPS));
    expect(UINT16_C(0x7C38) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_UI));

    expect(UINT16_C(0xCE13) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_AF));
    expect(UINT16_C(0x9ACE) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_BC));
    expect(UINT16_C(0x69CF) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_DE));
    expect(UINT16_C(0x7AD2) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_HL));

    expect(UINT32_C(0x456789) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_UBC));
    expect(UINT32_C(0xABCDEF) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_UDE));
    expect(UINT32_C(0x02468A) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_UHL));
    expect(UINT32_C(0x58BE04) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_UIX));
    expect(UINT32_C(0x8C159D) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_UIY));
    expect(UINT32_C(0x37BF05) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_SPL));
    expect(UINT32_C(0xAF16B2) == cemucore_get(core, CEMUCORE_PROP_REG, CEMUCORE_REG_PC));

    expect(UINT32_C(0x579ACE) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_UBC));
    expect(UINT32_C(0x0369CF) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_UDE));
    expect(UINT32_C(0x147AD2) == cemucore_get(core, CEMUCORE_PROP_REG_SHADOW, CEMUCORE_REG_UHL));
}

static void test_memory(cemucore_t *core) {
    cemucore_set(core, CEMUCORE_PROP_RAM, 0, 0x42);
    cemucore_set(core, CEMUCORE_PROP_RAM, 1, 0x24);
    expect(0x24 == cemucore_get(core, CEMUCORE_PROP_RAM, 1));
    expect(0x42 == cemucore_get(core, CEMUCORE_PROP_RAM, 0));
}

int main(int argc, char *argv[]) {
    fprintf(stderr, "Running %s tests.\n", strrchr(argv[0], '/') + 1);

    cemucore_t *core = cemucore_create(CEMUCORE_CREATE_FLAG_THREADED, NULL, NULL);
    test_cpu_registers(core);

    test_memory(core);

    uint8_t program[] = {0x00, 0x41, 0x53, 0x69, 0x7B, 0x76, 0x18, -2};
    for (uint32_t address = 0; address != sizeof(program); ++address)
        cemucore_set(core, CEMUCORE_PROP_RAM, address, program[address]);
    cemucore_set(core, CEMUCORE_PROP_REG, CEMUCORE_REG_PC, 0);
    cemucore_set(core, CEMUCORE_PROP_REG, CEMUCORE_REG_MB, 0xD0);

    expect(!cemucore_sleep(core));
    expect(cemucore_wake(core));
    expect(!cemucore_wake(core));
    sleep(1);
    expect(cemucore_sleep(core));
    expect(!cemucore_sleep(core));
    sleep(1);

    cemucore_destroy(core);
    core = NULL;

    fprintf(stderr, "All tests passed.\n");
}
