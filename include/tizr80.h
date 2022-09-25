#ifndef TIZR80_H
#define TIZR80_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#if defined(TIZR80_SHARED) && defined(_WIN32)
#define TIZR80_EXPORT __declspec(dllimport)
#else
#define TIZR80_EXPORT
#endif

typedef enum tizr80_sig {
    TIZR80_SIG_DEV_CHANGED,
    TIZR80_SIG_FLASH_SIZE_CHANGED,
    TIZR80_SIG_TRANSFER_TOTAL,
    TIZR80_SIG_TRANSFER_PROGRESS,
    TIZR80_SIG_TRANSFER_COMPLETE,
    TIZR80_SIG_LCD_FRAME,
    TIZR80_SIG_SOFT_CMD,
} tizr80_sig_t;

typedef enum tizr80_create_flags {
    TIZR80_CREATE_FLAG_THREADED = 1 << 0,
} tizr80_create_flags_t;

typedef enum tizr80_prop {
    TIZR80_PROP_DEV,
    TIZR80_PROP_REG,
    TIZR80_PROP_REG_SHADOW,
    TIZR80_PROP_KEY,
    TIZR80_PROP_FLASH_SIZE,
    TIZR80_PROP_MEM_Z80,
    TIZR80_PROP_MEM_ADL,
    TIZR80_PROP_FLASH,
    TIZR80_PROP_RAM,
    TIZR80_PROP_PORT,
    TIZR80_PROP_TRANSFER,
#if TIZR80_DEBUGGER
    TIZR80_PROP_WATCH,
    TIZR80_PROP_WATCH_ADDR,
    TIZR80_PROP_WATCH_SIZE,
    TIZR80_PROP_WATCH_FLAGS,
    TIZR80_PROP_MEM_Z80_WATCH_FLAGS,
    TIZR80_PROP_MEM_ADL_WATCH_FLAGS,
    TIZR80_PROP_FLASH_WATCH_FLAGS,
    TIZR80_PROP_RAM_WATCH_FLAGS,
    TIZR80_PROP_PORT_WATCH_FLAGS,
#endif
} tizr80_prop_t;

typedef enum tizr80_dev {
    TIZR80_DEV_UNKNOWN,
    TIZR80_DEV_TI84PCE,
    TIZR80_DEV_TI84PCEPE,
    TIZR80_DEV_TI83PCE,
    TIZR80_DEV_TI83PCEEP,
    TIZR80_DEV_TI84PCET,
    TIZR80_DEV_TI84PCETPE,
} tizr80_dev_t;

typedef enum tizr80_transfer {
    TIZR80_TRANSFER_TOTAL,
    TIZR80_TRANSFER_PROGRESS,
    TIZR80_TRANSFER_REMAINING,
    TIZR80_TRANSFER_ERROR,
} tizr80_transfer_t;

typedef enum tizr80_reg {
    /* state */
    TIZR80_STATE_ADL,
    TIZR80_STATE_IEF,
    TIZR80_STATE_IM,

    /* 1-bit flags */
    TIZR80_FLAG_C,
    TIZR80_FLAG_N,
    TIZR80_FLAG_PV,
    TIZR80_FLAG_X,
    TIZR80_FLAG_H,
    TIZR80_FLAG_Y,
    TIZR80_FLAG_Z,
    TIZR80_FLAG_S,

    /* 8-bit registers */
    TIZR80_REG_F,
    TIZR80_REG_A,
    TIZR80_REG_C,
    TIZR80_REG_B,
    TIZR80_REG_BCU,
    TIZR80_REG_E,
    TIZR80_REG_D,
    TIZR80_REG_DEU,
    TIZR80_REG_L,
    TIZR80_REG_H,
    TIZR80_REG_HLU,
    TIZR80_REG_IXL,
    TIZR80_REG_IXH,
    TIZR80_REG_IXU,
    TIZR80_REG_IYL,
    TIZR80_REG_IYH,
    TIZR80_REG_IYU,
    TIZR80_REG_I,
    TIZR80_REG_R,
    TIZR80_REG_MBASE,

    /* 16-bit registers */
    TIZR80_REG_AF,
    TIZR80_REG_BC,
    TIZR80_REG_DE,
    TIZR80_REG_HL,
    TIZR80_REG_IX,
    TIZR80_REG_IY,
    TIZR80_REG_SPS,
    TIZR80_REG_UI,

    /* 24-bit registers */
    TIZR80_REG_UBC,
    TIZR80_REG_UDE,
    TIZR80_REG_UHL,
    TIZR80_REG_UIX,
    TIZR80_REG_UIY,
    TIZR80_REG_SPL,
    TIZR80_REG_MBASEUI,
    TIZR80_REG_PC,
} tizr80_reg_t;

#if TIZR80_DEBUGGER
typedef enum tizr80_watch_flags {
    TIZR80_WATCH_AREA_PORT = 0 << 0,
    TIZR80_WATCH_AREA_MEM = 1 << 0,
    TIZR80_WATCH_AREA_FLASH = 2 << 0,
    TIZR80_WATCH_AREA_RAM = 3 << 0,
    TIZR80_WATCH_AREA_MASK = TIZR80_WATCH_AREA_PORT | TIZR80_WATCH_AREA_MEM |
                             TIZR80_WATCH_AREA_FLASH | TIZR80_WATCH_AREA_RAM,

    TIZR80_WATCH_MODE_PORT = 0 << 2,
    TIZR80_WATCH_MODE_Z80 = 1 << 2,
    TIZR80_WATCH_MODE_ADL = 2 << 2,
    TIZR80_WATCH_MODE_ANY = TIZR80_WATCH_MODE_Z80 | TIZR80_WATCH_MODE_ADL,

    TIZR80_WATCH_TYPE_READ = 1 << 4,
    TIZR80_WATCH_TYPE_WRITE = 1 << 5,
    TIZR80_WATCH_TYPE_READ_WRITE = TIZR80_WATCH_TYPE_READ | TIZR80_WATCH_TYPE_WRITE,
    TIZR80_WATCH_TYPE_EXECUTE = 1 << 6,
    TIZR80_WATCH_TYPE_ALL = TIZR80_WATCH_TYPE_READ_WRITE | TIZR80_WATCH_TYPE_EXECUTE,

    TIZR80_WATCH_ENABLE = 1 << 7,
} tizr80_watch_flags_t;
#endif

typedef struct tizr80 tizr80_t;

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*tizr80_sig_handler_t)(tizr80_sig_t, void *);

TIZR80_EXPORT tizr80_t *tizr80_create(tizr80_create_flags_t create_flags,
                                      tizr80_sig_handler_t sig_handler, void *sig_handler_data);
TIZR80_EXPORT tizr80_t *tizr80_destroy(tizr80_t *core);

TIZR80_EXPORT int32_t tizr80_get(tizr80_t *core, tizr80_prop_t prop, int32_t addr);
TIZR80_EXPORT void tizr80_get_buffer(tizr80_t *core, tizr80_prop_t prop, int32_t addr, void *buf,
                                     uint32_t len);
TIZR80_EXPORT void tizr80_set(tizr80_t *core, tizr80_prop_t prop, int32_t addr, int32_t val);
TIZR80_EXPORT void tizr80_set_buffer(tizr80_t *core, tizr80_prop_t prop, int32_t addr,
                                     const void *buf, uint32_t len);
TIZR80_EXPORT int tizr80_command(tizr80_t *core, const char *const *command);

TIZR80_EXPORT bool tizr80_sleep(tizr80_t *core);
TIZR80_EXPORT bool tizr80_wake(tizr80_t *core);

#undef TIZR80_EXPORT

#ifdef __cplusplus
}
#endif

#endif
