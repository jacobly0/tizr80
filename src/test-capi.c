#include <cemucore.h>

#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[]) {
    fprintf(stderr, "Running %s tests.\n", strrchr(argv[0], '/') + 1);

    fprintf(stderr, "NDEBUG");
#ifndef NDEBUG
    fprintf(stderr, " not");
#endif
    fprintf(stderr, " defined\n");

    fprintf(stderr, "CEMUCORE_DEBUGGER = %d\n", CEMUCORE_DEBUGGER);

    cemucore_t *core =
        cemucore_create(CEMUCORE_CREATE_FLAG_THREADED, NULL, NULL);
    cemucore_destroy(core);
    core = NULL;

    fprintf(stderr, "Tests passed.\n");
}
