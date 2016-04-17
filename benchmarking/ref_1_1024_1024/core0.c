#include <e-feldspar.h>
#include <e-lib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

uint32_t *const c0s = (uint32_t *) 8192;
uint32_t *const c0f = (uint32_t *) 8196;
uint32_t *const c1s = (uint32_t *) ((((0 + 32) * 64 + (8 + 1)) << 20) | 8192);
uint32_t *const c1f = (uint32_t *) ((((0 + 32) * 64 + (8 + 1)) << 20) | 8196);
int32_t *const cr = (int32_t *) ((((0 + 32) * 64 + (8 + 1)) << 20) | 8200);
void *const sh = (void *) 16777216;

#define BSZ 1024

int main()
{
    int32_t data[BSZ];

    while (1) {
        while (!*c0s);
        *c0s = 0;
        core_read_shared(sh, data, 0, 0, BSZ - 1);
#ifndef DONT_WORK
        for (int i = 0; i < BSZ; ++i) {
            data[i] += 8;
        }
#endif
        core_write_local(cr, data, 0, 0, BSZ - 1);
        *c1s = 1;
        *c0f = 1;
    }
    return 0;
}

