#include <e-feldspar.h>
#include <e-lib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

uint32_t *const c1s = (uint32_t *) 8192;
uint32_t *const c1f = (uint32_t *) 8196;
int32_t *const data = (int32_t *) 8200;
void *const sh = (void *) 16777216;

#define BSZ 1024

int main()
{
    while (1) {
        while (!*c1s);
        *c1s = 0;
#ifndef DONT_WORK
        for (int i = 0; i < BSZ; ++i) {
            data[i] += 8;
        }
#endif
        core_write_shared(sh, data, 0, 0, BSZ - 1);
        *c1f = 1;
    }
    return 0;
}

