#include <e-feldspar.h>
#include <e-lib.h>
#include <stdbool.h>
#include <stdint.h>

uint32_t *const la0 = (uint32_t *) 8192;
uint32_t *const la1 = (uint32_t *) 8196;
uint32_t *const la2 = (uint32_t *) 8200;
void *const sa2 = (void *) 16777216;

#define BSZ 1024

int main()
{
    int32_t data[BSZ];

    while (1) {
        while (!*la0);
        *la0 = 0;
#ifndef LOCAL
        core_read_shared(sa2, data, 0, 0, BSZ - 1);
#endif
#ifndef DONT_WORK
        for (int i = 0; i < BSZ; ++i) {
#ifndef LOCAL
            data[i] += 16;
#else
            la2[i] += 16;
#endif
        }
#endif
#ifndef LOCAL
        core_write_shared(sa2, data, 0, 0, BSZ - 1);
#endif
        *la1 = 1;
    }
    return 0;
}
