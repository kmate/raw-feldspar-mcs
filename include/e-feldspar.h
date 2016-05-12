#ifndef __EFELDSPAR_H__
#define __EFELDSPAR_H__

// host operations on core local memories

#define host_write_local(g, r, c, dst, src, offset, lower, upper) \
    e_write(g, r, c, (dst) + (offset) * sizeof(*src), (src) + (lower), ((upper) - (lower) + 1) * sizeof(*src))

#define host_read_local(g, r, c, src, dst, offset, lower, upper) \
    e_read(g, r, c, (src) + (offset) * sizeof(*dst), (dst) + (lower), ((upper) - (lower) + 1) * sizeof(*dst))

// host operations on shared external memory

#define host_write_shared(dst, src, offset, lower, upper) \
    e_write(dst, 0, 0, (offset) * sizeof(*src), (src) + (lower), ((upper) - (lower) + 1) * sizeof(*src))

#define host_read_shared(src, dst, offset, lower, upper) \
    e_read(src, 0, 0, (offset) * sizeof(*dst), (dst) + (lower), ((upper) - (lower) + 1) * sizeof(*dst))

// core operations on core local memories

// TODO: fall back to fast_memcpy when dma requirements fail
#define core_write_local(dst, src, offset, lower, upper) \
    e_dma_copy((void*)((dst) + (offset)), (void*)((src) + (lower)), ((upper) - (lower) + 1) * sizeof(*src))

#define core_read_local(src, dst, offset, lower, upper) \
    fast_memcpy((void*)((dst) + (lower)), (void*)((src) + (offset)), ((upper) - (lower) + 1) * sizeof(*dst))

#include <stdint.h>
#include <stdlib.h>

// based on the epiphany-ebsp library
void fast_memcpy(void *dst, const void *src, size_t bytes) {
    unsigned bits = (unsigned) dst | (unsigned) src;
    if (0 == bits & 0x7) { // align 8
        int count = bytes >> 3;
        bytes &= 0x7;
        uint64_t *dst8 = (uint64_t *) dst;
        const uint64_t *src8 = (const uint64_t *) src;
        while (count--) {
            *dst8++ = *src8++;
        }
        dst = (void *) dst8;
        src = (const void *) src8;
    } else if (0 == bits & 0x3) { // align 4
        int count = bytes >> 2;
        bytes &= 0x3;
        uint32_t *dst4 = (uint32_t *) dst;
        const uint32_t *src4 = (const uint32_t *) src;
        while (count--) {
            *dst4++ = *src4++;
        }
        dst = (void *) dst4;
        src = (const void *) src4;
    }
    uint8_t *dst1 = (uint8_t *) dst;
    const uint8_t *src1 = (const uint8_t *) src;
    while (bytes--) {
        *dst1++ = *src1++;
    }
}

// core operations on shared external memory

#define core_write_shared(dst, src, offset, lower, upper) \
    e_dma_copy((void*)(e_emem_config.base + (dst) + (offset) * sizeof(*src)), (void*)((src) + (lower)), ((upper) - (lower) + 1) * sizeof(*src))

#define core_read_shared(src, dst, offset, lower, upper) \
    e_dma_copy((void*)((dst) + (lower)), (void*)(e_emem_config.base + (src) + (offset) * sizeof(*dst)), ((upper) - (lower) + 1) * sizeof(*dst))

#endif /* __EFELDSPAR_H__ */

