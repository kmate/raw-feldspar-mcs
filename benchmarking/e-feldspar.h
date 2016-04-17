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

#if defined(USE_DMA)

// WORKS ONLY WHEN SOURCE AND DESTINATION IS ALIGNED!
#warning using dma

#define core_write_local(dst, src, offset, lower, upper) \
        core_trace("DMA_WRITE_DST", (unsigned)((dst) + (offset))); \
        core_trace("DMA_WRITE_SRC", (unsigned)((src) + (lower))); \
        core_trace("DMA_WRITE_SIZE", (unsigned)(((upper) - (lower) + 1) * sizeof(*src))); \
    e_dma_copy((dst) + (offset), (src) + (lower), ((upper) - (lower) + 1) * sizeof(*src))

#define core_read_local(src, dst, offset, lower, upper) \
        core_trace("DMA_READ_DST", (unsigned)((dst) + (lower))); \
        core_trace("DMA_READ_SRC", (unsigned)((src) + (offset))); \
        core_trace("DMA_READ_SIZE", (unsigned)(((upper) - (lower) + 1) * sizeof(*dst))); \
    for (int i = 0; i < (upper) - (lower) + 1; ++i) dst[i + (lower)] = src[i + (offset)];
//    e_dma_copy((dst) + (lower), (src) + (offset), ((upper) - (lower) + 1) * sizeof(*dst))

#elif defined(USE_FASTCOPY)

#warning using fast copy

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

#define core_write_local(dst, src, offset, lower, upper) \
    fast_memcpy((dst) + (offset), (src) + (lower), ((upper) - (lower) + 1) * sizeof(*src))

#define core_read_local(src, dst, offset, lower, upper) \
    fast_memcpy((dst) + (lower), (src) + (offset), ((upper) - (lower) + 1) * sizeof(*dst))

#elif defined(USE_LOOPCOPY)

#warning using loop copy

#define core_write_local(dst, src, offset, lower, upper) \
    for (int i = 0; i < (upper) - (lower) + 1; ++i) dst[i + (offset)] = src[i + (lower)];

#define core_read_local(src, dst, offset, lower, upper) \
    for (int i = 0; i < (upper) - (lower) + 1; ++i) dst[i + (lower)] = src[i + (offset)];

#else

#warning using memcpy

#define core_write_local(dst, src, offset, lower, upper) \
        core_trace("MEMCPY_WRITE_DST", (unsigned)((dst) + (offset))); \
        core_trace("MEMCPY_WRITE_SRC", (unsigned)((src) + (lower))); \
        core_trace("MEMCPY_WRITE_SIZE", (unsigned)(((upper) - (lower) + 1) * sizeof(*src))); \
    memcpy((dst) + (offset), (src) + (lower), ((upper) - (lower) + 1) * sizeof(*src))

#define core_read_local(src, dst, offset, lower, upper) \
        core_trace("MEMCPY_READ_DST", (unsigned)((dst) + (lower))); \
        core_trace("MEMCPY_READ_SRC", (unsigned)((src) + (offset))); \
        core_trace("MEMCPY_READ_SIZE", (unsigned)(((upper) - (lower) + 1) * sizeof(*dst))); \
    memcpy((dst) + (lower), (src) + (offset), ((upper) - (lower) + 1) * sizeof(*dst))

#endif

// core operations on shared external memory

#define core_write_shared(dst, src, offset, lower, upper) \
    e_dma_copy(e_emem_config.base + (dst) + (offset) * sizeof(*src), (src) + (lower), ((upper) - (lower) + 1) * sizeof(*src))

#define core_read_shared(src, dst, offset, lower, upper) \
    e_dma_copy((dst) + (lower), e_emem_config.base + (src) + (offset) * sizeof(*dst), ((upper) - (lower) + 1) * sizeof(*dst))

// tracing on core

#ifdef TRACE
#ifndef HOST

#include <e-lib.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define TEXT_SIZE_MAX 32

uint32_t *trace_flag = (uint32_t *) 25165824;
uint32_t *trace_data = (uint32_t *) 25165828;

void core_trace(const char *msg, unsigned value) {
    uint32_t flag = 0;
    typedef struct {
        unsigned value;
        char text[TEXT_SIZE_MAX];
        unsigned padding;
    } message;
    message m;
    strcpy(m.text, msg);
    m.value = value;
    do {
        core_read_shared(trace_flag, &flag, 0, 0, 0);
    } while (!flag);
    core_write_shared(trace_data, &m, 0, 0, sizeof(message));
    flag = 0;
    core_write_shared(trace_flag, &flag, 0, 0, 0);
}

#endif
#else

#define core_trace(msg, value)

#endif

#endif /* __EFELDSPAR_H__ */

