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

#define core_write_local(dst, src, offset, lower, upper) \
    memcpy((dst) + (offset) * sizeof(*src), (src) + (lower), ((upper) - (lower) + 1) * sizeof(*src))

#define core_read_local(src, dst, offset, lower, upper) \
    memcpy((dst) + (lower), (src) + (offset) * sizeof(*dst), ((upper) - (lower) + 1) * sizeof(*dst))

// core operations on shared external memory

#define core_write_shared(dst, src, offset, lower, upper) \
    e_dma_copy(e_emem_config.base + (dst) + (offset) * sizeof(*src), (src) + (lower), ((upper) - (lower) + 1) * sizeof(*src))

#define core_read_shared(src, dst, offset, lower, upper) \
    e_dma_copy((dst) + (lower), e_emem_config.base + (src) + (offset) * sizeof(*dst), ((upper) - (lower) + 1) * sizeof(*dst))

#endif /* __EFELDSPAR_H__ */

