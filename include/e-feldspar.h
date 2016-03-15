#ifndef __EFELDSPAR_H__
#define __EFELDSPAR_H__

#include <e-hal.h>

#define e_fetch(g, r, c, dst, src, offset, lower, upper) \
    e_write(g, r, c, dst + offset * sizeof(*src), src + lower, (upper - lower + 1) * sizeof(*src))

#define e_flush(g, r, c, src, dst, offset, lower, upper) \
    e_read(g, r, c, src + offset * sizeof(*dst), dst + lower, (upper - lower + 1) * sizeof(*dst))

#endif /* __EFELDSPAR_H__ */
