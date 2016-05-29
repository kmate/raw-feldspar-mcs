#include <stdlib.h>

#include "feldspar-parallella.h"

#ifndef __epiphany__

#include <stdint.h>
#include <time.h>

static inline int wait(uint32_t nsec)
{
  struct timespec ts = { .tv_sec = 0, .tv_nsec = nsec };
  return nanosleep(&ts, NULL);
}

void init_host_chan(host_chan_t *chan,
                    e_epiphany_t *g, e_coreid_t r, e_coreid_t c,
                    e_mem_t *buf, off_t is_open_o, off_t is_full_o) {
  chan->g = g;
  chan->r = r;
  chan->c = c;
  chan->buf = buf;
  chan->is_open = is_open_o;
  chan->is_full = is_full_o;
  bool is_open[1] = { true };
  host_write_local(g, r, c, is_open_o, is_open, 0, 0, 0);
  bool is_full[1] = { false };
  host_write_local(g, r, c, is_full_o, is_full, 0, 0, 0);
}

void init_core_chan(e_epiphany_t *g, e_coreid_t r, e_coreid_t c,
                    off_t is_open_o, off_t is_full_o) {
  bool is_open[1] = { true };
  host_write_local(g, r, c, is_open_o, is_open, 0, 0, 0);
  bool is_full[1] = { false };
  host_write_local(g, r, c, is_full_o, is_full, 0, 0, 0);
}

bool _host_write_h2c(host_chan_t chan, void *src, size_t off, size_t len) {
  // wait for empty space
  bool is_full[1] = { true };
  do {
    bool is_open[1] = { true };
    host_read_local(chan.g, chan.r, chan.c, chan.is_open, is_open, 0, 0, 0);
    if (!*is_open) {
      // do not wait for a closed channel to get empty
      return false;
    }
    host_read_local(chan.g, chan.r, chan.c, chan.is_full, is_full, 0, 0, 0);
  } while (*is_full && !wait(HOST_CHANNEL_POLL_NSEC));
  // write item and set channel full
  host_write_shared(chan.buf, src, 0, off, off + len - 1);
  *is_full = true;
  host_write_local(chan.g, chan.r, chan.c, chan.is_full, is_full, 0, 0, 0);
  return true;
}

bool _host_read_c2h(host_chan_t chan, void *dst, size_t off, size_t len) {
  // wait for an item
  bool is_full[1] = { false };
  do {
    bool is_open[1] = { true };
    host_read_local(chan.g, chan.r, chan.c, chan.is_open, is_open, 0, 0, 0);
    if (!*is_open) {
      // do not wait for a closed channel to be filled
      return false;
    }
    host_read_local(chan.g, chan.r, chan.c, chan.is_full, is_full, 0, 0, 0);
  } while (!*is_full && !wait(HOST_CHANNEL_POLL_NSEC));
  // read item and set channel empty
  host_read_shared(chan.buf, dst, 0, off, off + len - 1);
  *is_full = false;
  host_write_local(chan.g, chan.r, chan.c, chan.is_full, is_full, 0, 0, 0);
  return true;
}

void host_close_chan(host_chan_t chan) {
  bool is_open[1] = { false };
  host_write_local(chan.g, chan.r, chan.c, chan.is_open, is_open, 0, 0, 0);
}

#else /* __epiphany__ */

#include <stdint.h>

bool _core_write_c2h(volatile void *const buf,
                     volatile bool *const is_open,
                     volatile bool *const is_full,
                     void *src, size_t off, size_t len) {
  do {
    if (!*is_open) {
      // do not wait for a closed channel to get empty
      return false;
    }
  } while (*is_full);
  core_write_shared(buf, src, 0, off, off + len - 1);
  *is_full = true;
  return true;
}

bool _core_read_h2c(volatile void *const buf,
                    volatile bool *const is_open,
                    volatile bool *const is_full,
                    void *dst, size_t off, size_t len) {
  do {
    if (!*is_open) {
      // do not wait for a closed channel to be filled
      return false;
    }
  } while (!*is_full);
  core_read_shared(buf, dst, 0, off, off + len - 1);
  *is_full = false;
  return true;
}

bool _core_write_c2c(volatile void *const buf,
                     volatile bool *const is_open,
                     volatile bool *const is_full,
                     void *src, size_t off, size_t len) {
  do {
    if (!(*is_open)) {
      // do not wait for a closed channel to get empty
      return false;
    }
  } while (*is_full);
  core_write_local(buf, src, 0, off, off + len - 1);
  *is_full = true;
  return true;
}

bool _core_read_c2c(volatile void *const buf,
                    volatile bool *const is_open,
                    volatile bool *const is_full,
                    void *dst, size_t off, size_t len) {
  do {
    if (!*is_open) {
      // do not wait for a closed channel to be filled
      return false;
    }
  } while (!*is_full);
  core_read_local(buf, dst, 0, off, off + len - 1);
  *is_full = false;
  return true;
}

void core_close_chan(volatile void *const buf,
                     volatile bool *const is_open,
                     volatile bool *const is_full) {
  *is_open = false;
}

// based on the epiphany-bsp library
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

#endif /* __epiphany__ */
