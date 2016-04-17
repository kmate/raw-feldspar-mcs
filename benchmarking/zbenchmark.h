#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>

// file io ---------------------------------------------------------------------

FILE *in, *out;

void open_files() {
    in  = fopen("../../input.dat",  "rb");
    out = fopen("../../output.dat", "wb");
}

bool read_chunk(int32_t * array, size_t len) {
    if (feof(in)) {
        return false;
    }
    return len == fread(array, sizeof(*array), len, in);
}

void write_chunk(int32_t * array, size_t len) {
    fwrite(array, sizeof(*array), len, out);
}

void close_files() {
    fclose(in);
    fclose(out);
}

// timing ----------------------------------------------------------------------

struct timespec start;
struct timespec end;
unsigned long long nsecs = 0;

#define start_iteration() \
    clock_gettime(CLOCK_MONOTONIC, &start);

#define end_iteration() \
    clock_gettime(CLOCK_MONOTONIC, &end); \
    nsecs += (end.tv_sec - start.tv_sec) * 1e9 + (end.tv_nsec - start.tv_nsec);

#define print_time() \
    printf("%llu\n", nsecs / (unsigned long long)1e6);

// tracing ---------------------------------------------------------------------

#ifdef TRACE
#define TEXT_SIZE_MAX 32

pthread_t trace_thread;

off_t trace_flag_off = 25165824;
off_t trace_data_off = 25165828;

e_mem_t trace_flag_mem;
e_mem_t trace_data_mem;

typedef struct {
    unsigned value;
    char text[TEXT_SIZE_MAX];
    unsigned padding;
} message;

void *trace_main(void *unused) {
    printf("Starting TRACE thread.\n");
    int c = 0;
    for(;;) {
        uint32_t flag = 1;
        message m;
        host_write_shared(&trace_flag_mem, &flag, 0, 0, 0);
        do {
            host_read_shared(&trace_flag_mem, &flag, 0, 0, 0);
        } while (flag);
        host_read_shared(&trace_data_mem, &m, 0, 0, sizeof(message));
        printf("%d) %s: %u\n", ++c, m.text, m.value);
    }
}

void setup_trace() {
    e_alloc(&trace_flag_mem, trace_flag_off, sizeof(uint32_t));
    e_alloc(&trace_data_mem, trace_data_off, sizeof(message) * 100);
    if(pthread_create(&trace_thread, NULL, trace_main, NULL)) {
        fprintf(stderr, "Error creating trace thread\n");
        return;
    }
}

#else

#define setup_trace()

#endif

