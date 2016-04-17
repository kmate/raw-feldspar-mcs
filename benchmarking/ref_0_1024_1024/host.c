#include <e-feldspar.h>
#include <e-hal.h>
#include <e-loader.h>
#include <stdbool.h>
#include <stdint.h>
#include <zbenchmark.h>

off_t la0 = 8192;
off_t la1 = 8196;
off_t la2 = 8200;
off_t sa2 = 16777216;

#define BSZ 1024

int main()
{
    e_epiphany_t group0;
    e_mem_t shm1;

    e_init(0);
    e_reset_system();
    e_open(&group0, 0, 0, 4, 4);
    e_reset_group(&group0);
    e_alloc(&shm1, sa2, BSZ * sizeof(int32_t));

    int zero = 0;
    int one = 1;
    host_write_local(&group0, 0, 0, la0, &zero, 0, 0, 0);
    host_write_local(&group0, 0, 0, la1, &zero, 0, 0, 0);

    e_load("core0.srec", &group0, 0, 0, 1);
    setup_trace();
    open_files();

    int32_t input[BSZ];
    int32_t output[BSZ];
    bool cont;
    while (cont = read_chunk(input, BSZ)) {
        start_iteration();
#ifdef LOCAL
        host_write_local(&group0, 0, 0, la2, input, 0, 0, BSZ - 1);
#else
        host_write_shared(&shm1, input, 0, 0, BSZ - 1);
#endif
        host_write_local(&group0, 0, 0, la0, &one, 0, 0, 0);
        int done = 1;
        do {
            host_read_local(&group0, 0, 0, la1, &done, 0, 0, 0);
        } while (!done);
        host_write_local(&group0, 0, 0, la1, &zero, 0, 0, 0);
#ifdef LOCAL
        host_read_local(&group0, 0, 0, la2, output, 0, 0, BSZ - 1);
#else
        host_read_shared(&shm1, output, 0, 0, BSZ - 1);
#endif
        end_iteration();
        write_chunk(output, BSZ);
    }

    close_files();
    print_time();
    e_free(&shm1);
    e_close(&group0);
    e_finalize();
    return 0;
}
