#include <e-feldspar.h>
#include <e-hal.h>
#include <e-loader.h>
#include <stdbool.h>
#include <stdint.h>
#include <zbenchmark.h>

off_t c0s = 8192; // c0 start
off_t c0f = 8196; // c0 finish
off_t c1s = 8192; // c1 start
off_t c1f = 8196; // c1 finish
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
    host_write_local(&group0, 0, 0, c0s, &zero, 0, 0, 0);
    host_write_local(&group0, 0, 0, c0f, &zero, 0, 0, 0);
    host_write_local(&group0, 0, 1, c1s, &zero, 0, 0, 0);
    host_write_local(&group0, 0, 1, c1f, &zero, 0, 0, 0);

    e_load("core0.srec", &group0, 0, 0, 1);
    e_load("core1.srec", &group0, 0, 1, 1);
    setup_trace();
    open_files();

    int32_t input[BSZ];
    int32_t output[BSZ];
    bool cont;
    while (cont = read_chunk(input, BSZ)) {
        start_iteration();
        host_write_shared(&shm1, input, 0, 0, BSZ - 1);
        host_write_local(&group0, 0, 0, c0s, &one, 0, 0, 0);
        int done = 1;
        do {
            host_read_local(&group0, 0, 1, c1f, &done, 0, 0, 0);
        } while (!done);
        host_write_local(&group0, 0, 1, c1f, &zero, 0, 0, 0);
        host_read_shared(&shm1, output, 0, 0, BSZ - 1);
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

