#include "io.h"

#define N 8192

int main() {
    open_files();
    int32_t buffer[N];
    while (1) {
        if (!read_chunk(buffer, N)) {
            break;
        }
        for (int j = 0; j < N; ++j) {
            buffer[j] = buffer[j] + 16;
        }
        write_chunk(buffer, N);
    }
    close_files();
    return 0;
}

