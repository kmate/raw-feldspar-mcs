#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define N      8192
#define ROUNDS 256

int main() {
    FILE *out = fopen("input.dat",  "wb");

    srand(time(NULL));

    int32_t buffer[N];
    for (int i = 0; i < ROUNDS; ++i) {
        bool lastRound = i + 1 == ROUNDS;
        for (int j = 0; j < N; ++j) {
            buffer[j] = lastRound && j + 1 == N ? 0 : rand() % 10000;
        }
        fwrite(buffer, sizeof(int32_t), N, out);
    }

    fclose(out);
    return 0;
}

