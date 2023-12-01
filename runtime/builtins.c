#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>


int print_int(int i) {
    printf("%d", i);
    return 0;
}

int println_int(int i) {
    printf("%d\n", i);
    return 0;
}

int assert(bool value) {
    if (!value) {
        printf("Assertion failed!\n");
        exit(1);
    }
    return 0;
}
