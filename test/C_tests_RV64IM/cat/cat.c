#include <stdio.h>
#include <stdint.h>
#include "riscv_counters.h"

int main (int argc, char *argv[])
{
    int ch;

    while (1) {
	ch = fgetc (stdin);
	if ((ch == EOF) || (ch == 4) || (ch == 0xFF))
	    break;
	fputc (ch, stdout);
    }

    TEST_PASS
    return 0;
}
