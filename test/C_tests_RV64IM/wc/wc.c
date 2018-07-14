#include <stdio.h>
#include <stdint.h>
#include "riscv_counters.h"

int main (int argc, char *argv[])
{
    int ch, nc = 0, nw = 0, nl = 0;
    int inword = 0;

    while (1) {
	ch = fgetc (stdin);
	if ((ch == EOF) || (ch == 4) || (ch == 0xFF)) {
	    if (inword)
		nw++;
	    break;
	}
	nc++;
	if (ch <= ' ') {
	    if (inword)
		nw++;
	    if (ch == '\n')
		nl++;
	    inword = 0;
	}
	else
	    inword = 1;
    }
    printf (" %2d %2d %2d\n", nl, nw, nc);

    TEST_PASS
    return 0;
}
