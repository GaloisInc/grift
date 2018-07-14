#include <errno.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <sys/time.h>
#include "ns16550.h"

#ifndef __linux__

int read(int file, char *ptr, int len) {
  int todo;
  if(len == 0)
    return 0;
#ifdef CONSOLE_UART
  *ptr++ = ns16550_rxchar();
  for(todo = 1; todo < len; todo++) {
    if (!ns16550_rxready())
      break;
    *ptr++ = ns16550_rxchar();
  }
#endif
  return todo;
}

int write(int file, char *ptr, int len) {
  int todo;

#ifdef CONSOLE_UART
  if (file == 1) {
    for (todo = 0; todo < len; todo++) {
      ns16550_txchar (*ptr++);
    }
  }
#endif
  return len;
}

#define CLOCK_PERIOD  (10000000)

int gettimeofday(struct timeval *ptimeval, void *ptimezone)
{
    if (ptimeval)
    {
	long long tv;
#if __riscv_xlen == 64
	asm ("rdtime %0" : "=r" (tv));
#else
	unsigned int tvh;
	unsigned int tvl;
	asm ("rdtime %0;"
	    "rdtimeh %1 " : "=r" (tvl), "=r" (tvh));
	tv = ((long long)tvh) << 32 | tvl;
#endif
	ptimeval->tv_sec = tv / CLOCK_PERIOD;
	ptimeval->tv_usec = tv % CLOCK_PERIOD / (CLOCK_PERIOD / 1000000);
    }

    return 0;
}

unsigned int sleep(unsigned int seconds)
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    seconds += tv.tv_sec;

    while (tv.tv_sec < seconds)
	gettimeofday(&tv, NULL);

    return 0;
}

#endif
