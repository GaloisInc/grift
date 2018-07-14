#TOPDIR=..

#include ../Makefile

all: hello.text hello.map hello

hello.map: hello.c
	riscv32-unknown-elf-gcc -g -o hello -Wl,-Map,hello.map hello.c

hello: hello.c
	riscv32-unknown-elf-gcc -g -Ttext=0x00000200 -o hello hello.c

hello.text: hello
	riscv32-unknown-linux-gnu-objdump -dgx hello > hello.text

clean:
	rm hello hello.map hello.text

