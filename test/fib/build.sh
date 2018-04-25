#!/bin/bash
set -x

riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles -S fib.c -o fib32.s
riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles -S fib.c -o fib64.s
riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles -S fib.c -o fib32.s
riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles -S fib.c -o fib64.s

riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles start.s fib64.s -o fib64
riscv64-unknown-elf-objdump -D fib64 >fib64.text
riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles start.s fib32.s -o fib32
riscv32-unknown-elf-objdump -D fib32 >fib32.text

# # spike compilation
riscv64-unknown-elf-gcc fib.c -o fib-spike
riscv64-unknown-elf-objdump -D fib-spike >fib-spike.text
