#!/bin/bash
set -x

riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles -S multiply_main.c -o multiply_main32.s
riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles -S multiply.c -o multiply32.s
riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles -S multiply_main.c -o multiply_main64.s
riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles -S multiply.c -o multiply64.s

riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles start.s multiply64.s multiply_main64.s -o multiply64
riscv64-unknown-elf-objdump -D multiply64 >multiply64.text
riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles start.s multiply32.s multiply_main32.s -o multiply32
riscv32-unknown-elf-objdump -D multiply32 >multiply32.text

# # spike compilation
riscv64-unknown-elf-gcc multiply_main.c  multiply.c -o multiply-spike
riscv64-unknown-elf-objdump -D multiply-spike >multiply-spike.text
