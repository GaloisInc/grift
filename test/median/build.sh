#!/bin/bash
set -x

riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles -S median_main.c -o median_main32.s
riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles -S median_main.c -o median_main64.s
riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles -S median.c -o median32.s
riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles -S median.c -o median64.s

riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles start.s median64.s median_main64.s -o median64
riscv64-unknown-elf-objdump -D median64 >median64.text
riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles start.s median32.s median_main32.s -o median32
riscv32-unknown-elf-objdump -D median32 >median32.text

# # spike compilation
riscv64-unknown-elf-gcc median.c median_main.c -o median-spike
riscv64-unknown-elf-objdump -D median-spike >median-spike.text
