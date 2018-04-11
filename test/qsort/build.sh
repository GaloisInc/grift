#!/bin/bash
set -x

riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles -S qsort_main.c -o qsort_main32.s
riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles -S qsort_main.c -o qsort_main64.s

riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles start.s qsort_main64.s -o qsort64
riscv64-unknown-elf-objdump -D qsort64 >qsort64.text
riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles start.s qsort_main32.s -o qsort32
riscv32-unknown-elf-objdump -D qsort32 >qsort32.text

# # spike compilation
riscv64-unknown-elf-gcc qsort_main.c -o qsort-spike
riscv64-unknown-elf-objdump -D qsort-spike >qsort-spike.text
