#!/bin/bash
set -x

riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles -S fact.c -o fact32.s
riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles -S fact.c -o fact64.s
riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles -S fact.c -o fact32.s
riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles -S fact.c -o fact64.s

riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles start.s fact64.s -o fact64
riscv64-unknown-elf-objdump -D fact64 >fact64.text
riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles start.s fact32.s -o fact32
riscv32-unknown-elf-objdump -D fact32 >fact32.text

# # spike compilation
riscv64-unknown-elf-gcc fact.c -o fact-spike
riscv64-unknown-elf-objdump -D fact-spike >fact-spike.text
