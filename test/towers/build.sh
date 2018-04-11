#!/bin/bash
set -x

riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles -S towers_main.c -o towers_main32.s
riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles -S towers_main.c -o towers_main64.s

riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles start.s towers_main64.s -o towers64
riscv64-unknown-elf-objdump -D towers64 >towers64.text
riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles start.s towers_main32.s -o towers32
riscv32-unknown-elf-objdump -D towers32 >towers32.text

# # spike compilation
riscv64-unknown-elf-gcc towers_main.c -o towers-spike
riscv64-unknown-elf-objdump -D towers-spike >towers-spike.text
