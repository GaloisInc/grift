#!/bin/bash
set -x

riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles bench.c -S -o bench32.s
riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles bench.c -S -o bench64.s

riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles start.s bench64.s -o bench64
riscv64-unknown-elf-objdump -D bench64 >bench64.text
riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles start.s bench32.s -o bench32
riscv32-unknown-elf-objdump -D bench32 >bench32.text

# spike compilation
riscv64-unknown-elf-gcc bench.c -o bench-spike
riscv64-unknown-elf-objdump -D bench-spike >bench-spike.text
