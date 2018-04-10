#!/bin/bash
set -x

riscv64-unknown-elf-gcc -static -nostdlib -nostartfiles test-just-exit64.s -o test-just-exit64
riscv64-unknown-elf-objdump -D test-just-exit64 >test-just-exit64.text
riscv32-unknown-elf-gcc -static -nostdlib -nostartfiles test-just-exit32.s -o test-just-exit32
riscv32-unknown-elf-objdump -D test-just-exit32 >test-just-exit32.text
