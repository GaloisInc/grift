# riscv-tools

A general-purpose RISC-V library for Haskell (for use in SSITH)

## Overview

The RISC-V ISA (instruction set architecture) is an open-source ISA developed at
UC Berkeley. riscv-tools is a Haskell library that encodes the core of the ISA:
the encoding and decoding of instructions, as well as their semantics.

## Getting/Building

```
git clone git@github.com:benjaminselfridge/riscv-tools.git
git submodule init
git submodule update
stack build
```

## Running

This library also comes with a simulation environment, riscv-sim. It is built
along with the rest of the library via `stack build`. We use minimal custom
startup code when compiling with GCC; see the examples and build scripts in
test/ for the commands used to build the various executables.

The simulator is invoked thusly:

riscv-sim 1000000 test/fib/fib64

The numeric argument is the number of instructions to execute before
halting. The second argument is the path the executable you wish to run. This
executable must be in the ELF file format. The simulator supports buth RV32IM
and RV64IM.

## Current status

We have semantics for the base ISA and M extension, both 32- and 64-bit. We are
currently developing support for exception handling and the privileged
ISA. Since supervisor and user mode are extensions, we will be adding support
for those in the type system as we progress.
