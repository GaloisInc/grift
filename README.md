# riscv-tools

A WIP RISC-V library for binary tooling support.

## Overview
The RISC-V ISA (instruction set architecture) is an open-source ISA developed at UC
Berkeley. riscv-tools is a Haskell library that encodes the core of the ISA -- the
encoding and decoding of instructions, as well as their semantics. The intent is to
facilitate easy assembler/disassembler/simulator building.

## Getting/Building

```
git clone git@github.com:benjaminselfridge/riscv-tools.git
git submodule init
git submodule update
stack build
```

## Running

Nothing to run, this is a library.

## Current status
Instruction encoding is complete and seems kind of nice. Next up is going to be
decoding, then some sort of semantics. Perhaps it will be as simple as a list of
low-level operations.
