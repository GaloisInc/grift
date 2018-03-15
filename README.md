# riscv-tools

A general-purpose RISC-V library for Haskell (for use in SSITH)

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
Encoding/decoding are complete for the base ISA (RV32I).

Current TODOS:
  - work out a story for semantics
  - start adding extensions
  - generalize instruction word length to multiples of 16; perhaps just create a data
    wrapper type around BitVector with 32-bit and 16-bit constructors.
  - Work out encoding/decoding for the RV32C/RV64C compressed instructions. Perhaps a
    flag in the encode function for whether the compression is enabled.
  - come up with a story for the various privilege levels (this is semantics-side only)
