# riscv-tools

A general-purpose RISC-V library for Haskell (for use in SSITH)

## Overview
The RISC-V ISA (instruction set architecture) is an open-source ISA developed at UC
Berkeley. riscv-tools is a Haskell library that encodes the core of the ISA -- the
encoding and decoding of instructions, as well as their semantics. 

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
Encoding/decoding are complete for the base ISA and M extension
(RV32IM/RV64IM). 
