# GRIFT ISA specification reading guide

GRIFT is a Haskell library that codifies and formalizes the RISC-V ISA. It comes with
out-of-the-box support for simulation and coverage analysis.

The GRIFT source code is intended to be highly readable, even by non-Haskell experts,
to understand the semantics of the various instructions. GRIFT encodes instruction
semantics in a flexible, extensible embedded DSL (Domain Specific Language) within
the Haskell programming language. This language contains constructs for bit vector
operations and manipulation, along with constructs for accessing RISC-V machine state
(general & floating point registers, CSRs, memory, etc.).

There are two methods for ``reading'' GRIFT as an ISA manual -- reading the
pretty-printed GRIFT DSL specification of a particular instruction using the
`grift-doc` tool, and reading the source code itself. We outline both these methods
below.

## Reading instruction encoding & semantics with `grift-doc`

To view the encoding and semantics of the `slli` instruction:

```shell
cabal v2-run grift-doc -- slli
```

For some instructions, the semantics are different depending on exactly which
extensions are enabled and/or what the register width is, so we can set the RISC-V
configuration with the `-a` option:

```shell
cabal v2-run grift-doc -- -a RV32I slli
```

## Reading instruction encoding & semantics directly from source
