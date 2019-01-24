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
`grift-sim` tool, and reading the source code itself. We outline both these methods
below.

## Reading instruction semantics with `grift-sim`

## Reading instruction semantics directly from source
