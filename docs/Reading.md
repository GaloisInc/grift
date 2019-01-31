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

There are several spots in the code that contain information on the instruction
encodings -- piecing the information together can be a bit confusing, which is why we
recommend using the `grift-doc` tool for a quick visualization of how the
instruction is encoded. If you need in-depth information on GRIFT's representation of
instruction encodings, see the "How to Extend" guide in this directory.

The semantics of the instructions are contained in various source files in the
`grift/src/GRIFT/InstructionSet/` directory. There is one file per extension (plus
one called `Known.hs` which you can ignore). For instance, to look up the semantics
of the `DIV` instruction, we look at the file `M.hs` go to the definition of
`mSemantics`, containing the semantics for the instructions in the 32-bit `M`
extension. We find the following:

```
  , Pair Div $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as two's complement numbers."
      comment "Writes the quotient to r[d]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      let q = x_rs1 `quotsE` x_rs2

      assignGPR rd $ iteE (x_rs2 `eqE` litBV 0) (litBV (-1)) q
      incrPC
```

The semantics should be self-explanatory for most of the instructions.
