# riscv-tools TODO

Summary of pending tasks and things to think about.

## Compressed instructions

The manual describes the RV32C extension as a simple embedding of 16-bit vectors
into 32-bit ones. Therefore, we should be able to create a separate module,
called ExtC (or maybe just Compress), that exports two additional functions,
decodeC and encodeC of the following signatures:

decodeC :: BitVector 16 -> Maybe (BitVector 32)
encodeC :: BitVector 32 -> Maybe (BitVector 16)

decodeC returns Nothing if the 16-bit word is not a compressed instruction;
encodeC returns Nothing if the 32-bit word is not compressable. Then, our
encode/decode functions could take a single Bool flag indicating whether or not
compression is enabled; if it is, first call these functions to encode/decode.

## RV32 vs. RV64

It's currently unclear to me whether I should make this something on the type
level. My current position is that we should keep it at the type level, but
since MISA can be altered in some implementations to switch to 32-bit mode, we
definitely need to account for that. In those cases, I can imagine altering the
instruction semantics to still return 64-bit values, but to zero out the higher
order bits. Hard to say what the best way to do it is. This affects the
decoder, and it affects the compressed extension.

Another question is -- should I create separate versions of each extension
(including base) for RV32/RV64? That would be a lot of code duplication. But in
terms of semantics, it's going to be very difficult to get the widths right on
the type level. I think I need to ignore RV64 for the time being.

## Privileged architecture

Still need to do some more research here.

## Extensions

Currently have the Base ISA encoding and semantics defined, as well as the M
extension encodings (no semantics yet). Shouldn't be too hard to add M
semantics. Next up is probably F/D extensions.

SoftFloat -- talk to Flemming when I get to this

## Simulator

I have defined a simple MState class, which provides the minimal definitions
required to simulate RISC-V programs. We should be able to pretty easily
implement a backend for this.

I might add certain functions to this class that allow us to determine XLEN and
the various enabled extensions; on the other hand, this information may be
available in higher privilege modes by accessing the CSRs. One thing I haven't
fully worked out is whether to parameterize everything by Arch (RV32/RV64) on
the type level. One tricky thing about this is that currently, the privileged
architecture (M in particular) can actually dynamically set XLEN to disable RV64
to simulate RV32 without requiring binary translation. These details may be
tricky to work out.

This is probably the next thing to tackle. Part of it will involve writing a
universal step function, with a straightforward fetch/decode/execute. This
should be general and not specific to any particular backend. In order to
accomplish this, I will need to add certain things to the MState type class. One
idea is to just have a function iSet :: m (InstructionSet arch) that returns the
InstructionSet for the state machine.

If compression is enabled, we will factor that into the fetch/decode process and
then give that information to the execFormula function. The process is as
follows:

- Fetch
  - if compression is enabled:
  -   fetch a 16-bit word
  -   if it is a compressed instruction, expand it to a 32-bit word and set ib
  -      := 2
  -   else, ib := 4
  - else: ib := 4
- Decode
  - decode the 32-bit instruction word (perhaps expanded from a 16-bit word)
    - whether an instruction is successfully decoded depends on which
      instructions are enabled
- Execute
  - After successfully decoding the instruction, look up its semantics in the
    appropriate instruction set

## Assembler

All that would really be required for this would be to write a parser for RISC-V
assembly code. I should then be able to use ElfEdit to create Elf files.

Should create a single module for parsing lines of assembly code, as well as
serializing instructions to assembly format.

## Disassembler

Already have a mockup of a disassembler for the ELF format. After I finish the
assembler, I will work on 

## Exporting to other formats

What to do first? Should be fun.
