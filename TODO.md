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

It would be nice if there were a way to encode the mapping from 16-bit to 32-bit
in a similar style that I was able to do so for the 32-bit to the semantics, but
I don't think it's going to as simple as that because we're using entirely
different opcodes in the two cases. The operands may be, though. Might make
sense to create an entirely new 'OpBits' type, or at least new
constructors, and do the decoding in a similar style to keep things
consistent. I could create a concrete association list linking the compressed
OpBits to the uncompressed.

## Privileged architecture

Still need to do some more research here.

## Extensions

I stuck the register width and extensions on the type level, because why not? It
was a cool idea, and it helped simplify the definition of the RVState type
class. Instead of demanding that every implementation provide an InstructionSet,
that is now *automatically* inferred from the type!

SoftFloat -- talk to Flemming when I get to this

## Simulator

I've implemented an ST-based simulation backend, and have yet to run it or
create a good command-line interface for it.

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

# Odds and ends

- Combine Srai/Srli, Ecall/Ebreak into single instructions and remove the 'E'
  format. Modify the decoder to determine the format based only on the opcode
  bits.
- If we do tag the opcodes with extensions, it better be Extensions and not
  Extension (singular). That is, the tag should refer to the execution
  environment in which the thing is executing, and the only restriction placed
  on that tag is the minimum extensions required to support the instruction. If
  we did it this way, we would already know by the constructed instruction set
  that you couldn't decode an instruction that wasn't supported by the execution
  environment.
- Semantics should take into account the fact that rd is hardwired to 0.


