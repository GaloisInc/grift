# riscv-tools TODO

Summary of pending tasks and things to think about.

## Decoding and encoding

Right now, the decoding and encoding are very nice, but purely
operational/functional. I am considering a more denotational style in correspondence
with my semantics representation. The decoding itself is actually a part of the
semantics. It would be incredibly cool to capture the entire fetch/decode/execute
process in a purely denotational way. Right now I have a denotational representation
for execution of each instruction, and I could easily imagine doing the same for
instruction fetch. But decoding is not currently captured in this way, and I imagine
it could be useful to do so.

## Compressed instructions

I can pretty easily add support for the C extension operationally, albeit in a
somewhat more ad-hoc way than the very nice encoding/decoding style I'm using for the
base ISA.

However, I'm considering holding off on implementing this until I determine whether
it would be useful to capture the decoding in a more declarative style. If I were to
do that, I would want to skip the operational style-encoding for C entirely, and just
do a denotational approach.

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
assembler, I will work on this, just as part of the tool suite to go along with the
library.

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


