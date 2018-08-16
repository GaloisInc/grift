GRIFT - Galois RISC-V ISA Formal Tools
===

copyright (c) Galois Inc. 2018

*Galois RISC-V ISA Formal Tools* (hereafter, *GRIFT*) is part of the BESSPIN
software suite, developed by Galois, Inc. It contains a concrete representation
of the semantics of the RISC-V instruction set, along with an elegant
encoding/decoding mechanism, and simulation and analysis front-ends. It is
intended for broad use in the RISC-V community - simulation, binary analysis,
and software & hardware verification/validation are all current and/or potential
future uses for *GRIFT*, and we have designed it specifically with these broad
application domains in mind.

*GRIFT* differs from other Haskell-based RISC-V formalizations in its coding
style (using highly dependently-typed GHC Haskell) and some of its foundational
design decisions. Its primary use is as a library, providing mechanisms for the
encoding/decoding of instructions, as well as running RISC-V programs in
simulation. However, the semantics of the instructions themselves are
represented, not as Haskell functions on a RISC-V machine state (registers, PC,
memory, etc.), but as symbolic expressions in a general-purpose bitvector
expression language. This extra layer of representation, while sub-optimal for
fast simulation, facilitates the library's use as a general-purpose encoding of
the semantics, and makes *GRIFT* a general-purpose, "golden reference" model
that can be easily translated into the syntax of other tools by providing
minimal pretty printers, written in Haskell, for the underlying bitvector
expression language. Having explicit semantic data for each instruction also
facilitates the library's incorporation with other Haskell-based tooling, such
as coverage analysis (where a notion of coverage is encoded in the same
bitvector language as the semantics), binary analysis, and verification, both
within and without the Haskell programming environment.

Requirements
===

The following are a list of mandatory and secondary requirements for *GRIFT*.

# Mandatory Requirements

## General

- Must represent semantics of all RISC-V behavior (instructions and exceptional
  behavior) in a manipulatable and inspectable embedded bitvector expression
  language.
- Must have a type-level representation of the major aspects of the RISC-V
  feature model: register width and implemented extensions. To run in
  simulation, it should be enough to specify this information at the *type
  level*, and have the appropriate instance of RISC-V automatically.

## RISC-V support

- Must support RV32G/RV64G.
- Must support all privilege modes (M, S, U), modeling exceptional behavior
  accurately and completely.
- Must capture all other "customizable" aspects of the ISA (e.g. misaligned
  accesses in hardware).

## Simulation

- Must be able to run all code compiled by the RISC-V GCC toolchain (including,
  but not limited to, booting the Linux kernel). However, performance in
  simulation is explicitly *not* a concern.
- Must pass all relevant tests from riscv-tests and riscv-compliance test
  suites.
- Must integrate with SVF bisimulation tooling for hardware validation.

## Documentation

- Core Haskell code must be cleanly Haddock-documented.
- Instruction semantics should pretty-print to pseudocode in a readable form, in
  the style of the RISC-V Reader (Appendix A) pseudocode.

# Secondary Requirements

- Coverage analysis and test generation tooling for RISC-V Compliance Task
  Group.
- Straightforward integration with other languages, tools, and frameworks (Coq,
  Verilog, ...)
- Cabal-driven test suite incorporating riscv-tests and riscv-compliance test
  suites (among other tests)

Current Status
===

We currently support RV{32|64}IMA, with M-mode privileged
instructions. Exceptions are modeled incompletely; the bare minimum to run
user-level code with traps is supported.

Coming soon: support for floating point F/D extensions. I have created Haskell
bindings to John Hauser's SoftFloat library, which we will use for simulation
backends, and I am working on the subtleties of how to elegantly use Haskell's
type system to conditionally support the floating point registers (this is the
only extension for which there is additional state that we are supporting).

Coming soon: configuration argument or file that lets you configure the GRIFT
simulator to a particular RISC-V instance: RV32IM, RV64IMAFD, etc. Ultimately,
we will support all configuration options in this manner.

Other information
===

* contact: benselfridge@galois.com
