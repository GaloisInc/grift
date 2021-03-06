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
memory, etc.), but as symbolic expressions in a bitvector expression
language. This extra layer of representation, while sub-optimal for fast
simulation, facilitates the library's use as a general-purpose encoding of the
semantics, and makes *GRIFT* a general-purpose, "golden reference" model that
can be easily translated into the syntax of other tools by providing minimal
pretty printers, written in Haskell, for the underlying bitvector expression
language. Having explicit semantic data for each instruction also facilitates
the library's incorporation with other Haskell-based tooling, such as coverage
analysis (where a notion of coverage is encoded in the same bitvector language
as the semantics), binary analysis, and verification, both within and without
the Haskell programming environment.

Build instructions
===

We assume you have the Haskell build tool stack installed on your system.

First, clone all the dependencies recursively:
```shell
$ git submodule update --init --recursive
```

GRIFT depends on softfloat-hs, which in turn depends on the softfloat
library. To install it on Linux of OSX, run:
```shell
$ cd deps/softfloat-hs
$ make
$ sudo make install
```

Finally, build GRIFT using cabal new-build:
```shell
$ cabal new-build grift
```

Running
===

Type `cabal new-run grift-sim -- --help` for a full list of invocation
instructions.

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

We currently support RV{32|64}IMAFDC, with M-mode privileged instructions. Exceptions
are modeled incompletely; the bare minimum to run user-level code with traps is
supported. We are passing all the following tests in simulation:

- rv32ui
- rv64ui
- rv32um
- rv64um
- rv32ua (except lrsc)
- rv64ua (except lrsc)
- rv32uf
- rv64uf
- rv32ud
- rv64ud
- rv32uc
- rv64uc

We also now provide a command-line option to select a RISC-V architecture
variant. Not every combination is possible -- legal ones begin with "RV32" or "RV64"
and end in "I", "IM", "IMA", "IMAF", "G", or "GC".

We also support the --inst-coverage=OPCODE command-line option for testing coverage
of a particular instruction, based on that instruction's semantic if/then/else
branching structure. This type of coverage analysis is flexible enough to accomodate
most kinds of coverage, not necessarily based purely on the semantics of the
instruction in question. However, this would require a bit of hand-coding because we
do not support an option in grift-sim to specify exactly what kind of coverage to
keep track of. This is an area of active development, and we are open to suggestions
on this.

Known issues
===
Building softfloat on Darwin currently has some issues -- for whatever reason, some
of the conversion functions to not work correctly in some of the corner
cases. Therefore, some of GRIFT's behavior may not be entirely correct unless it is
run in Linux. We are investigating this issue.

Other information
===

* contact: benselfridge@galois.com
