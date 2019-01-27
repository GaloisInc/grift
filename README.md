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

## 1. Setup
We assume you have [ghc](https://www.haskell.org/ghc/download.html) version
8.4.4 or greater, and [cabal](https://www.haskell.org/cabal/download.html)
version 2.4.1.0 or greater. Run a `cabal update` before you begin.

## 2. Clone dependencies

First, clone all the dependencies recursively:
```shell
$ git submodule update --init --recursive
```

## 3. Install softfloat
GRIFT depends on softfloat-hs, which in turn depends on the softfloat
library. To install it on Linux or OSX, run:
```shell
$ cd deps/softfloat-hs
$ make
$ sudo make install
```

The Makefile in `deps/softfloat-hs` should install the softfloat library and
header files to the appropriate locations on both OSX and Linux.

## 4. Build GRIFT
Finally, build GRIFT and all associated executables using cabal v2-build:
```shell
$ cabal v2-build all
```

Running `grift-sim`
===

## Simulating a single ELF file

`grift-sim` is the GRIFT simulation and coverage analysis tool. Try running it
on the executable at `test/fib`:

```shell
cabal v2-run grift-sim -- --halt-pc=0 test/fib/fib
```

This will run the executable and dump the contents of some registers. You should
be able to do this with any executable. If you compile your executable for a
32-bit RISC-V program, you will need to use `-a RV32GC` to switch to 32-bit
mode.

The `--halt-pc=0` option tells `grift-sim` to stop simulating as soon as the
program counter is equal to 0. You can change this value for your particular
application. You can also limit the number of instructions executed in
simulation by using the `--steps` option.

If you want to view a region of memory after simulation instead of the register
files, use the `--mem-dump-start` and `--mem-dump-end` options:

```shell
cabal v2-run grift-sim -- --halt-pc=0 --mem-dump-start=0x11c90 --mem-dump-end=0x11c93 test/fib/fib
```

This will dump the contents of the memory between those memory locations; each
line will be 8 digits (four bytes).

## Analyzing instruction coverage

`grift-sim` can be used to inspect coverage of the RISC-V instruction set.

```shell
cabal v2-run grift-sim -- --halt-pc=0 --inst-coverage=add test/fib/fib
```

This will print out the semantic branching structure of the ADD instruction's
semantics, and will highlight the various branch conditions with colors
indicating the coverage of that instruction throughout execution. We can also
track coverage of the entire instruction set by using `--inst-coverage=all`,
which logs coverage of every instruction and prints out a report after the run.

`grift-sim` can also be used to analyze coverage of an entire test suite. If
`test-suite` is a directory containing only ELF files that we want to simulate,
we can try

```shell
cabal v2-run grift-sim -- --halt-pc=0 --inst-coverage=all test-suite/*
```

which will track coverage of all instructions after running all the tests back
to back. This is how we analyze coverage for the Compliance Working Group's test
suite. Just as for a single ELF file, we can dive into coverage of a particular
instruction by setting ``--inst-coverage=` for whichever instruction we are
interested in.

## Other

Type `cabal v2-run grift-sim -- --help` for a full list of invocation
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
- Coverage analysis and test generation tooling for RISC-V Compliance Task
  Group.

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
