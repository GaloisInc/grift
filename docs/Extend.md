# GRIFT ISA specification extension guide

## Overview

When adding a new extension (we call refer to the new extension as `X` throughout
this document for simplicity's sake), there are two GRIFT source files that will
definitely need to be modified:

- [grift/src/GRIFT/Types.hs](https://github.com/GaloisInc/grift/blob/master/grift/src/GRIFT/Types.hs)
- [grift/src/GRIFT/Decode.hs](https://github.com/GaloisInc/grift/blob/master/grift/src/GRIFT/Decode.hs)
- [grift/src/GRIFT/InstructionSet/Known.hs](https://github.com/GaloisInc/grift/blob/master/grift/src/GRIFT/InstructionSet/Known.hs)

If the instruction semantics for your extension can be adequately expressed with the
existing GRIFT semantics, then you may not need to touch any other source
files. However, if you need to augment the semantics DSL, you will need to modify
these files:

- [grift/src/GRIFT/Semantics.hs](https://github.com/GaloisInc/grift/blob/master/grift/src/GRIFT/Semantics.hs)
- [grift/src/GRIFT/Simulation.hs](https://github.com/GaloisInc/grift/blob/master/grift/src/GRIFT/Simulation.hs)

You will also need to add a file containing the encodings and semantics for your
extension:

- grift/src/GRIFT/InstructionSet/X.hs

If you want to have support with `grift-sim` and `grift-doc`, you will need to modify

- [grift-sim/src/GriftSim.hs](https://github.com/GaloisInc/grift/blob/master/grift-sim/src/GriftSim.hs)
- [grift-doc/src/GriftDoc.hs](https://github.com/GaloisInc/grift/blob/master/grift-doc/src/GriftDoc.hs)
