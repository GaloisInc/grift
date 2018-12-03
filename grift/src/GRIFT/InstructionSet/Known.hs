{-
This file is part of GRIFT (Galois RISC-V ISA Formal Tools).

GRIFT is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GRIFT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero Public License for more details.

You should have received a copy of the GNU Affero Public License
along with GRIFT.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : GRIFT.InstructionSet.Known
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module provides a function to compute the instruction set, complete with
encoding/decoding information and semantics, from a particular RISC-V configuration
(a data kind of kind RV).
-}

module GRIFT.InstructionSet.Known
  ( -- * RISC-V Base ISA and extensions
    knownISet
  , knownISetWithRepr
  ) where

import Data.Parameterized

import GRIFT.InstructionSet
import GRIFT.InstructionSet.Base
import GRIFT.InstructionSet.A
import GRIFT.InstructionSet.FD
import GRIFT.InstructionSet.M
import GRIFT.InstructionSet.Priv
import GRIFT.Types

-- | Infer the current instruction set.
knownISet :: forall rv . KnownRV rv => InstructionSet rv
knownISet = knownISetWithRepr knownRepr

-- | Infer an instruction set from an explicit 'RVRepr'.
knownISetWithRepr :: RVRepr rv -> InstructionSet rv
knownISetWithRepr rvRepr =
  let baseset = baseFromRepr rvRepr
      privset = privmFromRepr rvRepr -- TODO
      mset = mFromRepr rvRepr
      aset = aFromRepr rvRepr
      fset = fdFromRepr rvRepr
  in baseset <> privset <> mset <> aset <> fset
