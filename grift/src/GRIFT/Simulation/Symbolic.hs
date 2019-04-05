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

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

{-|
Module      : GRIFT.Simulation.Symbolic
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This is a highly experimental type class for symbolically simulating RISC-V code,
using the SBV library for symbolic execution.
-}

module GRIFT.Simulation.Symbolic where

import Control.Lens ( (^.) )
import Data.BitVector.Sized
import Data.BitVector.Sized.App
import Data.BitVector.Sized.Float.App
import Data.Foldable
import Data.Parameterized
import Data.Parameterized.List
import Data.Traversable
import qualified GHC.TypeLits as T
import Prelude hiding ((!!))

import GRIFT.Decode
import GRIFT.InstructionSet
import GRIFT.InstructionSet.Known
import GRIFT.Semantics
import GRIFT.Semantics.Expand
import GRIFT.Semantics.Utils
import GRIFT.Types

-- | State monad for symbolic execution of RISC-V code. Currently does not support
-- floating point operations.
class Monad m => RVStateM m (rv :: RV) | m -> rv where
  -- | Get the RISC-V configuration.
  getRV :: m (RVRepr rv)

  -- | Get the current PC.
  getPC   :: m (BitVector (RVWidth rv))
  -- | Get the value of a register. This function shouldn't ever be called with an
  -- argument of 0, so there is no need to hardwire it to 0 in an instance of this
  -- class.
  getGPR  :: BitVector 5 -> m (BitVector (RVWidth rv))
  -- | Read some number of bytes from memory.
  getMem  :: NatRepr bytes -> BitVector (RVWidth rv) -> m (BitVector (8 T.* bytes))
  -- | Get the value of a CSR.
  getCSR  :: BitVector 12 -> m (BitVector (RVWidth rv))
  -- | Get the current privilege level.
  getPriv :: m (BitVector 2)

  -- | Set the PC.
  setPC   :: BitVector (RVWidth rv) -> m ()
  -- | Write to a register.
  setGPR  :: BitVector 5 -> BitVector (RVWidth rv) -> m ()
  -- | Write a single byte to memory.
  setMem  :: NatRepr bytes -> BitVector (RVWidth rv) -> BitVector (8 T.* bytes) -> m ()
  -- | Write to a CSR.
  setCSR  :: BitVector 12 -> BitVector (RVWidth rv) -> m ()
  -- | Set the privilege level.
  setPriv :: BitVector 2 -> m ()

  -- | Condition for halting simulation.
  isHalted :: m Bool
  -- | Log the execution of a particular instruction.
  logInstruction :: InstructionSet rv -> Instruction rv fmt -> Integer -> m ()

