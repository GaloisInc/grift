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

{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-|
Module      : GRIFT.InstructionSet.Priv
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

RISC-V Privileged architecture extensions
-}

module GRIFT.InstructionSet.Priv
  ( privmFromRepr
  ) where

import Data.BitVector.Sized.App
import qualified Data.Parameterized.Map as Map
import Data.Parameterized
import Data.Parameterized.List

import GRIFT.InstructionSet
import GRIFT.InstructionSet.Utils
import GRIFT.Semantics
import GRIFT.Types

-- | Get the privileged instruction set from an explicit 'RVRepr'.
privmFromRepr :: RVRepr rv -> InstructionSet rv
privmFromRepr (RVRepr RV32Repr _) = privm
privmFromRepr (RVRepr RV64Repr _) = privm
privmFromRepr _ = mempty

-- | Instruction set for machine-mode privileged architecture.
privm :: KnownRVWidth rv => InstructionSet rv
privm = instructionSet privmEncode privmSemantics

privmEncode :: EncodeMap rv
privmEncode = Map.fromList
  [ Pair Mret (OpBits PRepr (0b00110000001000000000000001110011 :< Nil))
  , Pair Wfi  (OpBits PRepr (0b00010000010100000000000001110011 :< Nil))
  ]

privmSemantics :: KnownRVWidth rv => SemanticsMap rv
privmSemantics = Map.fromList
  [ Pair Mret $ instSemantics Nil $ do
      comment "Returns from a machine-mode exception handler."

      -- TODO: Need to add the rest of the behavior here.
      let mepc = rawReadCSR (litBV $ encodeCSR MEPC)

      assignPC mepc
  , Pair Wfi $ instSemantics Nil $ do
      comment ""

      -- Doesn't do anything yet.
      incrPC
  ]
