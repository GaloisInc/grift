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

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

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

import qualified Data.Parameterized.Map as Map
import Data.Parameterized ( type (<=), Pair(Pair) )
import Data.Parameterized.List ( List(Nil, (:<)) )

import GRIFT.BitVector.BVApp ( bvExpr )
import GRIFT.InstructionSet
  ( instructionSet, EncodeMap, InstructionSet, SemanticsMap )
import GRIFT.Semantics ( comment, instSemantics, rawReadCSR )
import GRIFT.Semantics.Utils ( encodeCSR, incrPC, jump, CSR(MEPC) )
import GRIFT.Types

-- | Get the privileged instruction set from an explicit 'RVRepr'.
privmFromRepr :: RVRepr rv -> InstructionSet rv
privmFromRepr rv@(RVRepr RV32Repr _) = withRV rv privm
privmFromRepr rv@(RVRepr RV64Repr _) = withRV rv privm
privmFromRepr _ = mempty

-- | Instruction set for machine-mode privileged architecture.
privm :: KnownRV rv => InstructionSet rv
privm = instructionSet privmEncode privmSemantics

privmEncode :: EncodeMap rv
privmEncode = Map.fromList
  [ Pair Mret (OpBits PRepr (0b00110000001000000000000001110011 :< Nil))
  , Pair Wfi  (OpBits PRepr (0b00010000010100000000000001110011 :< Nil))
  ]

privmSemantics :: ( KnownRV rv, w ~ RVWidth rv, 1 <= w) => SemanticsMap rv
privmSemantics = Map.fromList
  [ Pair Mret $ instSemantics Nil $ do
      comment "Returns from a machine-mode exception handler."

      -- TODO: Need to add the rest of the behavior here.
      let mepc = rawReadCSR (bvExpr $ encodeCSR MEPC)

      jump mepc
  , Pair Wfi $ instSemantics Nil $ do
      comment ""

      -- Doesn't do anything yet.
      incrPC
  ]
