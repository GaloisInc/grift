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

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}

{-|
Module      : GRIFT.Instruction
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Instruction sets
-}

module GRIFT.InstructionSet
  ( -- * Instruction sets
    InstructionSet(..)
  , instructionSet
  , EncodeMap, DecodeMap, SemanticsMap
    -- * Using instruction sets
  , opcodeFromOpBits
  , opBitsFromOpcode
  , semanticsFromOpcode
  ) where

import Data.Maybe (fromMaybe)
import Data.Parameterized
import qualified Data.Parameterized.Map as Map
import Data.Parameterized.Map (MapF)

import GRIFT.Semantics
import GRIFT.Types

-- | Instruction encoding, mapping each opcode to its associated 'OpBits', the bits
-- it fixes in an instruction word.
type EncodeMap (rv :: RV) = MapF (Opcode rv) OpBits

-- | Reverse of 'EncodeMap'
type DecodeMap (rv :: RV) = MapF OpBits (Opcode rv)

-- | Maps each opcode to its associated semantics 'Semantics'.
type SemanticsMap (rv :: RV) = MapF (Opcode rv) (InstSemantics rv)

-- | A set of RISC-V instructions. We use this type to group the various instructions
-- into categories based on extension and register width.
data InstructionSet (rv :: RV)
  = InstructionSet { isEncodeMap    :: !(EncodeMap rv)
                   , isDecodeMap    :: !(DecodeMap rv)
                   , isSemanticsMap :: !(SemanticsMap rv)
                   }

instance Semigroup (InstructionSet rv) where
  InstructionSet em1 dm1 sm1 <> InstructionSet em2 dm2 sm2
    = InstructionSet (em1 `Map.union` em2) (dm1 `Map.union` dm2) (sm1 `Map.union` sm2)

instance Monoid (InstructionSet rv) where
  -- RV32 is the default/minimum, so that should be mempty.
  mempty = InstructionSet Map.empty Map.empty Map.empty

  mappend = (<>)

-- | Construct an instructionSet from an EncodeMap and a SemanticsMap
instructionSet :: EncodeMap rv -> SemanticsMap rv -> InstructionSet rv
instructionSet em = InstructionSet em (transMap em)
  where swap (Pair k v) = Pair v k
        transMap = Map.fromList . map swap . Map.toList

-- | Given an instruction set, obtain the fixed bits of an opcode (encoding)
opBitsFromOpcode :: InstructionSet rv -> Opcode rv fmt -> OpBits fmt
opBitsFromOpcode is opcode = fromMaybe (error msg) $ Map.lookup opcode (isEncodeMap is)
  where msg = "Opcode " ++ show opcode ++ " does not have corresponding OpBits defined."

-- | Given an instruction set, obtain the opcode from its fixed bits (decoding)
opcodeFromOpBits :: InstructionSet rv -> OpBits fmt -> Either (Opcode rv X) (Opcode rv fmt)
opcodeFromOpBits is opBits =
  maybe (Left Illegal) Right (Map.lookup opBits (isDecodeMap is))

-- | Given an instruction set, obtain the semantics of an opcode
semanticsFromOpcode :: InstructionSet rv -> Opcode rv fmt -> InstSemantics rv fmt
semanticsFromOpcode is opcode = fromMaybe (error msg) $ Map.lookup opcode (isSemanticsMap is)
  where msg = "Opcode " ++ show opcode ++ " does not have corresponding semantics defined."
