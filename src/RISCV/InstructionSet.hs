{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : RISCV.Instruction
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Instruction sets
-}

module RISCV.InstructionSet
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

import RISCV.Semantics
import RISCV.Types

-- | Instruction encoding, mapping each opcode to its associated 'OpBits', the bits
-- it fixes in an instruction word.
type EncodeMap arch exts = MapF (Opcode arch exts) OpBits

-- | Reverse of 'EncodeMap'
type DecodeMap arch exts = MapF OpBits (Opcode arch exts)

-- | Maps each opcode to its associated semantics 'Formula'.
type SemanticsMap arch exts = MapF (Opcode arch exts) (Formula arch)

-- | A set of RISC-V instructions. We use this type to group the various instructions
-- into categories based on extension and register width.
data InstructionSet (arch :: BaseArch) (exts :: Extensions)
  = InstructionSet { isEncodeMap    :: !(EncodeMap arch exts)
                   , isDecodeMap    :: !(DecodeMap arch exts)
                   , isSemanticsMap :: !(SemanticsMap arch exts)
                   }

instance Monoid (InstructionSet arch exts) where
  -- RV32 is the default/minimum, so that should be mempty.
  mempty = InstructionSet Map.empty Map.empty Map.empty


  InstructionSet em1 dm1 sm1 `mappend` InstructionSet em2 dm2 sm2
    = InstructionSet (em1 `Map.union` em2) (dm1 `Map.union` dm2) (sm1 `Map.union` sm2)

-- | Construct an instructionSet from an EncodeMap and a SemanticsMap
instructionSet :: EncodeMap arch exts -> SemanticsMap arch exts -> InstructionSet arch exts
instructionSet em = InstructionSet em (transMap em)
  where swap :: Pair (k :: t -> *) (v :: t -> *) -> Pair v k
        swap (Pair k v) = Pair v k
        transMap :: OrdF v => MapF (k :: t -> *) (v :: t -> *) -> MapF v k
        transMap = Map.fromList . map swap . Map.toList

-- | Given an instruction set, obtain the fixed bits of an opcode (encoding)
opBitsFromOpcode :: InstructionSet arch exts -> Opcode arch exts fmt -> OpBits fmt
opBitsFromOpcode is opcode = fromMaybe (error msg) $ Map.lookup opcode (isEncodeMap is)
  where msg = "Opcode " ++ show opcode ++ " does not have corresponding OpBits defined."

-- | Given an instruction set, obtain the opcode from its fixed bits (decoding)
opcodeFromOpBits :: InstructionSet arch exts -> OpBits fmt -> Either (Opcode arch exts X) (Opcode arch exts fmt)
opcodeFromOpBits is opBits =
  maybe (Left Illegal) Right (Map.lookup opBits (isDecodeMap is))

-- | Given an instruction set, obtain the semantics of an opcode
semanticsFromOpcode :: InstructionSet arch exts -> Opcode arch exts fmt -> Formula arch fmt
semanticsFromOpcode is opcode = fromMaybe (error msg) $ Map.lookup opcode (isSemanticsMap is)
  where msg = "Opcode " ++ show opcode ++ " does not have corresponding semantics defined."

