{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : RISCV.ExtM
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

RV32M multiply extension
-}

module RISCV.ExtM
  ( m )
  where

import qualified Data.Parameterized.Map as Map
import Data.Parameterized

import RISCV.Instruction
import RISCV.InstructionSet

-- | M extension
m :: InstructionSet 'RV32
m = instructionSet mEncode mSemantics

mEncode :: EncodeMap
mEncode = Map.fromList
  [ -- RV32M
    -- R type
    Pair Mul    (ROpBits 0b0110011 0b000 0b0000001)
  , Pair Mulh   (ROpBits 0b0110011 0b001 0b0000001)
  , Pair Mulhsu (ROpBits 0b0110011 0b010 0b0000001)
  , Pair Mulhu  (ROpBits 0b0110011 0b011 0b0000001)
  , Pair Div    (ROpBits 0b0110011 0b100 0b0000001)
  , Pair Divu   (ROpBits 0b0110011 0b101 0b0000001)
  , Pair Rem    (ROpBits 0b0110011 0b110 0b0000001)
  , Pair Remu   (ROpBits 0b0110011 0b111 0b0000001)
  ]

mSemantics :: SemanticsMap 'RV32
mSemantics = undefined
