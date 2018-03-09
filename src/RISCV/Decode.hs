{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : RISCV.Decode
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module defines a function 'decode' that converts a 'BitVector' @32@ an internal
'Instruction'.
-}

module RISCV.Decode where

import Data.BitVector.Sized
import Data.Parameterized.Some

import RISCV.Instruction

decode :: BitVector 32 -> Some Instruction
decode bvec = case (opcodeBits, funct3Bits, funct7Bits, eBits) of
  -- J type
  (0b1101111, _, _, _) -> Some $ Inst Jal (getJOperands bvec)
  -- E type
  (0b1110011, _, _, 0b0000000000000000000000000) -> Some $ Inst Ecall  EOperands
  (0b1110011, _, _, 0b0000000000010000000000000) -> Some $ Inst Ebreak EOperands
  _ -> Some $ Inst Illegal (XOperands bvec)
  where dBits = getDecodeBits bvec
        opcodeBits = bvIntegerS (dOpcode dBits)
        funct3Bits = bvIntegerS (dFunct3 dBits)
        funct7Bits = bvIntegerS (dFunct7 dBits)
        eBits      = bvIntegerS (dEBits  dBits)

data DecodeBits = DBits { dOpcode :: BitVector 7
                        , dFunct3 :: BitVector 3
                        , dFunct7 :: BitVector 7
                        , dEBits  :: BitVector 25
                        }

getDecodeBits :: BitVector 32 -> DecodeBits
getDecodeBits bvec = DBits
  { dOpcode = bvExtract 0  bvec
  , dFunct3 = bvExtract 12 bvec
  , dFunct7 = bvExtract 25 bvec
  , dEBits  = bvExtract 7  bvec
  }

getROperands :: BitVector 32 -> Operands 'R
getROperands = undefined

getIOperands :: BitVector 32 -> Operands 'I
getIOperands = undefined

getSOperands :: BitVector 32 -> Operands 'S
getSOperands = undefined

getBOperands :: BitVector 32 -> Operands 'B
getBOperands = undefined

getUOperands :: BitVector 32 -> Operands 'U
getUOperands = undefined

getJOperands :: BitVector 32 -> Operands 'J
getJOperands = undefined
