{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : RISCV.Encode
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module defines a function 'encode' that converts an internal 'Instruction' into
a 'BitVector' @32@.
-}

module RISCV.Encode
  ( -- * Functions
    encode
  ) where

import Control.Lens ( (.~), (&) )
import Data.BitVector.Sized
import Data.Parameterized.List

import RISCV.Types
import RISCV.Instruction
import RISCV.InstructionSet
import RISCV.Instruction.Layouts

----------------------------------------
-- Encoding

-- | Encode an 'Instruction' as a 32-bit instruction word.
encode :: InstructionSet arch exts -> Instruction arch fmt -> BitVector 32
encode iset inst = case inst of
  Inst opcode (Operands RRepr (rd :< rs1 :< rs2 :< Nil)) ->
    case opBitsFromOpcode iset opcode of
      ROpBits o f3 f7 -> 0 &
        opcodeLens .~ o   &
        funct3Lens .~ f3  &
        funct7Lens .~ f7  &
        rdLens     .~ rd  &
        rs1Lens    .~ rs1 &
        rs2Lens    .~ rs2
  Inst opcode (Operands IRepr (rd :< rs1 :< imm12 :< Nil)) ->
    case opBitsFromOpcode iset opcode of
      IOpBits o f3 -> 0 &
        opcodeLens .~ o &
        funct3Lens .~ f3 &
        rdLens     .~ rd &
        rs1Lens    .~ rs1 &
        imm12ILens .~ imm12
  Inst opcode (Operands SRepr (rs1 :< rs2 :< imm12 :< Nil)) ->
    case opBitsFromOpcode iset opcode of
      SOpBits o f3 -> 0 &
        opcodeLens .~ o &
        funct3Lens .~ f3 &
        rs1Lens    .~ rs1 &
        rs2Lens    .~ rs2 &
        imm12SLens .~ imm12
  Inst opcode (Operands BRepr (rs1 :< rs2 :< imm12 :< Nil)) ->
    case opBitsFromOpcode iset opcode of
      BOpBits o f3 -> 0 &
        opcodeLens .~ o &
        funct3Lens .~ f3 &
        rs1Lens    .~ rs1 &
        rs2Lens    .~ rs2 &
        imm12BLens .~ imm12
  Inst opcode (Operands URepr (rd :< imm20 :< Nil)) ->
    case opBitsFromOpcode iset opcode of
      UOpBits o -> 0 &
        opcodeLens .~ o &
        rdLens     .~ rd &
        imm20ULens .~ imm20
  Inst opcode (Operands JRepr (rd :< imm20 :< Nil)) ->
    case opBitsFromOpcode iset opcode of
      JOpBits o -> 0 &
        opcodeLens .~ o &
        rdLens     .~ rd &
        imm20JLens .~ imm20
  Inst opcode (Operands XRepr (illBits :< Nil)) ->
    case opBitsFromOpcode iset opcode of
      XOpBits -> 0 & illegalLens .~ illBits
