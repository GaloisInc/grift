{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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

import Control.Lens

import RISCV.Instruction
import RISCV.Instruction.Lens

----------------------------------------
-- Encoding

-- | Encode an 'Instruction' as a 32-bit instruction word.
encode :: Instruction k -> InstWord 2
encode inst = case inst of
  Inst opcode (ROperands rd rs1 rs2) ->
    case opBitsFromOpcode opcode of
      ROpBits o f3 f7 -> 0 &
        opcodeLens .~ o   &
        funct3Lens .~ f3  &
        funct7Lens .~ f7  &
        rdLens     .~ rd  &
        rs1Lens    .~ rs1 &
        rs2Lens    .~ rs2
  Inst opcode (IOperands rd rs1 imm12) ->
    case opBitsFromOpcode opcode of
      IOpBits o f3 -> 0 &
        opcodeLens .~ o &
        funct3Lens .~ f3 &
        rdLens  .~ rd &
        rs1Lens .~ rs1 &
        imm12ILens .~ imm12
  Inst opcode (SOperands rs1 rs2 imm12) ->
    case opBitsFromOpcode opcode of
      SOpBits o f3 -> 0 &
        opcodeLens .~ o &
        funct3Lens .~ f3 &
        rs1Lens .~ rs1 &
        rs2Lens .~ rs2 &
        imm12SLens .~ imm12
  Inst opcode (BOperands rs1 rs2 imm12) ->
    case opBitsFromOpcode opcode of
      BOpBits o f3 -> 0 &
        opcodeLens .~ o &
        funct3Lens .~ f3 &
        rs1Lens .~ rs1 &
        rs2Lens .~ rs2 &
        imm12BLens .~ imm12
  Inst opcode (UOperands rd imm20) ->
    case opBitsFromOpcode opcode of
      UOpBits o -> 0 &
        opcodeLens .~ o &
        rdLens .~ rd &
        imm20ULens .~ imm20
  Inst opcode (JOperands rd imm20) ->
    case opBitsFromOpcode opcode of
      JOpBits o -> 0 &
        opcodeLens .~ o &
        rdLens .~ rd &
        imm20JLens .~ imm20
  Inst opcode (EOperands) ->
    case opBitsFromOpcode opcode of
      EOpBits o eBits -> 0 &
        opcodeLens .~ o &
        eLens .~ eBits
  Inst opcode (XOperands illBits) ->
    case opBitsFromOpcode opcode of
      XOpBits -> 0 & illegalLens .~ illBits
