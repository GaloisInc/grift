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

import Data.BitVector.Sized

import RISCV.Instruction

----------------------------------------
-- Encoding
-- TODO: consider annotating every single variable with the type in each case for
-- clarity.
-- | Encode an RV32I instruction as a 32-bit word.
encode :: forall (k :: Format). Instruction k -> BitVector 32
encode (Inst opcode (ROperands rd  rs1 rs2)) =
  -- R type
  case opBitsFromOpcode opcode of
    ROpBits opcodeBits funct3 funct7 ->
      funct7 <:>
      rs2    <:>
      rs1    <:>
      funct3 <:>
      rd     <:>
      opcodeBits
encode (Inst opcode (IOperands rd rs1 imm)) =
  -- I type
  case opBitsFromOpcode opcode of
    IOpBits opcodeBits funct3 ->
      imm    <:>
      rs1    <:>
      funct3 <:>
      rd     <:>
      opcodeBits
encode (Inst opcode (SOperands rs1 rs2 imm)) =
  -- S type
  case opBitsFromOpcode opcode of
    SOpBits opcodeBits funct3 ->
      (bvExtract 5 imm :: BitVector 7) <:>
      rs2                              <:>
      rs1                              <:>
      funct3                           <:>
      (bvExtract 0 imm :: BitVector 5) <:>
      opcodeBits
encode (Inst opcode (BOperands rs1 rs2 imm)) =
  -- B type
  case opBitsFromOpcode opcode of
    BOpBits opcodeBits funct3 ->
      (bvExtract 11 imm :: BitVector 1) <:>
      (bvExtract 4  imm :: BitVector 6) <:>
      rs2                               <:>
      rs1                               <:>
      funct3                            <:>
      (bvExtract 0  imm :: BitVector 4) <:>
      (bvExtract 10 imm :: BitVector 1) <:>
      opcodeBits
encode (Inst opcode (UOperands rd imm)) =
  -- U type
  case opBitsFromOpcode opcode of
    UOpBits opcodeBits ->
      imm <:>
      rd  <:>
      opcodeBits
encode (Inst opcode (JOperands rd imm)) =
  -- J type
  case opBitsFromOpcode opcode of
    JOpBits opcodeBits ->
      (bvExtract 19 imm :: BitVector 1)  <:>
      (bvExtract 0  imm :: BitVector 10) <:>
      (bvExtract 10 imm :: BitVector 1)  <:>
      (bvExtract 11 imm :: BitVector 8)  <:>
      rd                                 <:>
      opcodeBits
encode (Inst opcode (EOperands )) =
  -- E type
  case opBitsFromOpcode opcode of
    EOpBits opcodeBits eBits -> eBits <:> opcodeBits
encode (Inst _ (XOperands ill)) = ill
