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
    -- * OpBits
  , OpBits(..)
  , opBits
  ) where

import Data.BitVector.Sized
import Data.Parameterized.Classes

import RISCV.Instruction

----------------------------------------
-- Encoding
-- TODO: consider annotating every single variable with the type in each case for
-- clarity.
-- | Encode an RV32I instruction as a 32-bit word.
encode :: forall (k :: Format). Instruction k -> BitVector 32
encode (Inst opcode (ROperands rd  rs1 rs2)) =
  -- R type
  case opBits opcode of
    ROpBits opcodeBits funct3 funct7 ->
      funct7 <:>
      rs2    <:>
      rs1    <:>
      funct3 <:>
      rd     <:>
      opcodeBits
encode (Inst opcode (IOperands rd rs1 imm)) =
  -- I type
  case opBits opcode of
    IOpBits opcodeBits funct3 ->
      imm    <:>
      rs1    <:>
      funct3 <:>
      rd     <:>
      opcodeBits
encode (Inst opcode (SOperands rs1 rs2 imm)) =
  -- S type
  case opBits opcode of
    SOpBits opcodeBits funct3 ->
      (bvExtract 5 imm :: BitVector 7) <:>
      rs2                              <:>
      rs1                              <:>
      funct3                           <:>
      (bvExtract 0 imm :: BitVector 5) <:>
      opcodeBits
encode (Inst opcode (BOperands rs1 rs2 imm)) =
  -- B type
  case opBits opcode of
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
  case opBits opcode of
    UOpBits opcodeBits ->
      imm <:>
      rd  <:>
      opcodeBits
encode (Inst opcode (JOperands rd imm)) =
  -- J type
  case opBits opcode of
    JOpBits opcodeBits ->
      (bvExtract 19 imm :: BitVector 1)  <:>
      (bvExtract 0  imm :: BitVector 10) <:>
      (bvExtract 10 imm :: BitVector 1)  <:>
      (bvExtract 11 imm :: BitVector 8)  <:>
      rd                                 <:>
      opcodeBits
encode (Inst opcode (EOperands )) =
  -- E type
  case opBits opcode of
    EOpBits opcodeBits eBits -> eBits <:> opcodeBits
encode (Inst _ (XOperands ill)) = ill

----------------------------------------
-- Opcode -> OpBits map

-- | Maps opcodes to the OpBit bit patterns they fix in an instruction.
opBits :: forall k . Opcode k -> OpBits k

-- R type
opBits Add  = ROpBits (bv 0b0110011) (bv 0b000) (bv 0b0000000)
opBits Sub  = ROpBits (bv 0b0110011) (bv 0b000) (bv 0b0100000)
opBits Sll  = ROpBits (bv 0b0110011) (bv 0b001) (bv 0b0000000)
opBits Slt  = ROpBits (bv 0b0110011) (bv 0b010) (bv 0b0000000)
opBits Sltu = ROpBits (bv 0b0110011) (bv 0b011) (bv 0b0000000)
opBits Xor  = ROpBits (bv 0b0110011) (bv 0b100) (bv 0b0000000)
opBits Srl  = ROpBits (bv 0b0110011) (bv 0b101) (bv 0b0000000)
opBits Sra  = ROpBits (bv 0b0110011) (bv 0b101) (bv 0b0100000)
opBits Or   = ROpBits (bv 0b0110011) (bv 0b110) (bv 0b0000000)
opBits And  = ROpBits (bv 0b0110011) (bv 0b111) (bv 0b0000000)

-- I type
opBits Jalr    = IOpBits (bv 0b1100111) (bv 0b000)
opBits Lb      = IOpBits (bv 0b0000011) (bv 0b000)
opBits Lh      = IOpBits (bv 0b0000011) (bv 0b001)
opBits Lw      = IOpBits (bv 0b0000011) (bv 0b010)
opBits Lbu     = IOpBits (bv 0b0000011) (bv 0b100)
opBits Lhu     = IOpBits (bv 0b0000011) (bv 0b101)
opBits Addi    = IOpBits (bv 0b0010011) (bv 0b000)
opBits Slti    = IOpBits (bv 0b0010011) (bv 0b010)
opBits Sltiu   = IOpBits (bv 0b0010011) (bv 0b011)
opBits Xori    = IOpBits (bv 0b0010011) (bv 0b100)
opBits Ori     = IOpBits (bv 0b0010011) (bv 0b110)
opBits Andi    = IOpBits (bv 0b0010011) (bv 0b111)
opBits Slli    = IOpBits (bv 0b0010011) (bv 0b001)
opBits Srli    = IOpBits (bv 0b0010011) (bv 0b101)
opBits Srai    = IOpBits (bv 0b0010011) (bv 0b101)
opBits Fence   = IOpBits (bv 0b0001111) (bv 0b000)
opBits Fence_i = IOpBits (bv 0b0001111) (bv 0b001)
opBits Csrrw   = IOpBits (bv 0b1110011) (bv 0b001)
opBits Csrrs   = IOpBits (bv 0b1110011) (bv 0b010)
opBits Csrrc   = IOpBits (bv 0b1110011) (bv 0b011)
opBits Csrrwi  = IOpBits (bv 0b1110011) (bv 0b101)
opBits Csrrsi  = IOpBits (bv 0b1110011) (bv 0b110)
opBits Csrrci  = IOpBits (bv 0b1110011) (bv 0b111)

-- S type
opBits Sb = SOpBits (bv 0b0100011) (bv 0b000)
opBits Sh = SOpBits (bv 0b0100011) (bv 0b001)
opBits Sw = SOpBits (bv 0b0100011) (bv 0b010)

-- B type
opBits Beq  = BOpBits (bv 0b1100011) (bv 0b000)
opBits Bne  = BOpBits (bv 0b1100011) (bv 0b001)
opBits Blt  = BOpBits (bv 0b1100011) (bv 0b100)
opBits Bge  = BOpBits (bv 0b1100011) (bv 0b101)
opBits Bltu = BOpBits (bv 0b1100011) (bv 0b110)
opBits Bgeu = BOpBits (bv 0b1100011) (bv 0b111)

-- U type
opBits Lui   = UOpBits (bv 0b0110111)
opBits Auipc = UOpBits (bv 0b0010111)

-- J type
opBits Jal = JOpBits (bv 0b1101111)

-- E type
opBits Ecall  = EOpBits (bv 0b1110011) (bv 0b0000000000000000000000000)
opBits Ebreak = EOpBits (bv 0b1110011) (bv 0b0000000000010000000000000)

-- X type
opBits Illegal = XOpBits

