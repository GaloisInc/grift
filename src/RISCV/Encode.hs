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
a 'Word32'.
-}

module RISCV.Encode
  ( -- * Functions
    encode
  , encodeOpBits
  , encodeOperands
    -- * OpBits
  , OpBits(..)
  ) where

import Data.Bits
import Data.Parameterized.Classes

import RISCV.BitVector
import RISCV.Instruction

----------------------------------------
-- Instruction encoding
--
-- Because of the clean way RISC-V classifies the different instruction formats, we
-- can encode the opcode and the operand list of an instruction completely
-- independently if we know the format.
--
-- We have two internal functions, encodeOpcode and encodeOperands. These each create
-- separate Word32's than can then be OR-ed together to create the full instruction
-- word.
--
-- encodeOpcode technically does a bit more than JUST the opcode, since many
-- instructions have additional "funct" bits (formats R, I, S, B all have the funct3
-- field and format R also has an additional funct7 field). These bits are encoded as
-- well for the formats that use them. Everything else is 0.
--
-- encodeOperands also uses the specific format to guide how to lay the bits down for
-- where the various operands need to go in the instruction.

----------------------------------------
-- Encoding
-- | Encode an RV32I instruction as a 32-bit word.
encode :: forall (k :: Format). Instruction k -> BitVector 32
encode (Inst opcode operands) =
  encodeOperands operands .|. encodeOpBits (opBits opcode)

-- | Encode the operands of an instruction.
encodeOperands :: forall (k :: Format). Operands k -> BitVector 32
encodeOperands (ROperands rd rs1 rs2) =
  (bv 0 :: BitVector 7) `bvConcat`
  rs2                   `bvConcat`
  rs1                   `bvConcat`
  (bv 0 :: BitVector 3) `bvConcat`
  rd                    `bvConcat`
  (bv 0 :: BitVector 7)
encodeOperands (IOperands rd rs1 imm) =
  imm                   `bvConcat`
  rs1                   `bvConcat`
  (bv 0 :: BitVector 3) `bvConcat`
  rd                    `bvConcat`
  (bv 0 :: BitVector 7)
encodeOperands (SOperands rs1 rs2 imm) =
  (bvExtract 5 imm :: BitVector 7) `bvConcat`
  rs2                              `bvConcat`
  rs1                              `bvConcat`
  (bv 0 :: BitVector 3)            `bvConcat`
  (bvExtract 0 imm :: BitVector 5) `bvConcat`
  (bv 0 :: BitVector 7)
encodeOperands (BOperands rs1 rs2 imm) =
  (bvExtract 5 imm :: BitVector 7) `bvConcat`
  rs2                              `bvConcat`
  rs1                              `bvConcat`
  (bv 0 :: BitVector 3)            `bvConcat`
  (bvExtract 0 imm :: BitVector 5) `bvConcat`
  (bv 0 :: BitVector 7)
encodeOperands (UOperands rd imm) =
  imm `bvConcat` rd `bvConcat` (bv 0 :: BitVector 7)
encodeOperands (JOperands rd imm) =
  (bvExtract 19 imm :: BitVector 1)  `bvConcat`
  (bvExtract 0  imm :: BitVector 10) `bvConcat`
  (bvExtract 10 imm :: BitVector 1)  `bvConcat`
  (bvExtract 11 imm :: BitVector 8)  `bvConcat`
  rd                                 `bvConcat`
  (bv 0 :: BitVector 7)
encodeOperands (EOperands) = bv 0 :: BitVector 32

-- | Encode the OpBits of a particular opcode.
encodeOpBits :: forall (k :: Format). OpBits k -> BitVector 32
encodeOpBits (ROpBits opcode funct3 funct7) =
  funct7 `bvConcat`
  (bv 0 :: BitVector 10) `bvConcat`
  funct3 `bvConcat`
  (bv 0 :: BitVector 5) `bvConcat`
  opcode
encodeOpBits (IOpBits opcode funct3) =
  (bv 0 :: BitVector 17) `bvConcat`
  funct3 `bvConcat`
  (bv 0 :: BitVector 5) `bvConcat`
  opcode
encodeOpBits (SOpBits opcode funct3) =
  (bv 0 :: BitVector 17) `bvConcat`
  funct3 `bvConcat`
  (bv 0 :: BitVector 5) `bvConcat`
  opcode
encodeOpBits (BOpBits opcode funct3) =
  (bv 0 :: BitVector 17) `bvConcat`
  funct3 `bvConcat`
  (bv 0 :: BitVector 5) `bvConcat`
  opcode
encodeOpBits (UOpBits opcode) =
  (bv 0 :: BitVector 25) `bvConcat`
  opcode
encodeOpBits (JOpBits opcode) =
  (bv 0 :: BitVector 25) `bvConcat`
  opcode
encodeOpBits (EOpBits opcode b) =
  (bv 0 :: BitVector 11) `bvConcat`
  b `bvConcat`
  (bv 0 :: BitVector 13) `bvConcat`
  opcode

----------------------------------------
-- OpBits

-- | Bits fixed by an opcode.
-- Holds all the bits that are fixed by a particular opcode. Each format maps to a
-- potentially different set of bits.
data OpBits :: Format -> * where
  ROpBits :: BitVector 7 -> BitVector 3 -> BitVector 7 -> OpBits 'R
  IOpBits :: BitVector 7 -> BitVector 3                -> OpBits 'I
  SOpBits :: BitVector 7 -> BitVector 3                -> OpBits 'S
  BOpBits :: BitVector 7 -> BitVector 3                -> OpBits 'B
  UOpBits :: BitVector 7                               -> OpBits 'U
  JOpBits :: BitVector 7                               -> OpBits 'J
  EOpBits :: BitVector 7 -> BitVector 1                -> OpBits 'E

instance Show (OpBits k) where
  show (ROpBits opcode funct3 funct7) =
    "[ opcode = " ++ show opcode ++
    ", funct3 = " ++ show funct3 ++
    ", funct7 = " ++ show funct7 ++ "]"
  show (IOpBits opcode funct3) =
    "[ opcode = " ++ show opcode ++
    ", funct3 = " ++ show funct3 ++ "]"
  show (SOpBits opcode funct3) =
    "[ opcode = " ++ show opcode ++
    ", funct3 = " ++ show funct3 ++ "]"
  show (BOpBits opcode funct3) =
    "[ opcode = " ++ show opcode ++
    ", funct3 = " ++ show funct3 ++ "]"
  show (UOpBits opcode) =
    "[ opcode = " ++ show opcode ++ "]"
  show (JOpBits opcode) =
    "[ opcode = " ++ show opcode ++ "]"
  show (EOpBits opcode b) =
    "[ opcode = " ++ show opcode ++
    ", b = " ++ show b ++ "]"

instance ShowF OpBits

----------------------------------------
-- Opcode -> OpBits map

-- | Maps opcodes to the OpBit bit patterns they map to in an instruction.
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
opBits Beq  = BOpBits (bv 1100011) (bv 0b000)
opBits Bne  = BOpBits (bv 1100011) (bv 0b001)
opBits Blt  = BOpBits (bv 1100011) (bv 0b100)
opBits Bge  = BOpBits (bv 1100011) (bv 0b101)
opBits Bltu = BOpBits (bv 1100011) (bv 0b110)
opBits Bgeu = BOpBits (bv 1100011) (bv 0b111)

-- U type
opBits Lui   = UOpBits (bv 0b0110111)
opBits Auipc = UOpBits (bv 0b0010111)

-- J typep
opBits Jal = JOpBits (bv 0b1101111)

-- E type
opBits Ecall  = EOpBits (bv 0b1110011) (bv 0b0)
opBits Ebreak = EOpBits (bv 0b1110011) (bv 0b1)
