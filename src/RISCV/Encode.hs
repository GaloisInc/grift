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
  -- ( encode
  -- , encodeOpcode
  -- , encodeOperands
  -- ) where
  where -- temporarily exporting everything for debugging purposes

import Data.Bits

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
-- separate Word32 than can then be OR-ed together to create the full instruction
-- word.
--
-- encodeOpcode technically does a bit more than JUST the opcode, since many
-- instructions have additional "funct" bits (formats R, I, S, B all have the funct3
-- field and format R also has an additional funct7 field). These bits are encoded as
-- well for the formats that use them. Everything else is 0.
--
-- encodeOperands also uses the specific format to guide how to lay the bits down for
-- where the various operands need to go in the instruction.

-- | Encode an RV32I instruction as a 32-bit word.
encode :: forall (k :: Format). Instruction k -> BitVector 32
encode (Inst opcode operands) = encodeOperands operands .|. encodeOpcode opcode

-- TODO: Would this be faster with a standalone Data.Map?
-- TODO: Would it make sense to make a separate encoding function for each format?
-- that way we could give each encodeOpcode function a different argument list with
-- different types, and it would lay it down nicely.
encodeOpcode :: forall (k :: Format). Opcode k -> BitVector 32
encodeOpcode Add  = bv 0x00000033
encodeOpcode Sub  = bv 0x40000033
encodeOpcode Sll  = bv 0x00001033
encodeOpcode Slt  = bv 0x00002033
encodeOpcode Sltu = bv 0x00003033
encodeOpcode Xor  = bv 0x00004033
encodeOpcode Srl  = bv 0x00005033
encodeOpcode Sra  = bv 0x40005033
encodeOpcode Or   = bv 0x00006033
encodeOpcode And  = bv 0x00007033

encodeOpcode Jalr  = bv 0x00000067
encodeOpcode Lb    = bv 0x00000003
encodeOpcode Lh    = bv 0x00001003
encodeOpcode Lw    = bv 0x00002003
encodeOpcode Lbu   = bv 0x00004003
encodeOpcode Lhu   = bv 0x00005003
encodeOpcode Addi  = bv 0x00000013
encodeOpcode Slti  = bv 0x00002013
encodeOpcode Sltiu = bv 0x00003013
encodeOpcode Xori  = bv 0x00004013
encodeOpcode Ori   = bv 0x00006013
encodeOpcode Andi  = bv 0x00007013
encodeOpcode Slli  = bv 0x00001013
encodeOpcode Srli  = bv 0x00005013
encodeOpcode Srai  = bv 0x40005013

encodeOpcode Fence   = bv 0x0000000F
encodeOpcode Fence_i = bv 0x0000100F
encodeOpcode Ecall   = bv 0x00000073
encodeOpcode Ebreak  = bv 0x00100073
encodeOpcode Csrrw   = bv 0x00001073
encodeOpcode Csrrs   = bv 0x00002073
encodeOpcode Csrrc   = bv 0x00003073
encodeOpcode Csrrwi  = bv 0x00005073
encodeOpcode Csrrsi  = bv 0x00006073
encodeOpcode Csrrci  = bv 0x00007073

encodeOpcode Sb = bv 0x00000023
encodeOpcode Sh = bv 0x00001023
encodeOpcode Sw = bv 0x00002023

encodeOpcode Beq   = bv 0x00000063
encodeOpcode Bne   = bv 0x00001063
encodeOpcode Blt   = bv 0x00004063
encodeOpcode Bge   = bv 0x00005063
encodeOpcode Bltu  = bv 0x00006063
encodeOpcode Bgeu  = bv 0x00007063
encodeOpcode Lui   = bv 0x00000037
encodeOpcode Addui = bv 0x00000017

encodeOpcode Jal = bv 0x0000006F

encodeOperands :: forall (k :: Format). Operands k -> BitVector 32
encodeOperands (ROperands rd rs1 rs2) =
  (bv 0 :: BitVector 7) `bvConcat`
  rs2 `bvConcat`
  rs1 `bvConcat`
  (bv 0 :: BitVector 3) `bvConcat`
  rd `bvConcat`
  (bv 0 :: BitVector 7)
encodeOperands (IOperands rd rs1 imm) =
  imm `bvConcat`
  rs1 `bvConcat`
  (bv 0 :: BitVector 3) `bvConcat`
  rd `bvConcat`
  (bv 0 :: BitVector 7)
encodeOperands (SOperands _rs1 _rs2 _imm) = undefined
encodeOperands (BOperands _rs1 _rs2 _imm) = undefined
encodeOperands (UOperands _rd  _imm) = undefined
encodeOperands (JOperands _rd  _imm) = undefined


-- TODO: Replace all this code with code that uses the extract function.

-- placeRdBits :: BitVector 5 -> Maybe Word32
-- placeRdBits (BitVector 5 rd) = placeBitsUnsigned 7 11 rd

-- placeRs1Bits :: BitVector 5 -> Maybe Word32
-- placeRs1Bits (BitVector 5 rs1) = placeBitsUnsigned 15 18 rs1

-- placeRs2Bits :: BitVector 5 -> Maybe Word32
-- placeRs2Bits (BitVector 5 rs2) = placeBitsUnsigned 20 24 rs2

-- placeImmIBits :: BitVector 12 -> Maybe Word32
-- placeImmIBits (BitVector 12 imm) = placeBitsSigned 20 31 imm

-- placeImmSBits :: BitVector 12 -> Maybe Word32
-- placeImmSBits (BitVector 12 imm) = do
--   imm11_5 <- placeBitsSigned 25 31 (imm .&. 0xFE0)
--   imm4_0  <- placeBitsSigned 7  11 (imm .&. 0x01F)
--   return $ imm11_5 .|. imm4_0

-- placeImmBBits :: BitVector 12 -> Maybe Word32
-- placeImmBBits (BitVector 12 imm) = do
--   imm12   <- placeBitsSigned 31 31 (imm .&. 0x1000)
--   imm10_5 <- placeBitsSigned 25 30 (imm .&. 0x07E0)
--   imm4_1  <- placeBitsSigned 8  11 (imm .&. 0x001E)
--   imm11   <- placeBitsSigned 7  7  (imm .&. 0x0800)
--   return $ imm12 .|. imm10_5 .|. imm4_1 .|. imm11

-- placeImmUBits :: BitVector 20 -> Maybe Word32
-- placeImmUBits (BitVector 20 imm) = placeBitsSigned 12 31 ((imm .&. 0xFFFFF000) `shiftR` 12)

-- placeImmJBits :: BitVector 20 -> Maybe Word32
-- placeImmJBits (BitVector 20 imm) = do
--   imm20    <- placeBitsSigned 31 31 (imm .&. 0x00100000)
--   imm10_1  <- placeBitsSigned 21 30 (imm .&. 0x000007FE)
--   imm11    <- placeBitsSigned 20 20 (imm .&. 0x00000800)
--   imm19_12 <- placeBitsSigned 12 19 (imm .&. 0x000FF000)
--   return $ imm20 .|. imm10_1 .|. imm11 .|. imm19_12


