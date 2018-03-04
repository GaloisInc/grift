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

TODO module description
-}

module RISCV.Encode
  ( encode
  ) where

import Data.Bits
-- import Data.Parameterized.NatRepr
import Data.Word (Word32)
-- import GHC.TypeLits

import RISCV.Instruction

-- | Determine if a signed integer fits in the specified number of bits.
fitsBitsSigned :: Integer -> Int -> Bool
fitsBitsSigned word width = fitsBitsUnsigned adjWord width
  where adjWord = word + (1 `shiftL` (width - 1))

-- | Determine if an unsigned integer fits in the specified number of bits. Return
-- False if the input word is negative.
fitsBitsUnsigned :: Integer -> Int -> Bool
fitsBitsUnsigned word width =
  if 0 <= word && word < (1 `shiftL` width)
  then True
  else False

placeBitsSigned :: Int -> Int -> Integer -> Maybe Word32
placeBitsSigned lo hi w = do
  let bits = hi - lo + 1
  if w `fitsBitsSigned` bits
    then return $ (fromIntegral w .&. (bit bits - 1)) `shiftL` lo
    else Nothing

placeBitsUnsigned :: Int -> Int -> Integer -> Maybe Word32
placeBitsUnsigned lo hi w = do
  let bits = hi - lo + 1
  if w `fitsBitsUnsigned` bits
    then return $ (fromIntegral w .&. (bit bits - 1)) `shiftL` lo
    else Nothing

-- | Encode an RV32I instruction as a 32-bit word.
encode :: forall (k :: Format). Instruction k -> Maybe Word32
encode (Inst opcode operands) = do
  operandBits <- encodeOperands operands
  let opcodeBits = encodeOpcode opcode
  return $ opcodeBits .|. operandBits

-- TODO: Would this be faster with a standalone Data.Map?
encodeOpcode :: forall (k :: Format). Opcode k -> Word32
encodeOpcode Add  = 0x00000033
encodeOpcode Sub  = 0x40000033
encodeOpcode Sll  = 0x00001033
encodeOpcode Slt  = 0x00002033
encodeOpcode Sltu = 0x00003033
encodeOpcode Xor  = 0x00004033
encodeOpcode Srl  = 0x00005033
encodeOpcode Sra  = 0x40005033
encodeOpcode Or   = 0x00006033
encodeOpcode And  = 0x00007033

encodeOpcode Jalr  = 0x00000067
encodeOpcode Lb    = 0x00000003
encodeOpcode Lh    = 0x00001003
encodeOpcode Lw    = 0x00002003
encodeOpcode Lbu   = 0x00004003
encodeOpcode Lhu   = 0x00005003
encodeOpcode Addi  = 0x00000013
encodeOpcode Slti  = 0x00002013
encodeOpcode Sltiu = 0x00003013
encodeOpcode Xori  = 0x00004013
encodeOpcode Ori   = 0x00006013
encodeOpcode Andi  = 0x00007013
encodeOpcode Slli  = 0x00001013
encodeOpcode Srli  = 0x00005013
encodeOpcode Srai  = 0x40005013

encodeOpcode Fence   = 0x0000000F
encodeOpcode Fence_i = 0x0000100F
encodeOpcode Ecall   = 0x00000073
encodeOpcode Ebreak  = 0x00100073
encodeOpcode Csrrw   = 0x00001073
encodeOpcode Csrrs   = 0x00002073
encodeOpcode Csrrc   = 0x00003073
encodeOpcode Csrrwi  = 0x00005073
encodeOpcode Csrrsi  = 0x00006073
encodeOpcode Csrrci  = 0x00007073

encodeOpcode Sb = 0x00000023
encodeOpcode Sh = 0x00001023
encodeOpcode Sw = 0x00002023

encodeOpcode Beq   = 0x00000063
encodeOpcode Bne   = 0x00001063
encodeOpcode Blt   = 0x00004063
encodeOpcode Bge   = 0x00005063
encodeOpcode Bltu  = 0x00006063
encodeOpcode Bgeu  = 0x00007063
encodeOpcode Lui   = 0x00000037
encodeOpcode Addui = 0x00000017

encodeOpcode Jal = 0x0000006F

encodeOperands :: forall (k :: Format). Operands k -> Maybe Word32
encodeOperands (ROperands rd rs1 rs2) = do
  rdBits  <- placeRdBits  rd
  rs1Bits <- placeRs1Bits rs1
  rs2Bits <- placeRs2Bits rs2
  return $ rs2Bits .|. rs1Bits .|. rdBits
encodeOperands (IOperands rd rs1 imm) = do
  rdBits  <- placeRdBits   rd
  rs1Bits <- placeRs1Bits  rs1
  immBits <- placeImmIBits imm
  return $ immBits .|. rs1Bits .|. rdBits
encodeOperands (SOperands  rs1 rs2 imm) = do
  rs1Bits <- placeRs1Bits  rs1
  rs2Bits <- placeRs2Bits  rs2
  immBits <- placeImmSBits imm
  return $ rs1Bits .|. rs2Bits .|. immBits
encodeOperands (BOperands rs1 rs2 imm) = do
  rs1Bits <- placeRs1Bits  rs1
  rs2Bits <- placeRs2Bits  rs2
  immBits <- placeImmBBits imm
  return $ rs1Bits .|. rs2Bits .|. immBits
encodeOperands (UOperands rd imm) = do
  rdBits  <- placeRdBits   rd
  immBits <- placeImmUBits imm
  return $ rdBits .|. immBits
encodeOperands (JOperands rd imm) = do
  rdBits  <- placeRdBits   rd
  immBits <- placeImmJBits imm
  return $ rdBits .|. immBits

placeRdBits :: RegId -> Maybe Word32
placeRdBits (RegId rd) = placeBitsUnsigned 7 11 rd

placeRs1Bits :: RegId -> Maybe Word32
placeRs1Bits (RegId rs1) = placeBitsUnsigned 15 18 rs1

placeRs2Bits :: RegId -> Maybe Word32
placeRs2Bits (RegId rs2) = placeBitsUnsigned 20 24 rs2

-- TODO: Verify that all immediates are interpreted as signed integers. Even sltiu, I
-- believe, expands the value as a signed integer before comparison.
placeImmIBits :: Imm12 -> Maybe Word32
placeImmIBits (Imm12 imm) = placeBitsSigned 31 20 imm

placeImmSBits :: Imm12 -> Maybe Word32
placeImmSBits (Imm12 imm) = do
  imm11_5 <- placeBitsSigned 25 31 (imm .&. 0xFE0)
  imm4_0  <- placeBitsSigned 7  11 (imm .&. 0x01F)
  return $ imm11_5 .|. imm4_0

placeImmBBits :: Imm12 -> Maybe Word32
placeImmBBits (Imm12 imm) = do
  imm12   <- placeBitsSigned 31 31 (imm .&. 0x1000)
  imm10_5 <- placeBitsSigned 30 25 (imm .&. 0x07E0)
  imm4_1  <- placeBitsSigned 8  11 (imm .&. 0x001E)
  imm11   <- placeBitsSigned 11 11 (imm .&. 0x0800)
  return $ imm12 .|. imm10_5 .|. imm4_1 .|. imm11

placeImmUBits :: Imm20 -> Maybe Word32
placeImmUBits (Imm20 imm) = placeBitsSigned 31 12 (imm .&. 0xFFFFF000)

placeImmJBits :: Imm20 -> Maybe Word32
placeImmJBits (Imm20 imm) = do
  imm20    <- placeBitsSigned 31 31 (imm .&. 0x00100000)
  imm10_1  <- placeBitsSigned 21 30 (imm .&. 0x000007FE)
  imm11    <- placeBitsSigned 20 20 (imm .&. 0x00000800)
  imm19_12 <- placeBitsSigned 12 19 (imm .&. 0x000FF000)
  return $ imm20 .|. imm10_1 .|. imm11 .|. imm19_12


