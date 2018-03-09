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

-- TODO: we have to solve the one-to-one problem with shifts, fences and ecall/ebreak
-- here. Some of these instructions have ambiguous decode with respect to the
-- DecodeBits they map to, and we need to examine the operands to disambiguate. We
-- could either create a separate format for these guys (like we did for
-- ecall/ebreak) or we could just disambiguate in this function. I'd prefer the
-- former, as long as it doesn't get *too* ridiculous.
decode :: BitVector 32 -> Some Instruction
decode bvec = case (opcodeBits, funct3Bits, funct7Bits, eBits) of
  -- U type
  (0b0110111, _, _, _) -> Some $ Inst Lui   uOperands
  (0b0010111, _, _, _) -> Some $ Inst Auipc uOperands
  -- J type
  (0b1101111, _, _, _) -> Some $ Inst Jal jOperands
  -- E type
  (0b1110011, _, _, 0b0000000000000000000000000) -> Some $ Inst Ecall  EOperands
  (0b1110011, _, _, 0b0000000000010000000000000) -> Some $ Inst Ebreak EOperands
  -- I type
  (0b1100111, 0b000, _, _) -> Some $ Inst Jalr iOperands
  (0b0000011, 0b000, _, _) -> Some $ Inst Lb iOperands
  (0b0000011, 0b001, _, _) -> Some $ Inst Lh iOperands
  (0b0000011, 0b010, _, _) -> Some $ Inst Lw iOperands
  (0b0000011, 0b100, _, _) -> Some $ Inst Lbu iOperands
  (0b0000011, 0b101, _, _) -> Some $ Inst Lhu iOperands
  (0b0010011, 0b000, _, _) -> Some $ Inst Addi iOperands
  (0b0010011, 0b010, _, _) -> Some $ Inst Slti iOperands
  (0b0010011, 0b011, _, _) -> Some $ Inst Sltiu iOperands
  (0b0010011, 0b100, _, _) -> Some $ Inst Xori iOperands
  (0b0010011, 0b110, _, _) -> Some $ Inst Ori iOperands
  (0b0010011, 0b111, _, _) -> Some $ Inst Andi iOperands
  -- TODO: the shifts are also a different format, since the decoding actually
  -- depends on the immediates. That is why we have overlapping cases here. I'm
  -- leaving this as-is for now.
  (0b0010011, 0b001, _, _) -> Some $ Inst Slli iOperands
  (0b0010011, 0b101, _, _) -> Some $ Inst Srli iOperands
  -- (0b0010011, 0b101, _, _) -> Some $ Inst Srai iOperands
  -- TODO: This isn't quite right, fence is really a different format.
  (0b0001111, 0b000, _, _) -> Some $ Inst Fence iOperands
  (0b0001111, 0b001, _, _) -> Some $ Inst Fence_i iOperands
  (0b1110011, 0b001, _, _) -> Some $ Inst Csrrw iOperands
  (0b1110011, 0b010, _, _) -> Some $ Inst Csrrs iOperands
  (0b1110011, 0b011, _, _) -> Some $ Inst Csrrc iOperands
  (0b1110011, 0b101, _, _) -> Some $ Inst Csrrwi iOperands
  (0b1110011, 0b110, _, _) -> Some $ Inst Csrrsi iOperands
  (0b1110011, 0b111, _, _) -> Some $ Inst Csrrci iOperands
  -- S type
  (0b0100011, 0b000, _, _) -> Some $ Inst Sb sOperands
  (0b0100011, 0b001, _, _) -> Some $ Inst Sh sOperands
  (0b0100011, 0b010, _, _) -> Some $ Inst Sw sOperands
  -- B type
  (0b1100011, 0b000, _, _) -> Some $ Inst Beq  bOperands
  (0b1100011, 0b001, _, _) -> Some $ Inst Bne  bOperands
  (0b1100011, 0b100, _, _) -> Some $ Inst Blt  bOperands
  (0b1100011, 0b101, _, _) -> Some $ Inst Bge  bOperands
  (0b1100011, 0b110, _, _) -> Some $ Inst Bltu bOperands
  (0b1100011, 0b111, _, _) -> Some $ Inst Bgeu bOperands
  -- R type
  (0b0110011, 0b000, 0b0000000, _) -> Some $ Inst Add rOperands
  (0b0110011, 0b000, 0b0100000, _) -> Some $ Inst Sub rOperands
  (0b0110011, 0b001, 0b0000000, _) -> Some $ Inst Sll rOperands
  (0b0110011, 0b010, 0b0000000, _) -> Some $ Inst Slt rOperands
  (0b0110011, 0b011, 0b0000000, _) -> Some $ Inst Sltu rOperands
  (0b0110011, 0b100, 0b0000000, _) -> Some $ Inst Xor rOperands
  (0b0110011, 0b101, 0b0000000, _) -> Some $ Inst Srl rOperands
  (0b0110011, 0b101, 0b0100000, _) -> Some $ Inst Sra rOperands
  (0b0110011, 0b110, 0b0000000, _) -> Some $ Inst Or rOperands
  (0b0110011, 0b111, 0b0000000, _) -> Some $ Inst And rOperands
  -- X type (illegal instruction)
  _ -> Some $ Inst Illegal (XOperands bvec)
  where dBits      = getDecodeBits bvec
        opcodeBits = bvIntegerS (dOpcode dBits)
        funct3Bits = bvIntegerS (dFunct3 dBits)
        funct7Bits = bvIntegerS (dFunct7 dBits)
        eBits      = bvIntegerS (dEBits  dBits)
        rOperands  = undefined
        iOperands  = undefined
        sOperands  = undefined
        bOperands  = undefined
        uOperands  = undefined
        jOperands  = undefined

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
