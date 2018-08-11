{-
This file is part of GRIFT (Galois RISC-V ISA Formal Tools).

GRIFT is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GRIFT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero Public License for more details.

You should have received a copy of the GNU Affero Public License
along with GRIFT.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : RISCV.Decode
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module defines a function 'decode' that converts a 'BitVector' @32@ an internal
'Instruction', as well as the corresponding reverse 'encode' function.
-}

module RISCV.Decode
  ( -- * Functions
    decode
  , encode
  ) where

import Control.Lens hiding ( (:<), Index, op, iset )
import Data.BitVector.Sized
import Data.BitVector.Sized.BitLayout
import Data.Parameterized
import Data.Parameterized.List

import RISCV.InstructionSet
import RISCV.Types

type OpBitsLayout fmt = List (BitLayout 32) (OpBitsTypes fmt)
type OperandsLayout fmt = List (BitLayout 32) (OperandTypes fmt)

opcode :: BitLayout 32 7
opcode = chunk 0 <: empty

funct3 :: BitLayout 32 3
funct3 = singleChunk 12

funct7 :: BitLayout 32 7
funct7 = singleChunk 25

-- | Given a format, get the 'BitLayout's for the 'OpBits' of that format.
opBitsLayouts :: FormatRepr fmt -> OpBitsLayout fmt
opBitsLayouts repr = case repr of
  RRepr  -> opcode :< funct3 :< funct7 :< Nil
  IRepr  -> opcode :< funct3 :< Nil
  SRepr  -> opcode :< funct3 :< Nil
  BRepr  -> opcode :< funct3 :< Nil
  URepr  -> opcode :< Nil
  JRepr  -> opcode :< Nil
  HRepr  -> opcode :< funct3 :< funct5 :< Nil
  PRepr  -> funct32 :< Nil
  ARepr  -> opcode :< funct3 :< funct5 :< Nil
  R2Repr -> opcode :< funct12 :< Nil
  R3Repr -> opcode :< funct7 :< Nil
  R4Repr -> opcode :< funct2 :< Nil
  RXRepr -> opcode :< funct3 :< funct12 :< Nil
  XRepr  -> Nil
  where funct5 :: BitLayout 32 5
        funct5 = singleChunk 27
        funct32 :: BitLayout 32 32
        funct32 = singleChunk 0
        funct2 :: BitLayout 32 2
        funct2 = singleChunk 25
        funct12 :: BitLayout 32 12
        funct12 = singleChunk 20

-- | Given a format, get the 'BitLayout's for the 'Operands' of that format.
operandsLayouts :: FormatRepr fmt -> OperandsLayout fmt
operandsLayouts repr = case repr of
  RRepr  -> rdLayout  :< rs1Layout :< rs2Layout :< Nil
  IRepr  -> rdLayout  :< rs1Layout :< imm12ILayout :< Nil
  SRepr  -> rs1Layout :< rs2Layout :< imm12SLayout :< Nil
  BRepr  -> rs1Layout :< rs2Layout :< imm12BLayout :< Nil
  URepr  -> rdLayout  :< imm20ULayout :< Nil
  JRepr  -> rdLayout  :< imm20JLayout :< Nil
  HRepr  -> rdLayout  :< rs1Layout :< shamtLayout :< Nil
  PRepr  -> Nil
  ARepr  -> rdLayout  :< rs1Layout :< rs2Layout :< rlLayout :< aqLayout :< Nil
  R2Repr -> rdLayout  :< rmLayout :< rs1Layout :< Nil
  R3Repr -> rdLayout  :< rmLayout :< rs1Layout :< rs2Layout :< Nil
  R4Repr -> rdLayout  :< rmLayout :< rs1Layout :< rs2Layout :< rs3Layout :< Nil
  RXRepr -> rdLayout  :< rs1Layout :< Nil
  XRepr  -> illegalLayout :< Nil

  where rdLayout     :: BitLayout 32 5  = singleChunk 7
        rs1Layout    :: BitLayout 32 5 = singleChunk 15
        rs2Layout    :: BitLayout 32 5 = singleChunk 20
        rs3Layout    :: BitLayout 32 5 = singleChunk 27
        rmLayout     :: BitLayout 32 3 = singleChunk 12 -- same as funct3
        shamtLayout  :: BitLayout 32 7 = singleChunk 20
        rlLayout     :: BitLayout 32 1 = singleChunk 25
        aqLayout     :: BitLayout 32 1 = singleChunk 26
        imm12ILayout :: BitLayout 32 12 = singleChunk 20
        imm12SLayout :: BitLayout 32 12 = (chunk 25 :: Chunk 7) <: (chunk 7  :: Chunk 5) <: empty
        imm12BLayout :: BitLayout 32 12 =
          (chunk 31 :: Chunk 1) <: (chunk 7  :: Chunk 1) <:
          (chunk 25 :: Chunk 6) <: (chunk 8  :: Chunk 4) <:
          empty
        imm20ULayout :: BitLayout 32 20 = singleChunk 12
        imm20JLayout :: BitLayout 32 20 =
          (chunk 31 :: Chunk 1)  <: (chunk 12 :: Chunk 8)  <:
          (chunk 20 :: Chunk 1)  <: (chunk 21 :: Chunk 10) <:
          empty
        illegalLayout :: BitLayout 32 32 = singleChunk 0

-- | Get the format of an instruction word.
getFormat :: BitVector 32 -> Some FormatRepr
getFormat bv = case bv ^. layoutLens opcode of
  0b0110011 -> Some RRepr
  0b0111011 -> Some RRepr
  0b1010011 -> case bv ^. layoutLens funct7 of
    0b0000000 -> Some R3Repr
    0b0000100 -> Some R3Repr
    0b0001000 -> Some R3Repr
    0b0001100 -> Some R3Repr
    0b0101100 -> Some R2Repr
    0b1100000 -> Some R2Repr
    0b1101000 -> Some R2Repr
    0b1110000 -> Some RXRepr
    0b1111000 -> Some RXRepr
    _ -> Some RRepr

  0b1100111 -> Some IRepr
  0b0000011 -> Some IRepr
  0b0010011 -> case bv ^. layoutLens funct3 of
    0b001 -> Some HRepr
    0b101 -> Some HRepr
    _ -> Some IRepr
  0b0001111 -> Some IRepr
  0b0011011 -> case bv ^. layoutLens funct3 of
    0b001 -> Some RRepr
    0b101 -> Some RRepr
    _ -> Some IRepr
  0b0000111 -> Some IRepr

  0b0100011 -> Some SRepr
  0b0100111 -> Some SRepr

  0b1100011 -> Some BRepr

  0b0110111 -> Some URepr
  0b0010111 -> Some URepr

  0b1101111 -> Some JRepr

  0b1110011 -> case bv ^. layoutLens funct3 of
    0b000 -> Some PRepr
    _ -> Some IRepr

  0b0101111 -> Some ARepr

  0b1000011 -> Some R4Repr
  0b1000111 -> Some R4Repr
  0b1001011 -> Some R4Repr
  0b1001111 -> Some R4Repr

  _ ->         Some XRepr

-- | Decode an instruction word. Since we won't know the format ahead of time, we
-- have to hide the format parameter of the return type with 'Some'.
decode :: InstructionSet rv
       -> BitVector 32
       -> Some (Instruction rv)
decode iset bv = case getFormat bv of
  Some repr -> case decodeOpcode iset repr bv of
    Right op     -> Some $ Inst op (decodeOperands repr bv)
    Left Illegal -> Some $ Inst Illegal (decodeOperands XRepr bv)

-- | From the format, get the operands
decodeOperands :: FormatRepr fmt -> BitVector 32 -> Operands fmt
decodeOperands repr bv = Operands repr (bv ^. layoutsLens (operandsLayouts repr))

-- | From the format, get the opbits
decodeOpBits :: FormatRepr fmt -> BitVector 32 -> OpBits fmt
decodeOpBits repr bv = OpBits repr (bv ^. layoutsLens (opBitsLayouts repr))

decodeOpcode :: InstructionSet rv
             -> FormatRepr fmt
             -> BitVector 32
             -> Either (Opcode rv X) (Opcode rv fmt)
decodeOpcode iset repr bv = opcodeFromOpBits iset (decodeOpBits repr bv)

-- | Encode an 'Instruction' as a 32-bit instruction word.
encode :: InstructionSet rv -> Instruction rv fmt -> BitVector 32
encode iset (Inst opc (Operands repr operands)) =
  0 & (opBitsLens .~ opBits) & (operandsLens .~ operands)
  where opBitsLens   = layoutsLens (opBitsLayouts repr)
        operandsLens = layoutsLens (operandsLayouts repr)
        (OpBits _ opBits) = opBitsFromOpcode iset opc
