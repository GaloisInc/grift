{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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

module RISCV.Decode
  ( -- * Functions
    decode
  ) where

import Control.Lens ( (^.) )
import Data.BitVector.Sized
import Data.Parameterized

import RISCV.Instruction
import RISCV.InstructionSet
import RISCV.Instruction.Layouts

-- | Decode an instruction word. Since we won't know the format ahead of time, we
-- have to hide the format parameter of the return type with 'Some'.
decode :: KnownArch arch
       => InstructionSet arch exts
       -> BitVector 32
       -> Some (Instruction arch)
decode iset bv = case decodeFormat bv of
  Some repr -> case decodeOpcode iset repr bv of
    Right op     -> Some $ Inst op (decodeOperands repr bv)
    Left Illegal -> Some $ Inst Illegal (decodeOperands XRepr bv)

-- TODO: Decide whether we want to abstract this guy. I think we probably can compute
-- it from the encoding map, but it'll be tricky to do that unless I combine the
-- srai/srli and ecall/ebreak into single instructions.
-- | First, get the format
decodeFormat :: BitVector 32 -> Some FormatRepr
decodeFormat bv = case (bv ^. opcodeLens) of
  0b0110011 -> Some RRepr
  0b0111011 -> Some RRepr

  0b1100111 -> Some IRepr
  0b0000011 -> Some IRepr
  0b0010011 -> Some IRepr
  0b0001111 -> Some IRepr
  0b1110011 -> Some IRepr
  0b0011011 -> Some IRepr

  0b0100011 -> Some SRepr

  0b1100011 -> Some BRepr

  0b0110111 -> Some URepr
  0b0010111 -> Some URepr

  0b1101111 -> Some JRepr

  _ ->              Some XRepr

-- | From the format, get the operands
decodeOperands :: FormatRepr fmt -> BitVector 32 -> Operands fmt
decodeOperands repr bv = case repr of
  RRepr -> ROperands (bv ^. rdLens)  (bv ^. rs1Lens) (bv ^. rs2Lens)
  IRepr -> IOperands (bv ^. rdLens)  (bv ^. rs1Lens) (bv ^. imm12ILens)
  SRepr -> SOperands (bv ^. rs1Lens) (bv ^. rs2Lens) (bv ^. imm12SLens)
  BRepr -> BOperands (bv ^. rs1Lens) (bv ^. rs2Lens) (bv ^. imm12BLens)
  URepr -> UOperands (bv ^. rdLens)  (bv ^. imm20ULens)
  JRepr -> JOperands (bv ^. rdLens)  (bv ^. imm20JLens)
  XRepr -> XOperands (bv ^. illegalLens)

-- | From the format, get the opbits
decodeOpBits :: FormatRepr fmt -> BitVector 32 -> OpBits fmt
decodeOpBits repr bv = case repr of
  RRepr -> ROpBits (bv ^. opcodeLens) (bv ^. funct3Lens) (bv ^. funct7Lens)
  IRepr -> IOpBits (bv ^. opcodeLens) (bv ^. funct3Lens)
  SRepr -> SOpBits (bv ^. opcodeLens) (bv ^. funct3Lens)
  BRepr -> BOpBits (bv ^. opcodeLens) (bv ^. funct3Lens)
  URepr -> UOpBits (bv ^. opcodeLens)
  JRepr -> JOpBits (bv ^. opcodeLens)
  XRepr -> XOpBits

decodeOpcode :: InstructionSet arch exts
             -> FormatRepr fmt
             -> BitVector 32
             -> Either (Opcode arch 'X) (Opcode arch fmt)
decodeOpcode iset repr bv = opcodeFromOpBits iset (decodeOpBits repr bv)
