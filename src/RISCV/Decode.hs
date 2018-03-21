{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
import Data.Parameterized

import RISCV.Instruction
import RISCV.Instruction.Layouts

-- | Decode an instruction word. Since we won't know the format ahead of time, we
-- have to hide the format parameter of the return type with 'Some'.
decode :: InstructionSet -> InstWord 2 -> Some Instruction
decode iset bv = case decodeFormat bv of
  Some repr -> case decodeOpcode iset repr bv of
    Right op     -> Some $ Inst op (decodeOperands repr bv)
    Left Illegal -> Some $ Inst Illegal (decodeOperands XRepr bv)

-- TODO: Decide whether we want to abstract this guy
-- | First, get the format
decodeFormat :: InstWord 2 -> Some FormatRepr
decodeFormat bv = case (bv ^. opcodeLens, bv ^. funct3Lens) of
  (0b0110011, _) -> Some RRepr

  (0b1110011, 0b000) -> Some ERepr -- special case.

  (0b1100111, _) -> Some IRepr
  (0b0000011, _) -> Some IRepr
  (0b0010011, _) -> Some IRepr
  (0b0001111, _) -> Some IRepr
  (0b1110011, _) -> Some IRepr

  (0b0100011, _) -> Some SRepr

  (0b1100011, _) -> Some BRepr

  (0b0110111, _) -> Some URepr
  (0b0010111, _) -> Some URepr

  (0b1101111, _) -> Some JRepr

  _ ->              Some XRepr

-- | From the format, get the operands
decodeOperands :: FormatRepr k -> InstWord 2 -> Operands k
decodeOperands repr bv = case repr of
  RRepr -> ROperands (bv ^. rdLens)  (bv ^. rs1Lens) (bv ^. rs2Lens)
  IRepr -> IOperands (bv ^. rdLens)  (bv ^. rs1Lens) (bv ^. imm12ILens)
  SRepr -> SOperands (bv ^. rs1Lens) (bv ^. rs2Lens) (bv ^. imm12SLens)
  BRepr -> BOperands (bv ^. rs1Lens) (bv ^. rs2Lens) (bv ^. imm12BLens)
  URepr -> UOperands (bv ^. rdLens)  (bv ^. imm20ULens)
  JRepr -> JOperands (bv ^. rdLens)  (bv ^. imm20JLens)
  ERepr -> EOperands
  XRepr -> XOperands (bv ^. illegalLens)

-- | From the format, get the opbits
decodeOpBits :: FormatRepr k -> InstWord 2 -> OpBits k
decodeOpBits repr bv = case repr of
  RRepr -> ROpBits (bv ^. opcodeLens) (bv ^. funct3Lens) (bv ^. funct7Lens)
  IRepr -> IOpBits (bv ^. opcodeLens) (bv ^. funct3Lens)
  SRepr -> SOpBits (bv ^. opcodeLens) (bv ^. funct3Lens)
  BRepr -> BOpBits (bv ^. opcodeLens) (bv ^. funct3Lens)
  URepr -> UOpBits (bv ^. opcodeLens)
  JRepr -> JOpBits (bv ^. opcodeLens)
  ERepr -> EOpBits (bv ^. opcodeLens) (bv ^. eLens)
  XRepr -> XOpBits

decodeOpcode :: InstructionSet
             -> FormatRepr k
             -> InstWord 2
             -> Either (Opcode 'X) (Opcode k)
decodeOpcode iset repr bv = opcodeFromOpBits iset (decodeOpBits repr bv)
