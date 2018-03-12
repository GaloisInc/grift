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
import Data.BitVector.Sized
import Data.Parameterized

import RISCV.Instruction
import RISCV.Instruction.Lens

-- | Decode an instruction word. Since we won't know the format ahead of time, we
-- have to hide the format parameter of the return type with 'Some'.
decode :: BitVector 32 -> Some Instruction
decode bvec = case decodeFormat bvec of
  Some repr -> case decodeOpcode repr bvec of
    Right op     -> Some $ Inst op (decodeOperands repr bvec)
    Left Illegal -> Some $ Inst Illegal (decodeOperands XRepr bvec)

-- TODO: We could probably automatically derive this from opcodeOpBitsMap somehow,
-- but this seems simpler.
-- | First, get the format
decodeFormat :: BitVector 32 -> Some FormatRepr
decodeFormat bvec = case ( bvExtract 0  bvec :: BitVector 7
                         , bvExtract 12 bvec :: BitVector 3) of
  (BV _ 0b0110011, _) -> Some RRepr

  (BV _ 0b1110011, 0b000) -> Some ERepr -- special case.

  (BV _ 0b1100111, _) -> Some IRepr
  (BV _ 0b0000011, _) -> Some IRepr
  (BV _ 0b0010011, _) -> Some IRepr
  (BV _ 0b0001111, _) -> Some IRepr
  (BV _ 0b1110011, _) -> Some IRepr

  (BV _ 0b0100011, _) -> Some SRepr

  (BV _ 0b1100011, _) -> Some BRepr

  (BV _ 0b0110111, _) -> Some URepr
  (BV _ 0b0010111, _) -> Some URepr

  (BV _ 0b1101111, _) -> Some JRepr

  _ ->              Some XRepr

-- | From the format, get the operands
decodeOperands :: FormatRepr k -> BitVector 32 -> Operands k
decodeOperands repr bvec = case repr of
  RRepr -> ROperands (bvec ^. rdLens)  (bvec ^. rs1Lens) (bvec ^. rs2Lens)
  IRepr -> IOperands (bvec ^. rdLens)  (bvec ^. rs1Lens) (bvec ^. imm12ILens)
  SRepr -> SOperands (bvec ^. rs1Lens) (bvec ^. rs2Lens) (bvec ^. imm12SLens)
  BRepr -> BOperands (bvec ^. rs1Lens) (bvec ^. rs2Lens) (bvec ^. imm12BLens)
  URepr -> UOperands (bvec ^. rdLens)  (bvec ^. imm20ULens)
  JRepr -> JOperands (bvec ^. rdLens)  (bvec ^. imm20JLens)
  ERepr -> EOperands
  XRepr -> XOperands (bvec ^. illegalLens)

-- | From the format, get the opbits
decodeOpBits :: FormatRepr k -> BitVector 32 -> OpBits k
decodeOpBits repr bvec = case repr of
  RRepr -> ROpBits (bvec ^. opcodeLens) (bvec ^. funct3Lens) (bvec ^. funct7Lens)
  IRepr -> IOpBits (bvec ^. opcodeLens) (bvec ^. funct3Lens)
  SRepr -> SOpBits (bvec ^. opcodeLens) (bvec ^. funct3Lens)
  BRepr -> BOpBits (bvec ^. opcodeLens) (bvec ^. funct3Lens)
  URepr -> UOpBits (bvec ^. opcodeLens)
  JRepr -> JOpBits (bvec ^. opcodeLens)
  ERepr -> EOpBits (bvec ^. opcodeLens) (bvec ^. eLens)
  XRepr -> XOpBits

decodeOpcode :: FormatRepr k -> BitVector 32 -> Either (Opcode 'X) (Opcode k)
decodeOpcode repr bvec = opcodeFromOpBits (decodeOpBits repr bvec)
