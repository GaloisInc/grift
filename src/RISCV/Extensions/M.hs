{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : RISCV.Extensions.M
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

RV32M multiply extension
-}

module RISCV.Extensions.M
  ( m32
  , m64
  ) where

import Data.Monoid
import qualified Data.Parameterized.Map as Map
import Data.Parameterized

import RISCV.Instruction
import RISCV.InstructionSet
import RISCV.Semantics
import RISCV.Semantics.Helpers

-- | M extension (RV32)
m32 :: InstructionSet 'RV32I
m32 = m'

-- | M extension (RV64)
m64 :: InstructionSet 'RV64I
m64 = m' <> m64'

m' :: KnownArch arch => InstructionSet arch
m' = instructionSet mEncode mSemantics

m64' :: (KnownArch arch, arch >> 'RV64I) => InstructionSet arch
m64' = instructionSet m64Encode m64Semantics

mEncode :: EncodeMap arch
mEncode = Map.fromList
  [ Pair Mul    (ROpBits 0b0110011 0b000 0b0000001)
  , Pair Mulh   (ROpBits 0b0110011 0b001 0b0000001)
  , Pair Mulhsu (ROpBits 0b0110011 0b010 0b0000001)
  , Pair Mulhu  (ROpBits 0b0110011 0b011 0b0000001)
  , Pair Div    (ROpBits 0b0110011 0b100 0b0000001)
  , Pair Divu   (ROpBits 0b0110011 0b101 0b0000001)
  , Pair Rem    (ROpBits 0b0110011 0b110 0b0000001)
  , Pair Remu   (ROpBits 0b0110011 0b111 0b0000001)
  ]

m64Encode :: arch >> 'RV64I => EncodeMap arch
m64Encode = Map.fromList
  [ Pair Mulw  (ROpBits 0b0111011 0b000 0b0000001)
  , Pair Divw  (ROpBits 0b0111011 0b100 0b0000001)
  , Pair Divuw (ROpBits 0b0111011 0b101 0b0000001)
  , Pair Remw  (ROpBits 0b0111011 0b110 0b0000001)
  , Pair Remuw (ROpBits 0b0111011 0b111 0b0000001)
  ]

mSemantics :: KnownArch arch => SemanticsMap arch
mSemantics = Map.fromList
  [ Pair Mul $ getFormula $ do
      comment "Multiplies x[rs1] by x[rs2] and writes the product to x[rd]."
      comment "Arithmetic overflow is ignored."

      (rd, rs1, rs2) <- params

      x_rs1 <- regRead rs1
      x_rs2 <- regRead rs2

      result' <- x_rs1 `muluE` x_rs2
      result  <- extractE 0 result'
      assignReg rd result
      incrPC

  , Pair Mulh $ getFormula $ do
      comment "Multiples x[rs1] by x[rs2], treating the values as two's complement numbers."
      comment "Writes the upper half of the product in x[rd]."

      (rd, rs1, rs2) <- params

      x_rs1 <- regRead rs1
      x_rs2 <- regRead rs2

      result' <- x_rs1 `mulsE` x_rs2
      -- TODO: This is incompatible with RV64. Might make sense to just define
      -- different versions of each extension.
      result  <- extractE 32 result'
      assignReg rd result
      incrPC

  , Pair Mulhsu $ getFormula $ do
      comment "Multiplies x[rs1] by x[rs2], treating x[rs1] as a two's complement number and x[rs2] as an unsigned number."
      comment "Writes the upper half of the product in x[rd]."

      (rd, rs1, rs2) <- params

      x_rs1 <- regRead rs1
      x_rs2 <- regRead rs2

      result' <- x_rs1 `mulsuE` x_rs2
      -- TODO: This is incompatible with RV64. Might make sense to just define
      -- different versions of each extension.
      result  <- extractE 32 result'
      assignReg rd result
      incrPC

  , Pair Mulhu $ getFormula $ do
      comment "Multiplies x[rs1] by x[rs2], treating the values as unsigned numbers."
      comment "Writes the upper half of the product in x[rd]."

      (rd, rs1, rs2) <- params

      x_rs1 <- regRead rs1
      x_rs2 <- regRead rs2

      result' <- x_rs1 `muluE` x_rs2
      -- TODO: This is incompatible with RV64. Might make sense to just define
      -- different versions of each extension.
      result  <- extractE 32 result'
      assignReg rd result
      incrPC

  , Pair Div $ getFormula $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as two's complement numbers."
      comment "Writes the quotient to r[d]."

      rOp divsE

  , Pair Divu $ getFormula $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as unsigned numbers."
      comment "Writes the quotient to r[d]."

      rOp divuE

  , Pair Rem $ getFormula $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as two's complement numbers."
      comment "Writes the quotient to r[d]."

      rOp remsE

  , Pair Remu $ getFormula $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as unsigned numbers."
      comment "Writes the quotient to r[d]."

      rOp remuE

  ]

m64Semantics :: (KnownArch arch, arch >> 'RV64I) => SemanticsMap arch
m64Semantics = Map.fromList
  [ Pair Mulw $ getFormula $ do
      comment "Multiples x[rs1] by x[rs2], truncating the product to 32 bits."
      comment "Writes the sign-extended result to x[rd]. Arithmetic overflow is ignored."

      rOp32 muluE
  , Pair Divw $ getFormula $ do
      comment "Multiples x[rs1] by x[rs2], truncating the product to 32 bits."
      comment "Writes the sign-extended result to x[rd]. Arithmetic overflow is ignored."

      rOp32 divsE
  , Pair Divuw $ getFormula $ do
      comment "Multiples x[rs1] by x[rs2], truncating the product to 32 bits."
      comment "Writes the sign-extended result to x[rd]. Arithmetic overflow is ignored."

      rOp32 divuE
  , Pair Remw $ getFormula $ do
      comment "Multiples x[rs1] by x[rs2], truncating the product to 32 bits."
      comment "Writes the sign-extended result to x[rd]. Arithmetic overflow is ignored."

      rOp32 remsE
  , Pair Remuw $ getFormula $ do
      comment "Multiples x[rs1] by x[rs2], truncating the product to 32 bits."
      comment "Writes the sign-extended result to x[rd]. Arithmetic overflow is ignored."

      rOp32 remuE
  ]
