{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-|
Module      : RISCV.Extensions.M
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

RV32M/RV64M multiply extension
-}

module RISCV.Extensions.M
  ( m32
  , m64
  ) where

import Data.BitVector.Sized.App
import Data.Monoid
import qualified Data.Parameterized.Map as Map
import Data.Parameterized
import Data.Parameterized.List

import RISCV.Extensions.Helpers
import RISCV.InstructionSet
import RISCV.Semantics
import RISCV.Types

-- | M extension (RV32)
m32 :: (KnownArch arch, MExt << exts) => InstructionSet arch exts
m32 = instructionSet mEncode mSemantics

-- | M extension (RV64)
m64 :: (KnownArch arch, 64 <= ArchWidth arch, MExt << exts) => InstructionSet arch exts
m64 = m32 <> instructionSet m64Encode m64Semantics

mEncode :: MExt << exts => EncodeMap arch exts
mEncode = Map.fromList
  [ Pair Mul    (OpBits RRepr (0b0110011 :< 0b000 :< 0b0000001 :< Nil))
  , Pair Mulh   (OpBits RRepr (0b0110011 :< 0b001 :< 0b0000001 :< Nil))
  , Pair Mulhsu (OpBits RRepr (0b0110011 :< 0b010 :< 0b0000001 :< Nil))
  , Pair Mulhu  (OpBits RRepr (0b0110011 :< 0b011 :< 0b0000001 :< Nil))
  , Pair Div    (OpBits RRepr (0b0110011 :< 0b100 :< 0b0000001 :< Nil))
  , Pair Divu   (OpBits RRepr (0b0110011 :< 0b101 :< 0b0000001 :< Nil))
  , Pair Rem    (OpBits RRepr (0b0110011 :< 0b110 :< 0b0000001 :< Nil))
  , Pair Remu   (OpBits RRepr (0b0110011 :< 0b111 :< 0b0000001 :< Nil))
  ]

m64Encode :: (64 <= ArchWidth arch, MExt << exts) => EncodeMap arch exts
m64Encode = Map.fromList
  [ Pair Mulw  (OpBits RRepr (0b0111011 :< 0b000 :< 0b0000001 :< Nil))
  , Pair Divw  (OpBits RRepr (0b0111011 :< 0b100 :< 0b0000001 :< Nil))
  , Pair Divuw (OpBits RRepr (0b0111011 :< 0b101 :< 0b0000001 :< Nil))
  , Pair Remw  (OpBits RRepr (0b0111011 :< 0b110 :< 0b0000001 :< Nil))
  , Pair Remuw (OpBits RRepr (0b0111011 :< 0b111 :< 0b0000001 :< Nil))
  ]

mSemantics :: forall arch exts . (KnownArch arch, MExt << exts) => SemanticsMap arch exts
mSemantics = Map.fromList
  [ Pair Mul $ InstFormula $ getFormula $ do
      comment "Multiplies x[rs1] by x[rs2] and writes the prod to x[rd]."
      comment "Arithmetic ovexbrflow is ignored."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      x_rs1 <- readReg rs1
      x_rs2 <- readReg rs2

      assignReg rd (x_rs1 `mulE` x_rs2)
      incrPC

  , Pair Mulh $ InstFormula $ getFormula $ do
      comment "Multiples x[rs1] by x[rs2], treating the values as two's complement numbers."
      comment "Writes the upper half of the prod in x[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      x_rs1 <- readReg rs1
      x_rs2 <- readReg rs2

      archWidth <- getArchWidth

      let mulWidth  = archWidth `addNat` archWidth
          sext_x_rs1 = sextEWithRepr mulWidth x_rs1
          sext_x_rs2 = sextEWithRepr mulWidth x_rs2
          prod = sext_x_rs1 `mulE` sext_x_rs2

      assignReg rd $ extractE (fromIntegral $ natValue archWidth) prod
      incrPC

  , Pair Mulhsu $ InstFormula $ getFormula $ do
      comment "Multiplies x[rs1] by x[rs2]."
      comment "Treats x[rs1] as a two's complement number and x[rs2] as an unsigned number."
      comment "Writes the upper half of the prod in x[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      x_rs1 <- readReg rs1
      x_rs2 <- readReg rs2

      archWidth <- getArchWidth

      let mulWidth  = archWidth `addNat` archWidth
          sext_x_rs1 = sextEWithRepr mulWidth x_rs1
          zext_x_rs2 = zextEWithRepr mulWidth x_rs2
          prod = sext_x_rs1 `mulE` zext_x_rs2

      assignReg rd $ extractE (fromIntegral $ natValue archWidth) prod
      incrPC

  , Pair Mulhu $ InstFormula $ getFormula $ do
      comment "Multiplies x[rs1] by x[rs2], treating the values as unsigned numbers."
      comment "Writes the upper half of the prod in x[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      x_rs1 <- readReg rs1
      x_rs2 <- readReg rs2

      archWidth <- getArchWidth

      let mulWidth  = archWidth `addNat` archWidth
          zext_x_rs1 = zextEWithRepr mulWidth x_rs1
          zext_x_rs2 = zextEWithRepr mulWidth x_rs2
          prod = zext_x_rs1 `mulE` zext_x_rs2

      assignReg rd $ extractE (fromIntegral $ natValue archWidth) prod
      incrPC

  , Pair Div $ InstFormula $ getFormula $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as two's complement numbers."
      comment "Writes the quotient to r[d]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      x_rs1 <- readReg rs1
      x_rs2 <- readReg rs2

      let q = x_rs1 `quotsE` x_rs2

      assignReg rd $ iteE (x_rs2 `eqE` litBV 0) (litBV (-1)) q
      incrPC

  , Pair Divu $ InstFormula $ getFormula $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as unsigned numbers."
      comment "Writes the quotient to r[d]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      x_rs1 <- readReg rs1
      x_rs2 <- readReg rs2

      let q = x_rs1 `quotuE` x_rs2

      assignReg rd $ iteE (x_rs2 `eqE` litBV 0) (litBV (-1)) q
      incrPC

  , Pair Rem $ InstFormula $ getFormula $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as two's complement numbers."
      comment "Writes the quotient to r[d]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      x_rs1 <- readReg rs1
      x_rs2 <- readReg rs2

      let q = x_rs1 `quotsE` x_rs2

      assignReg rd $ iteE (x_rs2 `eqE` litBV 0) x_rs1 q
      incrPC

      rOp $ \x y -> return (remsE x y)

  , Pair Remu $ InstFormula $ getFormula $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as unsigned numbers."
      comment "Writes the quotient to r[d]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      x_rs1 <- readReg rs1
      x_rs2 <- readReg rs2

      let q = x_rs1 `quotuE` x_rs2

      assignReg rd $ iteE (x_rs2 `eqE` litBV 0) x_rs1 q
      incrPC

      rOp $ \x y -> return (remuE x y)

  ]

m64Semantics :: (KnownArch arch, 64 <= ArchWidth arch, MExt << exts) => SemanticsMap arch exts
m64Semantics = Map.fromList
  [ Pair Mulw $ InstFormula $ getFormula $ do
      comment "Multiples x[rs1] by x[rs2], truncating the prod to 32 bits."
      comment "Writes the sign-extended result to x[rd]. Arithmetic overflow is ignored."

      rOp32 $ \x y -> return (mulE x y)
  , Pair Divw $ InstFormula $ getFormula $ do
      comment "Divides x[rs1] by x[rs2] as signed integers."
      comment "Writes the sign-extended result to x[rd]. Arithmetic overflow is ignored."

      rOp32 $ \x y -> return (quotsE x y)
  , Pair Divuw $ InstFormula $ getFormula $ do
      comment "Divides x[rs1] by x[rs2] as unsigned integers."
      comment "Writes the sign-extended result to x[rd]. Arithmetic overflow is ignored."

      rOp32 $ \x y -> return (quotuE x y)
  , Pair Remw $ InstFormula $ getFormula $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as signed integers."
      comment "Writes the sign-extended result to x[rd]. Arithmetic overflow is ignored."

      rOp32 $ \x y -> return (remsE x y)
  , Pair Remuw $ InstFormula $ getFormula $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as unsigned integers."
      comment "Writes the sign-extended result to x[rd]. Arithmetic overflow is ignored."

      rOp32 $ \x y -> return (remuE x y)
  ]
