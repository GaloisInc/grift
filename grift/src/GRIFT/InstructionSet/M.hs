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

{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-|
Module      : GRIFT.InstructionSet.M
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

RV32M/RV64M multiply extension
-}

module GRIFT.InstructionSet.M
  ( mFromRepr
  ) where

import qualified Data.Parameterized.Map as Map
import Data.Parameterized ( type (<=), LeqProof(LeqProof), Pair(Pair), addIsLeq, addNat )
import Data.Parameterized.List ( List(Nil, (:<)) )
import GHC.TypeNats ( type(+) )

import GRIFT.BitVector.BVApp
import GRIFT.InstructionSet
import GRIFT.Semantics
import GRIFT.Semantics.Utils ( getArchWidth, incrPC, withLeqTrans )
import GRIFT.Types

-- | Get the M instruction set from an explicit 'RVRepr'.
mFromRepr :: RVRepr rv -> InstructionSet rv
mFromRepr rv@(RVRepr RV32Repr (ExtensionsRepr _ MYesRepr _ _ _)) = withRV rv m32
mFromRepr rv@(RVRepr RV64Repr (ExtensionsRepr _ MYesRepr _ _ _)) = withRV rv m64
mFromRepr _ = mempty

-- | M extension (RV32)
m32 :: (KnownRV rv, w ~ RVWidth rv, 5 <= w, MExt << rv) => InstructionSet rv
m32 = instructionSet mEncode mSemantics

-- | M extension (RV64)
m64 :: forall rv w. (KnownRV rv, w ~ RVWidth rv, 64 <= w, MExt << rv) => InstructionSet rv
m64 = withLeqTrans (knownNat @5) (knownNat @64) (knownNat @w) m32
      <> instructionSet m64Encode m64Semantics

mEncode :: MExt << rv => EncodeMap rv
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

m64Encode :: (64 <= RVWidth rv, MExt << rv) => EncodeMap rv
m64Encode = Map.fromList
  [ Pair Mulw  (OpBits RRepr (0b0111011 :< 0b000 :< 0b0000001 :< Nil))
  , Pair Divw  (OpBits RRepr (0b0111011 :< 0b100 :< 0b0000001 :< Nil))
  , Pair Divuw (OpBits RRepr (0b0111011 :< 0b101 :< 0b0000001 :< Nil))
  , Pair Remw  (OpBits RRepr (0b0111011 :< 0b110 :< 0b0000001 :< Nil))
  , Pair Remuw (OpBits RRepr (0b0111011 :< 0b111 :< 0b0000001 :< Nil))
  ]

mSemantics ::
  forall rv w.
  KnownRV rv =>
  (w ~ RVWidth rv, 5 <= w) =>
  MExt << rv =>
  SemanticsMap rv
mSemantics =
  let withLeqSum :: ((w <= w + w) => a) -> a
      withLeqSum a =
        case addIsLeq (knownNat @w) (knownNat @w) of
          LeqProof -> a
  in
  withLeqSum $
  Map.fromList
  [ Pair Mul $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Multiplies x[rs1] by x[rs2] and writes the prod to x[rd]."
      comment "Arithmetic ovexbrflow is ignored."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      assignGPR rd (x_rs1 `mulE` x_rs2)
      incrPC

  , Pair Mulh $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Multiples x[rs1] by x[rs2], treating the values as two's complement numbers."
      comment "Writes the upper half of the prod in x[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      archWidth <- getArchWidth

      let mulWidth = archWidth `addNat` archWidth
          sext_x_rs1 = sextEOrId' archWidth mulWidth x_rs1
          sext_x_rs2 = sextEOrId' archWidth mulWidth x_rs2
          prod = sext_x_rs1 `mulE` sext_x_rs2

      assignGPR rd $ extractE archWidth prod
      incrPC

  , Pair Mulhsu $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Multiplies x[rs1] by x[rs2]."
      comment "Treats x[rs1] as a two's complement number and x[rs2] as an unsigned number."
      comment "Writes the upper half of the prod in x[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      archWidth <- getArchWidth

      let mulWidth  = archWidth `addNat` archWidth
          sext_x_rs1 = sextEOrId' archWidth mulWidth x_rs1
          zext_x_rs2 = zextEOrId' archWidth mulWidth x_rs2
          prod = sext_x_rs1 `mulE` zext_x_rs2

      assignGPR rd $ extractE archWidth prod
      incrPC

  , Pair Mulhu $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Multiplies x[rs1] by x[rs2], treating the values as unsigned numbers."
      comment "Writes the upper half of the prod in x[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      archWidth <- getArchWidth

      let mulWidth  = archWidth `addNat` archWidth
          zext_x_rs1 = zextEOrId' archWidth mulWidth x_rs1
          zext_x_rs2 = zextEOrId' archWidth mulWidth x_rs2
          prod = zext_x_rs1 `mulE` zext_x_rs2

      assignGPR rd $ extractE archWidth prod
      incrPC

  , Pair Div $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as two's complement numbers."
      comment "Writes the quotient to r[d]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      let q = x_rs1 `quotsE` x_rs2

      assignGPR rd $ iteE (x_rs2 `eqE` bvInteger 0) (bvInteger (-1)) q
      incrPC

  , Pair Divu $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as unsigned numbers."
      comment "Writes the quotient to r[d]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      let q = x_rs1 `quotuE` x_rs2

      assignGPR rd $ iteE (x_rs2 `eqE` bvInteger 0) (bvInteger (-1)) q
      incrPC

  , Pair Rem $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as two's complement numbers."
      comment "Writes the quotient to r[d]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      let q = x_rs1 `remsE` x_rs2

      assignGPR rd $ iteE (x_rs2 `eqE` bvInteger 0) x_rs1 q
      incrPC

  , Pair Remu $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as unsigned numbers."
      comment "Writes the quotient to r[d]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      let q = x_rs1 `remuE` x_rs2

      assignGPR rd $ iteE (x_rs2 `eqE` bvInteger 0) x_rs1 q
      incrPC

  ]

m64Semantics ::
  forall rv w.
  KnownRV rv =>
  (w ~ RVWidth rv, 64 <= w) =>
  MExt << rv =>
  SemanticsMap rv
m64Semantics =
  withLeqTrans (knownNat @5) (knownNat @64) (knownNat @w) $
  withLeqTrans (knownNat @33) (knownNat @64) (knownNat @w) $
  Map.fromList
  [ Pair Mulw $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Multiples x[rs1] by x[rs2], truncating the prod to 32 bits."
      comment "Writes the sign-extended result to x[rd]. Arithmetic overflow is ignored."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      assignGPR rd (sextE (extractE' (knownNat @32) (knownNat @0) (x_rs1 `mulE` x_rs2)))
      incrPC
  , Pair Divw $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Divides x[rs1] by x[rs2] as signed integers."
      comment "Writes the sign-extended result to x[rd]. Arithmetic overflow is ignored."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let divisor = extractE' (knownNat @32) (knownNat @0) x_rs1
      let dividend = extractE' (knownNat @32) (knownNat @0) x_rs2

      let q = sextE (extractE' (knownNat @32) (knownNat @0) (divisor `quotsE` dividend))

      assignGPR rd $ iteE (dividend `eqE` bvInteger 0) (bvInteger (-1)) q
      incrPC
  , Pair Divuw $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Divides x[rs1] by x[rs2] as unsigned integers."
      comment "Writes the sign-extended result to x[rd]. Arithmetic overflow is ignored."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let divisor = extractE' (knownNat @32) (knownNat @0) x_rs1
      let dividend = extractE' (knownNat @32) (knownNat @0) x_rs2

      let q = sextE (extractE' (knownNat @32) (knownNat @0) (divisor `quotuE` dividend))

      assignGPR rd $ iteE (dividend `eqE` bvInteger 0) (bvInteger (-1)) q
      incrPC
  , Pair Remw $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as signed integers."
      comment "Writes the sign-extended result to x[rd]. Arithmetic overflow is ignored."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let divisor = extractE' (knownNat @32) (knownNat @0) x_rs1
      let dividend = extractE' (knownNat @32) (knownNat @0) x_rs2

      let q = sextE (extractE' (knownNat @32) (knownNat @0) (divisor `remsE` dividend))

      assignGPR rd $ iteE (dividend `eqE` bvInteger 0) (sextE divisor) q
      incrPC
  , Pair Remuw $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Divides x[rs1] by x[rs2], rounding towards zero, treating them as unsigned integers."
      comment "Writes the sign-extended result to x[rd]. Arithmetic overflow is ignored."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let divisor = extractE' (knownNat @32) (knownNat @0) x_rs1
      let dividend = extractE' (knownNat @32) (knownNat @0) x_rs2

      let q = sextE (extractE' (knownNat @32) (knownNat @0) (divisor `remuE` dividend))

      assignGPR rd $ iteE (dividend `eqE` bvInteger 0) (sextE divisor) q
      incrPC
  ]
