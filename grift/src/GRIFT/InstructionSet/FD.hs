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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-|
Module      : GRIFT.InstructionSet.FD
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

F and D extensions for RV32 and RV64.
-}

module GRIFT.InstructionSet.FD
  ( fdFromRepr
  ) where

import Data.BitVector.Sized.App
import Data.BitVector.Sized.Float.App
import qualified Data.Parameterized.Map as Map
import Data.Parameterized
import Data.Parameterized.List

import GRIFT.InstructionSet
import GRIFT.InstructionSet.Utils
import GRIFT.Semantics
import GRIFT.Types

-- | Get the F/D instruction set from an explicit 'RVRepr'.
fdFromRepr :: RVRepr rv -> InstructionSet rv
fdFromRepr (RVRepr RV32Repr (ExtensionsRepr _ _ _ FYesDNoRepr _)) = f32
fdFromRepr (RVRepr RV32Repr (ExtensionsRepr _ _ _ FDYesRepr _))   = f32 <> d32
fdFromRepr (RVRepr RV64Repr (ExtensionsRepr _ _ _ FYesDNoRepr _)) = f64
fdFromRepr (RVRepr RV64Repr (ExtensionsRepr _ _ _ FDYesRepr _))   = f64 <> d64
fdFromRepr _ = mempty

-- | F extension (RV32)
f32 :: (KnownRVWidth rv, KnownRVFloatType rv, FExt << rv) => InstructionSet rv
f32 = instructionSet fEncode fSemantics

-- | F extension (RV64)
f64 :: (KnownRVWidth rv, KnownRVFloatType rv, FExt << rv, 64 <= RVWidth rv) => InstructionSet rv
f64 = f32 <> instructionSet f64Encode f64Semantics

-- | D extension (RV32)
d32 :: (KnownRVWidth rv, KnownRVFloatWidth rv, KnownRVFloatType rv, FExt << rv, DExt << rv) => InstructionSet rv
d32 = instructionSet dEncode dSemantics

-- | D extension (RV64)
d64 :: (KnownRVWidth rv, KnownRVFloatWidth rv, KnownRVFloatType rv, FExt << rv, DExt << rv, 64 <= RVWidth rv) => InstructionSet rv
d64 = d32 <> instructionSet d64Encode d64Semantics

fEncode :: FExt << rv => EncodeMap rv
fEncode = Map.fromList
  [ Pair Flw       (OpBits IRepr  (0b0000111 :< 0b010 :< Nil))
  , Pair Fsw       (OpBits SRepr  (0b0100111 :< 0b010 :< Nil))
  , Pair Fmadd_s   (OpBits R4Repr (0b1000011 :< 0b00 :< Nil))
  , Pair Fmsub_s   (OpBits R4Repr (0b1000111 :< 0b00 :< Nil))
  , Pair Fnmsub_s  (OpBits R4Repr (0b1001011 :< 0b00 :< Nil))
  , Pair Fnmadd_s  (OpBits R4Repr (0b1001111 :< 0b00 :< Nil))
  , Pair Fadd_s    (OpBits R3Repr (0b1010011 :< 0b0000000 :< Nil))
  , Pair Fsub_s    (OpBits R3Repr (0b1010011 :< 0b0000100 :< Nil))
  , Pair Fmul_s    (OpBits R3Repr (0b1010011 :< 0b0001000 :< Nil))
  , Pair Fdiv_s    (OpBits R3Repr (0b1010011 :< 0b0001100 :< Nil))
  , Pair Fsqrt_s   (OpBits R2Repr (0b1010011 :< 0b010110000000 :< Nil))
  , Pair Fsgnj_s   (OpBits RRepr  (0b1010011 :< 0b000 :< 0b0010000 :< Nil))
  , Pair Fsgnjn_s  (OpBits RRepr  (0b1010011 :< 0b001 :< 0b0010000 :< Nil))
  , Pair Fsgnjx_s  (OpBits RRepr  (0b1010011 :< 0b010 :< 0b0010000 :< Nil))
  , Pair Fmin_s    (OpBits RRepr  (0b1010011 :< 0b000 :< 0b0010100 :< Nil))
  , Pair Fmax_s    (OpBits RRepr  (0b1010011 :< 0b001 :< 0b0010100 :< Nil))
  , Pair Fcvt_w_s  (OpBits R2Repr (0b1010011 :< 0b110000000000 :< Nil))
  , Pair Fcvt_wu_s (OpBits R2Repr (0b1010011 :< 0b110000000001 :< Nil))
  , Pair Fmv_x_w   (OpBits RXRepr (0b1010011 :< 0b000 :< 0b111000000000 :< Nil))
  , Pair Feq_s     (OpBits RRepr  (0b1010011 :< 0b010 :< 0b1010000 :< Nil))
  , Pair Flt_s     (OpBits RRepr  (0b1010011 :< 0b001 :< 0b1010000 :< Nil))
  , Pair Fle_s     (OpBits RRepr  (0b1010011 :< 0b000 :< 0b1010000 :< Nil))
  , Pair Fclass_s  (OpBits RXRepr (0b1010011 :< 0b001 :< 0b111000000000 :< Nil))
  , Pair Fcvt_s_w  (OpBits R2Repr (0b1010011 :< 0b110100000000 :< Nil))
  , Pair Fcvt_s_wu (OpBits R2Repr (0b1010011 :< 0b110100000001 :< Nil))
  , Pair Fmv_w_x   (OpBits RXRepr (0b1010011 :< 0b000 :< 0b111100000000 :< Nil))
  ]

fSemantics :: forall rv . (KnownRVWidth rv, KnownRVFloatType rv, FExt << rv) => SemanticsMap rv
fSemantics = Map.fromList
  [ Pair Flw $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Loads a single-precision float from memory address x[rs1] + sext(offset)."
      comment "Writes the result to f[rd]."

      rd :< rs1 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      mVal <- nanBox32 $ readMem (knownNat @4) (x_rs1 `addE` sextE offset)

      assignFPR rd mVal
      incrPC
  , Pair Fsw $ instSemantics (Rs1 :< Rs2 :< Imm12 :< Nil) $ do
      comment "Stores the single-precision float in register f[rs2] to memory at address x[rs1] + sext(offset)."

      rs1 :< rs2 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let f_rs2 = readFPR rs2

      assignMem (knownNat @4) (x_rs1 `addE` sextE offset) (extractE 0 f_rs2)
      incrPC
  , Pair Fmadd_s $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Rs3 :< Nil) $ do
      comment "Multiplies the single-precision floats in f[rs1] and f[rs2]."
      comment "Adds the unrounded product to the single-precision float in f[rs3]."
      comment "Writes the rounded single-precision result to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< rs3 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        f_rs1 <- unBox32 (readFPR rs1)
        f_rs2 <- unBox32 (readFPR rs2)
        f_rs3 <- unBox32 (readFPR rs3)
        let (res', flags) = getFResCanonical32 $ f32MulAddE rm f_rs1 f_rs2 f_rs3
        res <- nanBox32 res'

        assignFPR rd res
        raiseFPExceptions flags
        incrPC
  , Pair Fmsub_s $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Rs3 :< Nil) $ do
      comment "Multiplies the single-precision floats in f[rs1] and f[rs2]."
      comment "Subtracts the single-precision float in f[rs3] from the unrounded product."
      comment "Writes the rounded single-precision result to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< rs3 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        f_rs1 <- unBox32 (readFPR rs1)
        f_rs2 <- unBox32 (readFPR rs2)
        f_rs3 <- unBox32 (readFPR rs3)
        let (res', flags) = getFResCanonical32 $ f32MulAddE rm f_rs1 f_rs2 (negate32 f_rs3)
        res <- nanBox32 res'

        assignFPR rd res
        raiseFPExceptions flags
        incrPC
  , Pair Fnmsub_s $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Rs3 :< Nil) $ do
      comment "Multiplies the single-precision floats in f[rs1] and f[rs2], negating the result."
      comment "Subtracts the single-precision float in f[rs3] from the unrounded product."
      comment "Writes the rounded single-precision result to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< rs3 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        f_rs1 <- unBox32 (readFPR rs1)
        f_rs2 <- unBox32 (readFPR rs2)
        f_rs3 <- unBox32 (readFPR rs3)
        let (res', flags) = getFResCanonical32 $ f32MulAddE rm (negate32 f_rs1) f_rs2 f_rs3
        res <- nanBox32 res'

        assignFPR rd res
        raiseFPExceptions flags
        incrPC
  , Pair Fnmadd_s $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Rs3 :< Nil) $ do
      comment "Multiplies the single-precision floats in f[rs1] and f[rs2], negating the result."
      comment "Adds the single-precision float in f[rs3] to the unrounded product."
      comment "Writes the rounded single-precision result to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< rs3 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        f_rs1 <- unBox32 (readFPR rs1)
        f_rs2 <- unBox32 (readFPR rs2)
        f_rs3 <- unBox32 (readFPR rs3)
        let (res', flags) = getFResCanonical32 $ f32MulAddE rm (negate32 f_rs1) f_rs2 (negate32 f_rs3)
        res <- nanBox32 res'

        assignFPR rd res
        raiseFPExceptions flags
        incrPC
  , Pair Fadd_s $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Nil) $ do
      comment "Adds the single-precision float in registers f[rs1] and f[rs2]."
      comment "Writes the rounded single-precision sum to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        f_rs1 <- unBox32 (readFPR rs1)
        f_rs2 <- unBox32 (readFPR rs2)
        let (res', flags) = getFResCanonical32 $ f32AddE rm f_rs1 f_rs2
        res <- nanBox32 res'

        assignFPR rd res
        raiseFPExceptions flags
        incrPC
  , Pair Fsub_s $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Nil) $ do
      comment "Subtracts the single-precision float in register f[rs2] from f[rs1]."
      comment "Writes the rounded single-precision difference to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        f_rs1 <- unBox32 (readFPR rs1)
        f_rs2 <- unBox32 (readFPR rs2)
        let (res', flags) = getFResCanonical32 $ f32SubE rm f_rs1 f_rs2
        res <- nanBox32 res'

        assignFPR rd res
        raiseFPExceptions flags
        incrPC
  , Pair Fmul_s $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Nil) $ do
      comment "Multiplies the single-precision float in registers f[rs1] and f[rs2]."
      comment "Writes the rounded single-precision sum to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        f_rs1 <- unBox32 (readFPR rs1)
        f_rs2 <- unBox32 (readFPR rs2)
        let (res', flags) = getFResCanonical32 $ f32MulE rm f_rs1 f_rs2
        res <- nanBox32 res'

        assignFPR rd res
        raiseFPExceptions flags
        incrPC
  , Pair Fdiv_s $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Nil) $ do
      comment "Divides the single-precision float in register f[rs1] by f[rs2]."
      comment "Writes the rounded single-precision difference to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        f_rs1 <- unBox32 (readFPR rs1)
        f_rs2 <- unBox32 (readFPR rs2)
        let (res', flags) = getFResCanonical32 $ f32DivE rm f_rs1 f_rs2
        res <- nanBox32 res'

        assignFPR rd res
        raiseFPExceptions flags
        incrPC
  , Pair Fsqrt_s $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Computes the square root of the single-precision float in register f[rs1]."
      comment "Writes the rounded single-precision result to f[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        f_rs1 <- unBox32 (readFPR rs1)
        let (res', flags) = getFResCanonical32 $ f32SqrtE rm f_rs1
        res <- nanBox32 res'

        assignFPR rd res
        raiseFPExceptions flags
        incrPC
  , Pair Fsgnj_s $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Constructs a new single-precision float from the significant and exponent of f[rs1]."
      comment "Uses the sign of f[rs2], and writes the result to f[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs
      f_rs1 <- unBox32 (readFPR rs1)
      f_rs2 <- unBox32 (readFPR rs2)

      let res_sign = f32Sgn (extractE 0 f_rs2)
      let res_rst  = extractE' (knownNat @31) 0 f_rs1
      let res' = res_sign `concatE` res_rst
      res <- nanBox32 res'

      assignFPR rd res
      incrPC
  , Pair Fsgnjn_s $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Constructs a new single-precision float from the significant and exponent of f[rs1]."
      comment "Uses the opposite sign of f[rs2], and writes the result to f[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs
      f_rs1 <- unBox32 (readFPR rs1)
      f_rs2 <- unBox32 (readFPR rs2)

      let res_sign = notE (f32Sgn (extractE 0 f_rs2))
      let res_rst  = extractE' (knownNat @31) 0 f_rs1
      let res' = zextE (res_sign `concatE` res_rst)
      res <- nanBox32 res'

      assignFPR rd res
      incrPC
  , Pair Fsgnjx_s $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Constructs a new single-precision float from the significant and exponent of f[rs1]."
      comment "Uses the xor of the signs of f[rs1] and f[rs2], and writes the result to f[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs
      f_rs1 <- unBox32 (readFPR rs1)
      f_rs2 <- unBox32 (readFPR rs2)

      let res_sign = (f32Sgn (extractE 0 f_rs1) `xorE` f32Sgn (extractE 0 f_rs2))
      let res_rst  = extractE' (knownNat @31) 0 f_rs1
      let res' = zextE (res_sign `concatE` res_rst)
      res <- nanBox32 res'

      assignFPR rd res
      incrPC
  , Pair Fmin_s $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Computes the smaller of the single-precision floats in registers f[rs1] and f[rs2]."
      comment "Copies the result to f[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs
      f_rs1 <- unBox32 (readFPR rs1)
      f_rs2 <- unBox32 (readFPR rs2)
      let (cmp, _) = getFRes $ f32LeE f_rs1 f_rs2 -- ignore flags

      let res' = cases
            [ (isNaN32 f_rs1 `andE` isNaN32 f_rs2, canonicalNaN32)
            , (isNaN32 f_rs1, f_rs2)
            , (isNaN32 f_rs2, f_rs1)
            , ((f_rs1 `eqE` negZero32) `andE` (f_rs2 `eqE` posZero32), f_rs1)
            , ((f_rs1 `eqE` posZero32) `andE` (f_rs2 `eqE` negZero32), f_rs2)
            ]
            $ iteE cmp f_rs1 f_rs2
      res <- nanBox32 res'
      let invalid = isSNaN32 f_rs1 `orE` isSNaN32 f_rs2
      let flags = iteE invalid (litBV 0x10) (litBV 0x00)

      assignFPR rd res
      raiseFPExceptions flags
      incrPC
  , Pair Fmax_s $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Computes the larger of the single-precision floats in registers f[rs1] and f[rs2]."
      comment "Copies the result to f[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs
      f_rs1 <- unBox32 (readFPR rs1)
      f_rs2 <- unBox32 (readFPR rs2)
      let (cmp, _) = getFRes $ f32LeE f_rs1 f_rs2 -- ignore flags

      let res' = cases
            [ (isNaN32 f_rs1 `andE` isNaN32 f_rs2, canonicalNaN32)
            , (isNaN32 f_rs1, f_rs2)
            , (isNaN32 f_rs2, f_rs1)
            , ((f_rs1 `eqE` negZero32) `andE` (f_rs2 `eqE` posZero32), f_rs2)
            , ((f_rs1 `eqE` posZero32) `andE` (f_rs2 `eqE` negZero32), f_rs1)
            ]
            $ iteE cmp f_rs2 f_rs1
      res <- nanBox32 res'
      let invalid = isSNaN32 f_rs1 `orE` isSNaN32 f_rs2
      let flags = iteE invalid (litBV 0x10) (litBV 0x00)

      assignFPR rd res
      raiseFPExceptions flags
      incrPC
  , Pair Fcvt_w_s $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the single-precision float in f[rs1] to a 32-bit signed integer."
      comment "Writes the result to x[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        f_rs1 <- unBox32 (readFPR rs1)
        let (res, flags) = getFRes $ f32ToI32E rm (extractE 0 f_rs1)

        assignGPR rd (sextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fcvt_wu_s $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the single-precision float in f[rs1] to a 32-bit unsigned integer."
      comment "Writes the result to x[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        f_rs1 <- unBox32 (readFPR rs1)
        let (res, flags) = getFRes $ f32ToUi32E rm (extractE 0 f_rs1)

        assignGPR rd (sextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fmv_x_w $ instSemantics (Rd :< Rs1 :< Nil) $ do
      comment "Copies the single-precision float in register f[rs1] to x[rd]."
      comment "Sign-extends the result."

      rd :< rs1 :< Nil <- operandEs
      let f_rs1 = readFPR rs1

      assignGPR rd (sextE (extractE' (knownNat @32) 0 f_rs1))
      incrPC
  , Pair Feq_s $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Writes 1 to x[rd] if the single-precision float in f[rs1] equals f[rs2]."
      comment "Writes 0 to x[rd] if not."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      f_rs1 <- unBox32 (readFPR rs1)
      f_rs2 <- unBox32 (readFPR rs2)
      let (res, flags) = getFRes $ f32EqE f_rs1 f_rs2

      assignGPR rd (zextE res)
      raiseFPExceptions flags
      incrPC
  , Pair Flt_s $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Writes 1 to x[rd] if the single-precision float in f[rs1] is less than f[rs2]."
      comment "Writes 0 to x[rd] if not."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      f_rs1 <- unBox32 (readFPR rs1)
      f_rs2 <- unBox32 (readFPR rs2)
      let (res, flags) = getFRes $ f32LtE f_rs1 f_rs2

      assignGPR rd (zextE res)
      raiseFPExceptions flags
      incrPC
  , Pair Fle_s $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Writes 1 to x[rd] if the single-precision float in f[rs1] is less than or equal to f[rs2]."
      comment "Writes 0 to x[rd] if not."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      f_rs1 <- unBox32 (readFPR rs1)
      f_rs2 <- unBox32 (readFPR rs2)
      let (res, flags) = getFRes $ f32LeE f_rs1 f_rs2

      assignGPR rd (zextE res)
      raiseFPExceptions flags
      incrPC
  , Pair Fclass_s $ instSemantics (Rd :< Rs1 :< Nil) $ do
      comment "Writes to x[rd] a mask indicating the class of the single-precision float in f[rs1]."
      comment "Exactly one bit in x[rd] is set. Table not included in this comment."

      rd :< rs1 :< Nil <- operandEs
      f_rs1 <- unBox32 (readFPR rs1)
      let res = cases
            [ (f_rs1 `eqE` negInfinity32, litBV 0x1)
            , (f32Sgn f_rs1 `andE` isNormal32 f_rs1, litBV 0x2)
            , (f32Sgn f_rs1 `andE` isSubnormal32 f_rs1, litBV 0x4)
            , (f_rs1 `eqE` negZero32, litBV 0x8)
            , (f_rs1 `eqE` posZero32, litBV 0x10)
            , (notE (f32Sgn f_rs1) `andE` isSubnormal32 f_rs1, litBV 0x20)
            , (notE (f32Sgn f_rs1) `andE` isNormal32 f_rs1, litBV 0x40)
            , (f_rs1 `eqE` posInfinity32, litBV 0x80)
            , (isSNaN32 f_rs1, litBV 0x100)
            , (isQNaN32 f_rs1, litBV 0x200)
            ]
            (litBV 0x0)

      assignGPR rd res
      incrPC
  , Pair Fcvt_s_w $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the 32-bit signed integer in x[rs1] to a single-precision float."
      comment "Writes the result to f[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let x_rs1 = readGPR rs1
            (res', flags) = getFResCanonical32 $ i32ToF32E rm (extractE 0 x_rs1)
        res <- nanBox32 res'

        assignFPR rd res
        raiseFPExceptions flags
        incrPC
  , Pair Fcvt_s_wu $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the 32-bit unsigned integer in x[rs1] to a single-precision float."
      comment "Writes the result to f[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let x_rs1 = readGPR rs1
            (res', flags) = getFResCanonical32 $ ui32ToF32E rm (extractE 0 x_rs1)
        res <- nanBox32 res'

        assignFPR rd res
        raiseFPExceptions flags
        incrPC
  , Pair Fmv_w_x $ instSemantics (Rd :< Rs1 :< Nil) $ do
      comment "Copies the single-precision float in register x[rs1] to f[rd]."
      comment "Sign-extends the result."

      rd :< rs1 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      res <- nanBox32 (extractE' (knownNat @32) 0 x_rs1)

      assignFPR rd res
      incrPC
  ]

f64Encode :: (64 <= RVWidth rv, FExt << rv) => EncodeMap rv
f64Encode = Map.fromList
  [ Pair Fcvt_l_s  (OpBits R2Repr (0b1010011 :< 0b110000000010 :< Nil))
  , Pair Fcvt_lu_s (OpBits R2Repr (0b1010011 :< 0b110000000011 :< Nil))
  , Pair Fcvt_s_l  (OpBits R2Repr (0b1010011 :< 0b110100000010 :< Nil))
  , Pair Fcvt_s_lu (OpBits R2Repr (0b1010011 :< 0b110100000011 :< Nil))
  ]

f64Semantics :: (KnownRVWidth rv, KnownRVFloatType rv, FExt << rv, 64 <= RVWidth rv) => SemanticsMap rv
f64Semantics = Map.fromList
  [ Pair Fcvt_l_s $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the single-precision float in f[rs1] to a 64-bit signed integer."
      comment "Writes the result to x[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        f_rs1 <- unBox32 (readFPR rs1)
        let (res, flags) = getFRes $ f32ToI64E rm (extractE 0 f_rs1)

        assignGPR rd (sextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fcvt_lu_s $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the single-precision float in f[rs1] to a 64-bit unsigned integer."
      comment "Writes the result to x[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        f_rs1 <- unBox32 (readFPR rs1)
        let (res, flags) = getFRes $ f32ToUi64E rm (extractE 0 f_rs1)

        assignGPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fcvt_s_l $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the 64-bit signed integer in x[rs1] to a single-precision float."
      comment "Writes the result to f[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let x_rs1 = readGPR rs1
        let (res', flags) = getFResCanonical32 $ i64ToF32E rm (extractE 0 x_rs1)
        res <- nanBox32 res'

        assignFPR rd res
        raiseFPExceptions flags
        incrPC
  , Pair Fcvt_s_lu $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the 64-bit unsigned integer in x[rs1] to a single-precision float."
      comment "Writes the result to f[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let x_rs1 = readGPR rs1
        let (res', flags) = getFResCanonical32 $ ui64ToF32E rm (extractE 0 x_rs1)
        res <- nanBox32 res'

        assignFPR rd res
        raiseFPExceptions flags
        incrPC
  ]

dEncode :: DExt << rv => EncodeMap rv
dEncode = Map.fromList
  [ Pair Fld       (OpBits IRepr  (0b0000111 :< 0b011 :< Nil))
  , Pair Fsd       (OpBits SRepr  (0b0100111 :< 0b011 :< Nil))
  , Pair Fmadd_d   (OpBits R4Repr (0b1000011 :< 0b01 :< Nil))
  , Pair Fmsub_d   (OpBits R4Repr (0b1000111 :< 0b01 :< Nil))
  , Pair Fnmsub_d  (OpBits R4Repr (0b1001011 :< 0b01 :< Nil))
  , Pair Fnmadd_d  (OpBits R4Repr (0b1001111 :< 0b01 :< Nil))
  , Pair Fadd_d    (OpBits R3Repr (0b1010011 :< 0b0000001 :< Nil))
  , Pair Fsub_d    (OpBits R3Repr (0b1010011 :< 0b0000101 :< Nil))
  , Pair Fmul_d    (OpBits R3Repr (0b1010011 :< 0b0001001 :< Nil))
  , Pair Fdiv_d    (OpBits R3Repr (0b1010011 :< 0b0001101 :< Nil))
  , Pair Fsqrt_d   (OpBits R2Repr (0b1010011 :< 0b010110100000 :< Nil))
  , Pair Fsgnj_d   (OpBits RRepr  (0b1010011 :< 0b000 :< 0b0010001 :< Nil))
  , Pair Fsgnjn_d  (OpBits RRepr  (0b1010011 :< 0b001 :< 0b0010001 :< Nil))
  , Pair Fsgnjx_d  (OpBits RRepr  (0b1010011 :< 0b010 :< 0b0010001 :< Nil))
  , Pair Fmin_d    (OpBits RRepr  (0b1010011 :< 0b000 :< 0b0010101 :< Nil))
  , Pair Fmax_d    (OpBits RRepr  (0b1010011 :< 0b001 :< 0b0010101 :< Nil))
  , Pair Fcvt_s_d  (OpBits R2Repr (0b1010011 :< 0b010000000001 :< Nil))
  , Pair Fcvt_d_s  (OpBits R2Repr (0b1010011 :< 0b010000100000 :< Nil))
  , Pair Feq_d     (OpBits RRepr  (0b1010011 :< 0b010 :< 0b1010001 :< Nil))
  , Pair Flt_d     (OpBits RRepr  (0b1010011 :< 0b001 :< 0b1010001 :< Nil))
  , Pair Fle_d     (OpBits RRepr  (0b1010011 :< 0b000 :< 0b1010001 :< Nil))
  , Pair Fclass_d  (OpBits RXRepr (0b1010011 :< 0b001 :< 0b111000100000 :< Nil))
  , Pair Fcvt_w_d  (OpBits R2Repr (0b1010011 :< 0b110000100000 :< Nil))
  , Pair Fcvt_wu_d (OpBits R2Repr (0b1010011 :< 0b110000100001 :< Nil))
  , Pair Fcvt_d_w  (OpBits R2Repr (0b1010011 :< 0b110100100000 :< Nil))
  , Pair Fcvt_d_wu (OpBits R2Repr (0b1010011 :< 0b110100100001 :< Nil))
  ]

dSemantics :: (KnownRVWidth rv, KnownRVFloatType rv, KnownRVFloatWidth rv, FExt << rv, DExt << rv) => SemanticsMap rv
dSemantics = Map.fromList
  [ Pair Fld $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Loads a double-precision float from memory address x[rs1] + sext(offset)."
      comment "Writes the result to f[rd]."

      rd :< rs1 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let mVal  = readMem (knownNat @8) (x_rs1 `addE` sextE offset)

      assignFPR rd (zextE mVal)
      incrPC
  , Pair Fsd $ instSemantics (Rs1 :< Rs2 :< Imm12 :< Nil) $ do
      comment "Stores the double-precision float in register f[rs2] to memory at address x[rs1] + sext(offset)."

      rs1 :< rs2 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let f_rs2 = readFPR rs2

      assignMem (knownNat @8) (x_rs1 `addE` sextE offset) (extractE 0 f_rs2)
      incrPC
  , Pair Fmadd_d $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Rs2 :< Nil) $ do
      comment "Multiplies the double-precision floats in f[rs1] and f[rs2]."
      comment "Adds the unrounded product to the double-precision float in f[rs3]."
      comment "Writes the rounded double-precision result to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< rs3 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFPR rs1)
        let f_rs2 = extractE 0 (readFPR rs2)
        let f_rs3 = extractE 0 (readFPR rs3)
        let (res, flags) = getFResCanonical64 $ f64MulAddE rm f_rs1 f_rs2 f_rs3

        assignFPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fmsub_d $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Rs3 :< Nil) $ do
      comment "Multiplies the double-precision floats in f[rs1] and f[rs2]."
      comment "Subtracts the double-precision float in f[rs3] from the unrounded product."
      comment "Writes the rounded double-precision result to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< rs3 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFPR rs1)
        let f_rs2 = extractE 0 (readFPR rs2)
        let f_rs3 = extractE 0 (readFPR rs3)
        let (res, flags) = getFResCanonical64 $ f64MulAddE rm f_rs1 f_rs2 (negate64 f_rs3)

        assignFPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fnmsub_d $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Rs3 :< Nil) $ do
      comment "Multiplies the double-precision floats in f[rs1] and f[rs2], negating the result."
      comment "Subtracts the double-precision float in f[rs3] from the unrounded product."
      comment "Writes the rounded double-precision result to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< rs3 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFPR rs1)
        let f_rs2 = extractE 0 (readFPR rs2)
        let f_rs3 = extractE 0 (readFPR rs3)
        let (res, flags) = getFResCanonical64 $ f64MulAddE rm (negate64 f_rs1) f_rs2 f_rs3

        assignFPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fnmadd_d $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Rs3 :< Nil) $ do
      comment "Multiplies the double-precision floats in f[rs1] and f[rs2], negating the result."
      comment "Adds the double-precision float in f[rs3] to the unrounded product."
      comment "Writes the rounded double-precision result to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< rs3 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFPR rs1)
        let f_rs2 = extractE 0 (readFPR rs2)
        let f_rs3 = extractE 0 (readFPR rs3)
        let (res, flags) = getFResCanonical64 $ f64MulAddE rm (negate64 f_rs1) f_rs2 (negate64 f_rs3)

        assignFPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fadd_d $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Nil) $ do
      comment "Adds the double-precision float in registers f[rs1] and f[rs2]."
      comment "Writes the rounded double-precision sum to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFPR rs1)
        let f_rs2 = extractE 0 (readFPR rs2)
        let (res, flags) = getFResCanonical64 $ f64AddE rm f_rs1 f_rs2

        assignFPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fsub_d $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Nil) $ do
      comment "Subtracts the double-precision float in register f[rs2] from f[rs1]."
      comment "Writes the rounded double-precision difference to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFPR rs1)
        let f_rs2 = extractE 0 (readFPR rs2)
        let (res, flags) = getFResCanonical64 $ f64SubE rm f_rs1 f_rs2

        assignFPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fmul_d $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Nil) $ do
      comment "Multiplies the double-precision float in registers f[rs1] and f[rs2]."
      comment "Writes the rounded double-precision sum to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFPR rs1)
        let f_rs2 = extractE 0 (readFPR rs2)
        let (res, flags) = getFResCanonical64 $ f64MulE rm f_rs1 f_rs2

        assignFPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fdiv_d $ instSemantics (Rd :< Rm :< Rs1 :< Rs2 :< Nil) $ do
      comment "Divides the double-precision float in register f[rs1] by f[rs2]."
      comment "Writes the rounded double-precision difference to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFPR rs1)
        let f_rs2 = extractE 0 (readFPR rs2)
        let (res, flags) = getFResCanonical64 $ f64DivE rm f_rs1 f_rs2

        assignFPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fsqrt_d $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Computes the square root of the double-precision float in register f[rs1]."
      comment "Writes the rounded double-precision result to f[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFPR rs1)
        let (res, flags) = getFResCanonical64 $ f64SqrtE rm f_rs1

        assignFPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fsgnj_d $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Constructs a new double-precision float from the significant and exponent of f[rs1]."
      comment "Uses the sign of f[rs2], and writes the result to f[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs
      let f_rs1 = readFPR rs1
      let f_rs2 = readFPR rs2

      let res_sign = f64Sgn (extractE 0 f_rs2)
      let res_rst  = extractE' (knownNat @63) 0 f_rs1
      let res = zextE (res_sign `concatE` res_rst)

      assignFPR rd res
      incrPC
  , Pair Fsgnjn_d $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Constructs a new double-precision float from the significant and exponent of f[rs1]."
      comment "Uses the opposite sign of f[rs2], and writes the result to f[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs
      let f_rs1 = readFPR rs1
      let f_rs2 = readFPR rs2

      let res_sign = notE (f64Sgn (extractE 0 f_rs2))
      let res_rst  = extractE' (knownNat @63) 0 f_rs1
      let res = zextE (res_sign `concatE` res_rst)

      assignFPR rd res
      incrPC
  , Pair Fsgnjx_d $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Constructs a new double-precision float from the significant and exponent of f[rs1]."
      comment "Uses the xor of the signs of f[rs1] and f[rs2], and writes the result to f[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs
      let f_rs1 = readFPR rs1
      let f_rs2 = readFPR rs2

      let res_sign = (f64Sgn (extractE 0 f_rs1) `xorE` f64Sgn (extractE 0 f_rs2))
      let res_rst  = extractE' (knownNat @63) 0 f_rs1
      let res = zextE (res_sign `concatE` res_rst)

      assignFPR rd res
      incrPC
  , Pair Fmin_d $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Computes the smaller of the double-precision floats in registers f[rs1] and f[rs2]."
      comment "Copies the result to f[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs
      let f_rs1 = extractE 0 (readFPR rs1)
      let f_rs2 = extractE 0 (readFPR rs2)
      let (cmp, _) = getFRes $ f64LeE f_rs1 f_rs2 -- ignore flags

      let res = cases
            [ (isNaN64 f_rs1 `andE` isNaN64 f_rs2, canonicalNaN64)
            , (isNaN64 f_rs1, f_rs2)
            , (isNaN64 f_rs2, f_rs1)
            , ((f_rs1 `eqE` negZero64) `andE` (f_rs2 `eqE` posZero64), f_rs1)
            , ((f_rs1 `eqE` posZero64) `andE` (f_rs2 `eqE` negZero64), f_rs2)
            ]
            $ iteE cmp f_rs1 f_rs2
      let invalid = isSNaN64 f_rs1 `orE` isSNaN64 f_rs2
      let flags = iteE invalid (litBV 0x10) (litBV 0x00)

      assignFPR rd (zextE res)
      raiseFPExceptions flags
      incrPC
  , Pair Fmax_d $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Computes the larger of the double-precision floats in registers f[rs1] and f[rs2]."
      comment "Copies the result to f[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs
      let f_rs1 = extractE 0 (readFPR rs1)
      let f_rs2 = extractE 0 (readFPR rs2)
      let (cmp, _) = getFRes $ f64LeE f_rs1 f_rs2 -- ignore flags

      let res = cases
            [ (isNaN64 f_rs1 `andE` isNaN64 f_rs2, canonicalNaN64)
            , (isNaN64 f_rs1, f_rs2)
            , (isNaN64 f_rs2, f_rs1)
            , ((f_rs1 `eqE` negZero64) `andE` (f_rs2 `eqE` posZero64), f_rs2)
            , ((f_rs1 `eqE` posZero64) `andE` (f_rs2 `eqE` negZero64), f_rs1)
            ]
            $ iteE cmp f_rs2 f_rs1
      let invalid = isSNaN64 f_rs1 `orE` isSNaN64 f_rs2
      let flags = iteE invalid (litBV 0x10) (litBV 0x00)

      assignFPR rd (zextE res)
      raiseFPExceptions flags
      incrPC
  , Pair Fcvt_s_d $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the double-precision float in f[rs1] to a single-precision float."
      comment "Writes the result to f[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let f_rs1 = readFPR rs1
        let (res', flags) = getFResCanonical32 $ f64ToF32E rm (extractE 0 f_rs1)
        res <- nanBox32 res'

        assignFPR rd res
        raiseFPExceptions flags
        incrPC
  , Pair Fcvt_d_s $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the single-precision float in f[rs1] to a double-precision float."
      comment "Writes the result to f[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let f_rs1 = readFPR rs1
            (res, flags) = getFResCanonical64 $ f32ToF64E rm (extractE 0 f_rs1)

        assignFPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Feq_d $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Writes 1 to x[rd] if the double-precision float in f[rs1] equals f[rs2]."
      comment "Writes 0 to x[rd] if not."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let f_rs1 = extractE 0 (readFPR rs1)
      let f_rs2 = extractE 0 (readFPR rs2)
      let (res, flags) = getFRes $ f64EqE f_rs1 f_rs2

      assignGPR rd (zextE res)
      raiseFPExceptions flags
      incrPC
  , Pair Flt_d $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Writes 1 to x[rd] if the double-precision float in f[rs1] is less than f[rs2]."
      comment "Writes 0 to x[rd] if not."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let f_rs1 = extractE 0 (readFPR rs1)
      let f_rs2 = extractE 0 (readFPR rs2)
      let (res, flags) = getFRes $ f64LtE f_rs1 f_rs2

      assignGPR rd (zextE res)
      raiseFPExceptions flags
      incrPC
  , Pair Fle_d $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Writes 1 to x[rd] if the double-precision float in f[rs1] is less than or equal to f[rs2]."
      comment "Writes 0 to x[rd] if not."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let f_rs1 = extractE 0 (readFPR rs1)
      let f_rs2 = extractE 0 (readFPR rs2)
      let (res, flags) = getFRes $ f64LeE f_rs1 f_rs2

      assignGPR rd (zextE res)
      raiseFPExceptions flags
      incrPC
  , Pair Fclass_d $ instSemantics (Rd :< Rs1 :< Nil) $ do
      comment "Writes to x[rd] a mask indicating the class of the double-precision float in f[rs1]."
      comment "Exactly one bit in x[rd] is set. Table not included in this comment."

      rd :< rs1 :< Nil <- operandEs
      let f_rs1 = extractE 0 (readFPR rs1)
      let res = cases
            [ (f_rs1 `eqE` negInfinity64, litBV 0x1)
            , (f64Sgn f_rs1 `andE` isNormal64 f_rs1, litBV 0x2)
            , (f64Sgn f_rs1 `andE` isSubnormal64 f_rs1, litBV 0x4)
            , (f_rs1 `eqE` negZero64, litBV 0x8)
            , (f_rs1 `eqE` posZero64, litBV 0x10)
            , (notE (f64Sgn f_rs1) `andE` isSubnormal64 f_rs1, litBV 0x20)
            , (notE (f64Sgn f_rs1) `andE` isNormal64 f_rs1, litBV 0x40)
            , (f_rs1 `eqE` posInfinity64, litBV 0x80)
            , (isSNaN64 f_rs1, litBV 0x100)
            , (isQNaN64 f_rs1, litBV 0x200)
            ]
            (litBV 0x0)

      assignGPR rd res
      incrPC
  , Pair Fcvt_w_d $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the double-precision float in f[rs1] to a 32-bit signed integer."
      comment "Writes the result to x[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let f_rs1 = readFPR rs1
            (res, flags) = getFRes $ f64ToI32E rm (extractE 0 f_rs1)

        assignGPR rd (sextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fcvt_wu_d $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the double-precision float in f[rs1] to a 32-bit unsigned integer."
      comment "Writes the result to x[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let f_rs1 = readFPR rs1
            (res, flags) = getFRes $ f64ToUi32E rm (extractE 0 f_rs1)

        assignGPR rd (sextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fcvt_d_w $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the 32-bit signed integer in x[rs1] to a double-precision float."
      comment "Writes the result to f[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let x_rs1 = readGPR rs1
            (res, flags) = getFResCanonical64 $ i32ToF64E rm (extractE 0 x_rs1)

        assignFPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fcvt_d_wu $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the 32-bit unsigned integer in x[rs1] to a double-precision float."
      comment "Writes the result to f[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let x_rs1 = readGPR rs1
            (res, flags) = getFResCanonical64 $ ui32ToF64E rm (extractE 0 x_rs1)

        assignFPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  ]

d64Encode :: (64 <= RVWidth rv, DExt << rv) => EncodeMap rv
d64Encode = Map.fromList
  [ Pair Fcvt_l_d  (OpBits R2Repr (0b1010011 :< 0b110000100010 :< Nil))
  , Pair Fcvt_lu_d (OpBits R2Repr (0b1010011 :< 0b110000100011 :< Nil))
  , Pair Fmv_x_d   (OpBits RXRepr (0b1010011 :< 0b000 :< 0b111000100000 :< Nil))
  , Pair Fcvt_d_l  (OpBits R2Repr (0b1010011 :< 0b110100100010 :< Nil))
  , Pair Fcvt_d_lu (OpBits R2Repr (0b1010011 :< 0b110100100011 :< Nil))
  , Pair Fmv_d_x   (OpBits RXRepr (0b1010011 :< 0b000 :< 0b111100100000 :< Nil))
  ]

d64Semantics :: (KnownRVWidth rv, KnownRVFloatWidth rv, KnownRVFloatType rv, FExt << rv, DExt << rv, 64 <= RVWidth rv) => SemanticsMap rv
d64Semantics = Map.fromList
  [ Pair Fcvt_l_d $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the double-precision float in f[rs1] to a 64-bit signed integer."
      comment "Writes the result to x[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let f_rs1 = readFPR rs1
            (res, flags) = getFRes $ f64ToI64E rm (extractE 0 f_rs1)

        assignGPR rd (sextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fcvt_lu_d $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the double-precision float in f[rs1] to a 64-bit unsigned integer."
      comment "Writes the result to x[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let f_rs1 = readFPR rs1
            (res, flags) = getFRes $ f64ToUi64E rm (extractE 0 f_rs1)

        assignGPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fmv_x_d $ instSemantics (Rd :< Rs1 :< Nil) $ do
      comment "Copies the double-precision float in register f[rs1] to x[rd]."
      comment "Sign-extends the result."

      rd :< rs1 :< Nil <- operandEs

      let f_rs1 = readFPR rs1

      assignGPR rd (sextE (extractE' (knownNat @64) 0 f_rs1))
      incrPC
  , Pair Fcvt_d_l $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the 64-bit signed integer in x[rs1] to a double-precision float."
      comment "Writes the result to f[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let x_rs1 = readGPR rs1
            (res, flags) = getFResCanonical64 $ i64ToF64E rm (extractE 0 x_rs1)

        assignFPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fcvt_d_lu $ instSemantics (Rd :< Rm :< Rs1 :< Nil) $ do
      comment "Converts the 64-bit unsigned integer in x[rs1] to a double-precision float."
      comment "Writes the result to f[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let x_rs1 = readGPR rs1
            (res, flags) = getFResCanonical64 $ ui64ToF64E rm (extractE 0 x_rs1)

        assignFPR rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fmv_d_x $ instSemantics (Rd :< Rs1 :< Nil) $ do
      comment "Copies the double-precision float in register x[rs1] to f[rd]."
      comment "Sign-extends the result."

      rd :< rs1 :< Nil <- operandEs

      let x_rs1 = readGPR rs1

      assignFPR rd (sextE (extractE' (knownNat @64) 0 x_rs1))
      incrPC
  ]
