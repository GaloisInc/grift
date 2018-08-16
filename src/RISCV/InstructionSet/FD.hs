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
Module      : RISCV.InstructionSet.FD
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

F and D extensions for RV32 and RV64.
-}

module RISCV.InstructionSet.FD
  ( f32
  -- , f64
  -- , d32
  -- , d64
  ) where

import Data.BitVector.Sized.App
import Data.BitVector.Sized.Float.App
import qualified Data.Parameterized.Map as Map
import Data.Parameterized
import Data.Parameterized.List

import RISCV.InstructionSet
import RISCV.InstructionSet.Utils
import RISCV.Semantics
import RISCV.Types

-- | F extension (RV32)
f32 :: (KnownRV rv, FExt << rv) => InstructionSet rv
f32 = instructionSet fEncode fSemantics

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

fSemantics :: (KnownRV rv, FExt << rv) => SemanticsMap rv
fSemantics = Map.fromList
  [ Pair Flw $ InstSemantics $ getSemantics $ do
      comment "Loads a single-precision float from memory address x[rs1] + sext(offset)."
      comment "Writes the result to f[rd]."

      rd :< rs1 :< offset :< Nil <- operandEs

      let x_rs1 = readReg rs1
      let mVal  = readMem (knownNat @4) (x_rs1 `addE` sextE offset)

      assignFReg rd (zextE mVal)
      incrPC
  , Pair Fsw $ InstSemantics $ getSemantics $ do
      comment "Stores the single-precision float in register f[rs2] to memory at address x[rs1] + sext(offset)."

      rs1 :< rs2 :< offset :< Nil <- operandEs

      let x_rs1 = readReg rs1
      let f_rs2 = readFReg rs2

      assignMem (knownNat @4) (x_rs1 `addE` sextE offset) (extractE 0 f_rs2)
      incrPC
  , Pair Fmadd_s $ InstSemantics $ getSemantics $ do
      comment "Multiplies the single-precision floats in f[rs1] and f[rs2]."
      comment "Adds the unrounded product to the single precision float in f[rs3]."
      comment "Writes the rounded single-precision result to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< rs3 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFReg rs1)
        let f_rs2 = extractE 0 (readFReg rs2)
        let f_rs3 = extractE 0 (readFReg rs3)
        let (res, flags) = getFRes32 $ f32MulAddE rm f_rs1 f_rs2 f_rs3

        assignFReg rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fmsub_s $ InstSemantics $ getSemantics $ do
      comment "Multiplies the single-precision floats in f[rs1] and f[rs2]."
      comment "Subtracts the single precision float in f[rs3] from the unrounded product."
      comment "Writes the rounded single-precision result to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< rs3 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFReg rs1)
        let f_rs2 = extractE 0 (readFReg rs2)
        let f_rs3 = extractE 0 (readFReg rs3)
        let (res, flags) = getFRes32 $ f32MulAddE rm f_rs1 f_rs2 (negate32 f_rs3)

        assignFReg rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fnmsub_s $ InstSemantics $ getSemantics $ do
      comment "Multiplies the single-precision floats in f[rs1] and f[rs2], negating the result."
      comment "Subtracts the single precision float in f[rs3] from the unrounded product."
      comment "Writes the rounded single-precision result to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< rs3 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFReg rs1)
        let f_rs2 = extractE 0 (readFReg rs2)
        let f_rs3 = extractE 0 (readFReg rs3)
        let (res, flags) = getFRes32 $ f32MulAddE rm (negate32 f_rs1) f_rs2 (negate32 f_rs3)

        assignFReg rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fnmadd_s $ InstSemantics $ getSemantics $ do
      comment "Multiplies the single-precision floats in f[rs1] and f[rs2], negating the result."
      comment "Adds the single precision float in f[rs3] to the unrounded product."
      comment "Writes the rounded single-precision result to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< rs3 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFReg rs1)
        let f_rs2 = extractE 0 (readFReg rs2)
        let f_rs3 = extractE 0 (readFReg rs3)
        let (res, flags) = getFRes32 $ f32MulAddE rm (negate32 f_rs1) f_rs2 f_rs3

        assignFReg rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fadd_s $ InstSemantics $ getSemantics $ do
      comment "Adds the single-precision float in registers f[rs1] and f[rs2]."
      comment "Writes the rounded single-precision sum to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFReg rs1)
        let f_rs2 = extractE 0 (readFReg rs2)
        let (res, flags) = getFRes32 $ f32AddE rm f_rs1 f_rs2

        assignFReg rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fsub_s $ InstSemantics $ getSemantics $ do
      comment "Subtracts the single-precision float in register f[rs2] from f[rs1]."
      comment "Writes the rounded single-precision difference to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFReg rs1)
        let f_rs2 = extractE 0 (readFReg rs2)
        let (res, flags) = getFRes32 $ f32SubE rm f_rs1 f_rs2

        assignFReg rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fmul_s $ InstSemantics $ getSemantics $ do
      comment "Multiplies the single-precision float in registers f[rs1] and f[rs2]."
      comment "Writes the rounded single-precision sum to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFReg rs1)
        let f_rs2 = extractE 0 (readFReg rs2)
        let (res, flags) = getFRes32 $ f32MulE rm f_rs1 f_rs2

        assignFReg rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fdiv_s $ InstSemantics $ getSemantics $ do
      comment "Divides the single-precision float in register f[rs1] by f[rs2]."
      comment "Writes the rounded single-precision difference to f[rd]."

      rd :< rm' :< rs1 :< rs2 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFReg rs1)
        let f_rs2 = extractE 0 (readFReg rs2)
        let (res, flags) = getFRes32 $ f32DivE rm f_rs1 f_rs2

        assignFReg rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fsqrt_s $ InstSemantics $ getSemantics $ do
      comment "Computes the square root of the single-precision float in register f[rs1]."
      comment "Writes the rounded single-precision result to f[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs

      withRM rm' $ \rm -> do
        let f_rs1 = extractE 0 (readFReg rs1)
        let (res, flags) = getFRes32 $ f32SqrtE rm f_rs1

        assignFReg rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fsgnj_s $ InstSemantics $ getSemantics $ do
      comment "Constructs a new single-precision float from the significant and exponent of f[rs1]."
      comment "Uses the sign of f[rs2], and writes the result to f[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs
      let f_rs1 = readFReg rs1
      let f_rs2 = readFReg rs2

      let res_sign = f32Sgn (extractE 0 f_rs2)
      let res_rst  = extractEWithRepr (knownNat @31) 0 f_rs1
      let res = zextE (res_sign `concatE` res_rst)

      assignFReg rd res
      incrPC
  , Pair Fsgnjn_s $ InstSemantics $ getSemantics $ do
      comment "Constructs a new single-precision float from the significant and exponent of f[rs1]."
      comment "Uses the opposite sign of f[rs2], and writes the result to f[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs
      let f_rs1 = readFReg rs1
      let f_rs2 = readFReg rs2

      let res_sign = notE (f32Sgn (extractE 0 f_rs2))
      let res_rst  = extractEWithRepr (knownNat @31) 0 f_rs1
      let res = zextE (res_sign `concatE` res_rst)

      assignFReg rd res
      incrPC
  , Pair Fsgnjx_s $ InstSemantics $ getSemantics $ do
      comment "Constructs a new single-precision float from the significant and exponent of f[rs1]."
      comment "Uses the xor of the signs of f[rs1] and f[rs2], and writes the result to f[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs
      let f_rs1 = readFReg rs1
      let f_rs2 = readFReg rs2

      let res_sign = (f32Sgn (extractE 0 f_rs1) `xorE` f32Sgn (extractE 0 f_rs2))
      let res_rst  = extractEWithRepr (knownNat @31) 0 f_rs1
      let res = zextE (notE res_sign `concatE` res_rst)

      assignFReg rd res
      incrPC
  , Pair Fmin_s $ InstSemantics $ getSemantics $ do
      comment "Computes the smaller of the single-precision floats in registers f[rs1] and f[rs2]."
      comment "Copies the result to f[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs
      let f_rs1 = extractE 0 (readFReg rs1)
      let f_rs2 = extractE 0 (readFReg rs2)
      let (cmp, flags) = getFRes $ f32LeE f_rs1 f_rs2

      let res = iteE cmp f_rs1 f_rs2

      assignReg rd (zextE res)
      raiseFPExceptions flags
      incrPC
  , Pair Fmax_s $ InstSemantics $ getSemantics $ do
      comment "Computes the larger of the single-precision floats in registers f[rs1] and f[rs2]."
      comment "Copies the result to f[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs
      let f_rs1 = extractE 0 (readFReg rs1)
      let f_rs2 = extractE 0 (readFReg rs2)
      let (cmp, flags) = getFRes $ f32LeE f_rs1 f_rs2

      let res = iteE cmp f_rs2 f_rs1

      assignReg rd (zextE res)
      raiseFPExceptions flags
      incrPC
  , Pair Fcvt_w_s $ InstSemantics $ getSemantics $ do
      comment "Converts the single-precision float in f[rs1] to a 32-bit signed integer."
      comment "Writes the result to x[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let f_rs1 = readFReg rs1
            (res, flags) = getFRes32 $ f32ToI32E rm (extractE 0 f_rs1)

        assignReg rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fcvt_wu_s $ InstSemantics $ getSemantics $ do
      comment "Converts the single-precision float in f[rs1] to a 32-bit unsigned integer."
      comment "Writes the result to x[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let f_rs1 = readFReg rs1
            (res, flags) = getFRes32 $ f32ToUi32E rm (extractE 0 f_rs1)

        assignReg rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fmv_x_w $ InstSemantics $ getSemantics $ do
      comment "Copies the single-precision float in register f[rs1] to x[rd]."
      comment "Sign-extends the result."

      rd :< rs1 :< Nil <- operandEs

      let f_rs1 = readFReg rs1

      assignReg rd (sextE (extractEWithRepr (knownNat @32) 0 f_rs1))
      incrPC
  , Pair Feq_s $ InstSemantics $ getSemantics $ do
      comment "Writes 1 to x[rd] if the single-precision float in f[rs1] equals f[rs2]."
      comment "Writes 0 to x[rd] if not."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let f_rs1 = extractE 0 (readFReg rs1)
      let f_rs2 = extractE 0 (readFReg rs2)
      let (res, flags) = getFRes $ f32EqE f_rs1 f_rs2

      assignReg rd (zextE res)
      raiseFPExceptions flags
      incrPC
  , Pair Flt_s $ InstSemantics $ getSemantics $ do
      comment "Writes 1 to x[rd] if the single-precision float in f[rs1] is less than f[rs2]."
      comment "Writes 0 to x[rd] if not."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let f_rs1 = extractE 0 (readFReg rs1)
      let f_rs2 = extractE 0 (readFReg rs2)
      let (res, flags) = getFRes $ f32LtE f_rs1 f_rs2

      assignReg rd (zextE res)
      raiseFPExceptions flags
      incrPC
  , Pair Fle_s $ InstSemantics $ getSemantics $ do
      comment "Writes 1 to x[rd] if the single-precision float in f[rs1] is less than or equal to f[rs2]."
      comment "Writes 0 to x[rd] if not."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let f_rs1 = extractE 0 (readFReg rs1)
      let f_rs2 = extractE 0 (readFReg rs2)
      let (res, flags) = getFRes $ f32LeE f_rs1 f_rs2

      assignReg rd (zextE res)
      raiseFPExceptions flags
      incrPC
  , Pair Fclass_s $ InstSemantics $ getSemantics $ do
      comment "Writes to x[rd] a mask indicating the class of the single-precision float in f[rs1]."
      comment "Exactly one bit in x[rd] is set. Table not included in this comment."

      rd :< rs1 :< Nil <- operandEs
      let f_rs1 = extractE 0 (readFReg rs1)
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

      assignReg rd res
      incrPC
  , Pair Fcvt_s_w $ InstSemantics $ getSemantics $ do
      comment "Converts the 32-bit signed integer in x[rs1] to a single-precision float."
      comment "Writes the result to f[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let x_rs1 = readReg rs1
            (res, flags) = getFRes32 $ i32ToF32E rm (extractE 0 x_rs1)

        assignFReg rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fcvt_s_wu $ InstSemantics $ getSemantics $ do
      comment "Converts the 32-bit unsigned integer in x[rs1] to a single-precision float."
      comment "Writes the result to f[rd]."

      rd :< rm' :< rs1 :< Nil <- operandEs
      withRM rm' $ \rm -> do
        let x_rs1 = readReg rs1
            (res, flags) = getFRes32 $ ui32ToF32E rm (extractE 0 x_rs1)

        assignFReg rd (zextE res)
        raiseFPExceptions flags
        incrPC
  , Pair Fmv_w_x $ InstSemantics $ getSemantics $ do
      comment "Copies the single-precision float in register x[rs1] to f[rd]."
      comment "Sign-extends the result."

      rd :< rs1 :< Nil <- operandEs

      let x_rs1 = readReg rs1

      assignFReg rd (sextE (extractEWithRepr (knownNat @32) 0 x_rs1))
      incrPC
  ]


  -- -- RV64F
  -- Fcvt_l_s  :: (64 <= RVWidth rv, FExt << rv) => Opcode rv R2
  -- Fcvt_lu_s :: (64 <= RVWidth rv, FExt << rv) => Opcode rv R2
  -- Fcvt_s_l  :: (64 <= RVWidth rv, FExt << rv) => Opcode rv R2
  -- Fcvt_s_lu :: (64 <= RVWidth rv, FExt << rv) => Opcode rv R2

  -- -- RV32D
  -- Fld       :: DExt << rv => Opcode rv I
  -- Fsd       :: DExt << rv => Opcode rv S
  -- Fmadd_d   :: DExt << rv => Opcode rv R4
  -- Fmsub_d   :: DExt << rv => Opcode rv R4
  -- Fnmsub_d  :: DExt << rv => Opcode rv R4
  -- Fnmadd_d  :: DExt << rv => Opcode rv R4
  -- Fadd_d    :: DExt << rv => Opcode rv R3
  -- Fsub_d    :: DExt << rv => Opcode rv R3
  -- Fmul_d    :: DExt << rv => Opcode rv R3
  -- Fdiv_d    :: DExt << rv => Opcode rv R3
  -- Fsqrt_d   :: DExt << rv => Opcode rv R2
  -- Fsgnj_d   :: DExt << rv => Opcode rv R
  -- Fsgnjn_d  :: DExt << rv => Opcode rv R
  -- Fsgnjx_d  :: DExt << rv => Opcode rv R
  -- Fmin_d    :: DExt << rv => Opcode rv R
  -- Fmax_d    :: DExt << rv => Opcode rv R
  -- Fcvt_s_d  :: DExt << rv => Opcode rv R2
  -- Fcvt_d_s  :: DExt << rv => Opcode rv R2
  -- Feq_d     :: DExt << rv => Opcode rv R
  -- Flt_d     :: DExt << rv => Opcode rv R
  -- Fle_d     :: DExt << rv => Opcode rv R
  -- Fclass_d  :: DExt << rv => Opcode rv RX
  -- Fcvt_w_d  :: DExt << rv => Opcode rv R2
  -- Fcvt_wu_d :: DExt << rv => Opcode rv R2
  -- Fcvt_d_w  :: DExt << rv => Opcode rv R2
  -- Fcvt_d_wu :: DExt << rv => Opcode rv R2

  -- -- RV64D
  -- Fcvt_l_d  :: (64 <= RVWidth rv, DExt << rv) => Opcode rv R2
  -- Fcvt_lu_d :: (64 <= RVWidth rv, DExt << rv) => Opcode rv R2
  -- Fmv_x_d   :: (64 <= RVWidth rv, DExt << rv) => Opcode rv RX
  -- Fcvt_d_l  :: (64 <= RVWidth rv, DExt << rv) => Opcode rv R2
  -- Fcvt_d_lu :: (64 <= RVWidth rv, DExt << rv) => Opcode rv R2
  -- Fmv_d_x   :: (64 <= RVWidth rv, DExt << rv) => Opcode rv RX
