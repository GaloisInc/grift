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

import RISCV.InstructionSet.Helpers
import RISCV.InstructionSet
import RISCV.Semantics
import RISCV.Types

-- | RV32F
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
      incrPC
  , Pair Fsw $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fmadd_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fmsub_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fnmsub_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fnmadd_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fadd_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fsub_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fmul_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fdiv_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fsqrt_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fsgnj_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fsgnjn_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fsgnjx_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fmin_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fmax_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fcvt_w_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fcvt_wu_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fmv_x_w $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Feq_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Flt_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fle_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fclass_s $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fcvt_s_w $ InstSemantics $ getSemantics $ do
      incrPC
  , Pair Fcvt_s_wu $ InstSemantics $ getSemantics $ do
      -- TODO:
      -- FCSR
      -- Dynamic rounding
      -- Exception flags
      -- NaN handling (any NaN should be canonical)
      comment "Converts the 32-bit unsigned integer in x[rs1] to a single-precision float."
      comment "Writes the result to f[rd]."

      rd :< rm :< rs1 :< Nil <- operandEs
      let x_rs1 = readReg rs1
          (res, flags) = getFRes $ ui32ToF32E rm (extractE 0 x_rs1)

      assignReg rd (zextE res)
      incrPC
  , Pair Fmv_w_x $ InstSemantics $ getSemantics $ do
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
