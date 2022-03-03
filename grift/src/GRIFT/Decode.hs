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
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : GRIFT.Decode
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module defines a function 'decode' that converts a 'BitVector' @32@ an internal
'Instruction', as well as the corresponding reverse 'encode' function.
-}

module GRIFT.Decode
  ( -- * Functions
    decode
  , decodeC
  , encode
  , opBitsLayouts, OpBitsLayout
  , operandsLayouts, OperandsLayout
  ) where

import Control.Lens hiding ( (:<), Index, op, iset )
import Data.BitVector.Sized ( concat )
import Data.Parameterized ( type (<=), type (+), Some(..), knownNat )
import Data.Parameterized.List ( List(..) )
import Prelude hiding ( concat )

import GRIFT.BitVector.BitLayout
import GRIFT.InstructionSet
    ( opBitsFromOpcode, opcodeFromOpBits, InstructionSet )
import GRIFT.Types
import GHC.TypeNats ( KnownNat )
import GRIFT.BitVector.BVApp ( sextOrId, zextOrId )

type OpBitsLayout fmt = List (BitLayout 32) (OpBitsTypes fmt)
type OperandsLayout fmt = List (BitLayout 32) (OperandTypes fmt)

opcode :: BitLayout 32 7
opcode = chunk @7 @0 <: empty

funct3 :: BitLayout 32 3
funct3 = singleChunk @12

funct7 :: BitLayout 32 7
funct7 = singleChunk @25

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
        funct5 = singleChunk @27
        funct32 :: BitLayout 32 32
        funct32 = singleChunk @0
        funct2 :: BitLayout 32 2
        funct2 = singleChunk @25
        funct12 :: BitLayout 32 12
        funct12 = singleChunk @20

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

  where rdLayout     :: BitLayout 32 5  = singleChunk @7
        rs1Layout    :: BitLayout 32 5 = singleChunk @15
        rs2Layout    :: BitLayout 32 5 = singleChunk @20
        rs3Layout    :: BitLayout 32 5 = singleChunk @27
        rmLayout     :: BitLayout 32 3 = singleChunk @12 -- same as funct3
        shamtLayout  :: BitLayout 32 7 = singleChunk @20
        rlLayout     :: BitLayout 32 1 = singleChunk @25
        aqLayout     :: BitLayout 32 1 = singleChunk @26
        imm12ILayout :: BitLayout 32 12 = singleChunk @20
        imm12SLayout :: BitLayout 32 12 = chunk @7 @25 <: chunk @5 @7 <: empty
        imm12BLayout :: BitLayout 32 12 =
          chunk @1 @31 <: chunk @1 @7 <:
          chunk @6 @25 <: chunk @4 @8 <:
          empty
        imm20ULayout :: BitLayout 32 20 = singleChunk @12
        imm20JLayout :: BitLayout 32 20 =
          chunk @1 @31 <: chunk @8 @12 <:
          chunk @1 @20 <: chunk @10 @21 <:
          empty
        illegalLayout :: BitLayout 32 32 = singleChunk @0

-- | Get the format of an instruction word.
getFormat :: SizedBV 32 -> Some FormatRepr
getFormat bv = case bv ^. layoutLens opcode of
  0b0110011 -> Some RRepr
  0b0111011 -> Some RRepr
  0b1010011 -> case bv ^. layoutLens funct7 of
    0b0000000 -> Some R3Repr
    0b0000100 -> Some R3Repr
    0b0001000 -> Some R3Repr
    0b0001100 -> Some R3Repr
    0b0000001 -> Some R3Repr
    0b0000101 -> Some R3Repr
    0b0001001 -> Some R3Repr
    0b0001101 -> Some R3Repr
    0b0101100 -> Some R2Repr
    0b0101101 -> Some R2Repr
    0b1100000 -> Some R2Repr
    0b1100001 -> Some R2Repr
    0b1101000 -> Some R2Repr
    0b1101001 -> Some R2Repr
    0b0100000 -> Some R2Repr
    0b0100001 -> Some R2Repr
    0b1110000 -> Some RXRepr
    0b1110001 -> Some RXRepr
    0b1111000 -> Some RXRepr
    0b1111001 -> Some RXRepr
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
       -> SizedBV 32
       -> Some (Instruction rv)
decode iset bv = case getFormat bv of
  Some repr -> case decodeOpcode iset repr bv of
    Right op     -> Some $ Inst op (decodeOperands repr bv)
    Left Illegal -> Some $ Inst Illegal (decodeOperands XRepr bv)

-- | From the format, get the operands
decodeOperands :: FormatRepr fmt -> SizedBV 32 -> Operands fmt
decodeOperands repr bv =
  Operands repr (bv ^. layoutsLens (operandsLayouts repr))

-- | From the format, get the opbits
decodeOpBits :: FormatRepr fmt -> SizedBV 32 -> OpBits fmt
decodeOpBits repr bv = OpBits repr (bv ^. layoutsLens (opBitsLayouts repr))

decodeOpcode :: InstructionSet rv
             -> FormatRepr fmt
             -> SizedBV 32
             -> Either (Opcode rv X) (Opcode rv fmt)
decodeOpcode iset repr bv = opcodeFromOpBits iset (decodeOpBits repr bv)

-- | Encode an 'Instruction' as a 32-bit instruction word.
encode :: InstructionSet rv -> Instruction rv fmt -> SizedBV 32
encode iset (Inst opc (Operands repr operands)) =
  sizedBVInteger 0 & (opBitsLens .~ opBits) & (operandsLens .~ operands)
  where opBitsLens   = layoutsLens (opBitsLayouts repr)
        operandsLens = layoutsLens (operandsLayouts repr)
        OpBits _ opBits = opBitsFromOpcode iset opc

-- | 'concat' over 'SizedBV's
sconcat :: forall l r.
  KnownNat l =>
  KnownNat r =>
  KnownNat (l + r) =>
  SizedBV l -> SizedBV r -> SizedBV (l + r)
sconcat (SizedBV _ l) (SizedBV _ r) =
  SizedBV knownNat (concat knownNat knownNat l r)

-- | 'zextOrId' over 'SizedBV's.
szext :: forall i o.
  KnownNat i =>
  KnownNat o =>
  i <= o =>
  SizedBV i -> SizedBV o
szext (SizedBV _ bv) = SizedBV (knownNat @o) (zextOrId bv)

-- | 'sextOrId' over 'SizedBV's.
ssext :: forall i o.
  KnownNat i =>
  KnownNat o =>
  i <= o =>
  1 <= i =>
  SizedBV i -> SizedBV o
ssext (SizedBV _ bv) = SizedBV (knownNat @o) (sextOrId bv)

-- | Attempt to decode a compressed 16-bit instruction word. This is only for the C
-- extension.
decodeC :: CExt << rv => RVRepr rv -> SizedBV 16 -> Maybe (Some (Instruction rv))
decodeC rv bv =
  case bv ^. layoutLens slice0_1 of
    0b00 -> case bv ^. layoutLens slice13_15 of
      0b000 -> case bv ^. layoutLens addi4spn_imm of
        0 -> js illegal
        imm' -> js $
          let rd = szext (bv ^. layoutLens slice2_4) + 0x8
              rs1 = 0b00010
              imm = szext $ sconcat imm' (0 :: SizedBV 2)
          in Inst Addi (Operands IRepr (rd :< rs1 :< imm :< Nil))
      0b001 -> case rv of
        RVRepr _ (ExtensionsRepr _ _ _ FDYesRepr _) -> js $
          let rd   = szext (bv ^. layoutLens slice2_4) + 0x8
              rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
              imm' = bv ^. layoutLens q0imm
              offset  = szext $ sconcat imm' (0 :: SizedBV 3)
          in Inst Fld (Operands IRepr (rd :< rs1 :< offset :< Nil))
        _ -> js illegal
      0b010 -> js $
        let rd   = szext (bv ^. layoutLens slice2_4) + 0x8
            rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
            imm' = bv ^. layoutLens q0imm2
            offset = szext $ sconcat imm' (0 :: SizedBV 2)
        in Inst Lw (Operands IRepr (rd :< rs1 :< offset :< Nil))
      0b011 -> case rv of
        RVRepr RV32Repr (ExtensionsRepr _ _ _ FDYesRepr _) -> js $
          let rd  = szext (bv ^. layoutLens slice2_4) + 0x8
              rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
              imm' = bv ^. layoutLens q0imm2
              offset = szext $ sconcat imm' (0 :: SizedBV 2)
          in Inst Flw (Operands IRepr (rd :< rs1 :< offset :< Nil))
        RVRepr RV32Repr (ExtensionsRepr _ _ _ FYesDNoRepr _) -> js $
          let rd  = szext (bv ^. layoutLens slice2_4) + 0x8
              rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
              imm' = bv ^. layoutLens q0imm2
              offset = szext $ sconcat imm' (0 :: SizedBV 2)
          in Inst Flw (Operands IRepr (rd :< rs1 :< offset :< Nil))
        RVRepr RV64Repr _ -> js $
          let rd  = szext (bv ^. layoutLens slice2_4) + 0x8
              rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
              imm' = bv ^. layoutLens q0imm
              offset = szext $ sconcat imm' (0 :: SizedBV 3)
          in Inst Ld (Operands IRepr (rd :< rs1 :< offset :< Nil))
        _ -> js illegal
      0b100 -> js illegal
      0b101 -> case rv of
        RVRepr _ (ExtensionsRepr _ _ _ FDYesRepr _) -> js $
          let rs2  = szext (bv ^. layoutLens slice2_4) + 0x8
              rs1  = szext (bv ^. layoutLens slice7_9) + 0x8
              imm' = bv ^. layoutLens q0imm
              offset = szext $ sconcat imm' (0 :: SizedBV 3)
          in Inst Fsd (Operands SRepr (rs1 :< rs2 :< offset :< Nil))
        _ -> js illegal
      0b110 -> js $
        let rs2  = szext (bv ^. layoutLens slice2_4) + 0x8
            rs1  = szext (bv ^. layoutLens slice7_9) + 0x8
            imm' = bv ^. layoutLens q0imm2
            offset = szext $ sconcat imm' (0 :: SizedBV 2)
        in Inst Sw (Operands SRepr (rs1 :< rs2 :< offset :< Nil))
      0b111 -> case rv of
        RVRepr RV32Repr (ExtensionsRepr _ _ _ FDYesRepr _) -> js $
          let rs2  = szext (bv ^. layoutLens slice2_4) + 0x8
              rs1  = szext (bv ^. layoutLens slice7_9) + 0x8
              imm' = bv ^. layoutLens q0imm2
              offset = szext $ sconcat imm' (0 :: SizedBV 2)
          in Inst Fsw (Operands SRepr (rs1 :< rs2 :< offset :< Nil))
        RVRepr RV64Repr _ -> js $
          let rs2  = szext (bv ^. layoutLens slice2_4) + 0x8
              rs1  = szext (bv ^. layoutLens slice7_9) + 0x8
              imm' = bv ^. layoutLens q0imm
              offset = szext $ sconcat imm' (0 :: SizedBV 3)
          in Inst Sd (Operands SRepr (rs1 :< rs2 :< offset :< Nil))
        _ -> js illegal
      _ -> js illegal
    0b01 -> case bv ^. layoutLens slice13_15 of
      0b000 -> js $
        let rd  = bv ^. layoutLens slice7_11
            rs1 = bv ^. layoutLens slice7_11
            imm = ssext (bv ^. layoutLens imm6)
        in Inst Addi (Operands IRepr (rd :< rs1 :< imm :< Nil))
      0b001 -> case rv of
        RVRepr RV32Repr _ -> js $
          let rd = 0b00001
              imm' = bv ^. layoutLens j_imm
              offset  = ssext imm' -- ???
          in Inst Jal (Operands JRepr (rd :< offset :< Nil))
        RVRepr RV64Repr _ -> js $
          let rd  = bv ^. layoutLens slice7_11
              rs1 = bv ^. layoutLens slice7_11
              imm = ssext (bv ^. layoutLens imm6)
          in Inst Addiw (Operands IRepr (rd :< rs1 :< imm :< Nil))
        _ -> js illegal
      0b010 -> js $
        let rd  = bv ^. layoutLens slice7_11
            rs1 = 0b00000
            imm = ssext (bv ^. layoutLens imm6)
        in Inst Addi (Operands IRepr (rd :< rs1 :< imm :< Nil))
      0b011 -> case bv ^. layoutLens slice7_11 of
        2 -> js $
          let rd   = 0b00010
              rs1  = 0b00010
              imm' = bv ^. layoutLens addi16sp_imm
              imm  = ssext $ sconcat imm' (0 :: SizedBV 4)
          in Inst Addi (Operands IRepr (rd :< rs1 :< imm :< Nil))
        rd -> js $
          let imm' = bv ^. layoutLens imm6
              imm  = ssext imm'
          in Inst Lui (Operands URepr (rd :< imm :< Nil))
      0b100 -> case bv ^. layoutLens slice10_11 of
        0b00 -> js $
          let rd  = szext (bv ^. layoutLens slice7_9) + 0x8
              rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
              imm = szext (bv ^. layoutLens imm6)
          in Inst Srli (Operands HRepr (rd :< rs1 :< imm :< Nil))
        0b01 -> js $
          let rd  = szext (bv ^. layoutLens slice7_9) + 0x8
              rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
              imm = szext (bv ^. layoutLens imm6)
          in Inst Srai (Operands HRepr (rd :< rs1 :< imm :< Nil))
        0b10 -> js $
          let rd  = szext (bv ^. layoutLens slice7_9) + 0x8
              rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
              imm = ssext (bv ^. layoutLens imm6)
          in Inst Andi (Operands IRepr (rd :< rs1 :< imm :< Nil))
        0b11 -> case bv ^. layoutLens arithCtrl of
          0b000 -> js $
            let rd  = szext (bv ^. layoutLens slice7_9) + 0x8
                rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
                rs2 = szext (bv ^. layoutLens slice2_4) + 0x8
            in Inst Sub (Operands RRepr (rd :< rs1 :< rs2 :< Nil))
          0b001 -> js $
            let rd  = szext (bv ^. layoutLens slice7_9) + 0x8
                rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
                rs2 = szext (bv ^. layoutLens slice2_4) + 0x8
            in Inst Xor (Operands RRepr (rd :< rs1 :< rs2 :< Nil))
          0b010 -> js $
            let rd  = szext (bv ^. layoutLens slice7_9) + 0x8
                rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
                rs2 = szext (bv ^. layoutLens slice2_4) + 0x8
            in Inst Or (Operands RRepr (rd :< rs1 :< rs2 :< Nil))
          0b011 -> js $
            let rd  = szext (bv ^. layoutLens slice7_9) + 0x8
                rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
                rs2 = szext (bv ^. layoutLens slice2_4) + 0x8
            in Inst And (Operands RRepr (rd :< rs1 :< rs2 :< Nil))
          0b100 -> case rv of
            RVRepr RV64Repr _ -> js $
              let rd  = szext (bv ^. layoutLens slice7_9) + 0x8
                  rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
                  rs2 = szext (bv ^. layoutLens slice2_4) + 0x8
              in Inst Subw (Operands RRepr (rd :< rs1 :< rs2 :< Nil))
            _ -> js illegal
          0b101 -> case rv of
            RVRepr RV64Repr _ -> js $
              let rd  = szext (bv ^. layoutLens slice7_9) + 0x8
                  rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
                  rs2 = szext (bv ^. layoutLens slice2_4) + 0x8
              in Inst Addw (Operands RRepr (rd :< rs1 :< rs2 :< Nil))
            _ -> js illegal
          _ -> js illegal
        _ -> js illegal
      0b101 -> js $
        let rd = 0b00000
            imm' = bv ^. layoutLens j_imm
            offset  = ssext imm'
        in Inst Jal (Operands JRepr (rd :< offset :< Nil))
      0b110 -> js $
        let rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
            rs2 = 0b00000
            imm' = bv ^. layoutLens b_imm
            offset = ssext imm'
        in Inst Beq (Operands BRepr (rs1 :< rs2 :< offset :< Nil))
      0b111 -> js $
        let rs1 = szext (bv ^. layoutLens slice7_9) + 0x8
            rs2 = 0b00000
            imm' = bv ^. layoutLens b_imm
            offset = ssext imm'
        in Inst Bne (Operands BRepr (rs1 :< rs2 :< offset :< Nil))
      _ -> js illegal
    0b10 -> case bv ^. layoutLens slice13_15 of
      0b000 -> case rv of
        RVRepr _ _ -> js $
          let rd   = bv ^. layoutLens slice7_11
              rs1  = bv ^. layoutLens slice7_11
              imm' = bv ^. layoutLens imm6
              imm = szext imm'
          in Inst Slli (Operands HRepr (rd :< rs1 :< imm :< Nil))
      0b001 -> case rv of
        RVRepr _ (ExtensionsRepr _ _ _ FDYesRepr _) -> js $
          -- C.FLDSP
          let rd     = bv ^. layoutLens slice7_11
              rs1    = 0b00010
              imm    = bv ^. layoutLens sp_imm_d
              offset = szext $ sconcat imm (0 :: SizedBV 3)
          in Inst Fld (Operands IRepr (rd :< rs1 :< offset :< Nil))
        _ -> js illegal
      0b010 -> js $
        -- C.LWSP
        let rd     = bv ^. layoutLens slice7_11
            rs1    = 0b00010
            imm    = bv ^. layoutLens sp_imm_w
            offset = szext $ sconcat imm (0 :: SizedBV 2)
        in Inst Lw (Operands IRepr (rd :< rs1 :< offset :< Nil))
      0b011 -> case rv of
        RVRepr RV32Repr (ExtensionsRepr _ _ _ FDYesRepr _) -> js $
          -- FLWSP
          let rd     = bv ^. layoutLens slice7_11
              rs1    = 0b00010
              imm    = bv ^. layoutLens sp_imm_w
              offset = szext $ sconcat imm (0 :: SizedBV 2)
          in Inst Flw (Operands IRepr (rd :< rs1 :< offset :< Nil))
        RVRepr RV32Repr (ExtensionsRepr _ _ _ FYesDNoRepr _) -> js $
          -- FLWSP
          let rd     = bv ^. layoutLens slice7_11
              rs1    = 0b00010
              imm    = bv ^. layoutLens sp_imm_w
              offset = szext $ sconcat imm (0 :: SizedBV 2)
          in Inst Flw (Operands IRepr (rd :< rs1 :< offset :< Nil))
        RVRepr RV64Repr _ -> js $
          -- LDSP
          let rd     = bv ^. layoutLens slice7_11
              rs1    = 0b00010
              imm    = bv ^. layoutLens sp_imm_d
              offset = szext $ sconcat imm (0 :: SizedBV 3)
          in Inst Ld (Operands IRepr (rd :< rs1 :< offset :< Nil))
        _ -> js illegal
      0b100 -> case bv ^. layoutLens slice2_6 of
        -- C.JR, C.MV, C.EBREAK, C.JALR, C.ADD
        0 -> case bv ^. layoutLens slice12_12 of
          0 -> js $
            -- C.JR
            let rd = 0b00000
                rs1 = bv ^. layoutLens slice7_11
                offset = 0
            in Inst Jalr (Operands IRepr (rd :< rs1 :< offset :< Nil))
          _ -> case bv ^. layoutLens slice7_11 of
            0 -> js $ Inst Ebreak (Operands PRepr Nil)
            rs1 -> js $
             let rd = 0b00001
                 offset = 0
             in Inst Jalr (Operands IRepr (rd :< rs1 :< offset :< Nil))
        rs2 -> case bv ^. layoutLens slice12_12 of
          0 -> js $
            let rd = bv ^. layoutLens slice7_11
                rs1 = 0b00000
            in Inst Add (Operands RRepr (rd :< rs1 :< rs2 :< Nil))
          1 -> js $
            let rd = bv ^. layoutLens slice7_11
                rs1 = bv ^. layoutLens slice7_11
            in Inst Add (Operands RRepr (rd :< rs1 :< rs2 :< Nil))
          _ -> js illegal
      0b101 -> case rv of
        RVRepr _ (ExtensionsRepr _ _ _ FDYesRepr _) -> js $
          -- C.FSDSP
          let rs1 = 0b00010
              rs2 = bv ^. layoutLens slice2_6
              imm = bv ^. layoutLens sp_imm_d_s
              offset = szext $ sconcat imm (0 :: SizedBV 3)
          in Inst Fsd (Operands SRepr (rs1 :< rs2 :< offset :< Nil))
        _ -> js illegal
      0b110 -> js $
        let rs1 = 0b00010
            rs2 = bv ^. layoutLens slice2_6
            imm = bv ^. layoutLens sp_imm_w_s
            offset = szext $ sconcat imm (0 :: SizedBV 2)
        in Inst Sw (Operands SRepr (rs1 :< rs2 :< offset :< Nil))
      0b111 -> case rv of
        -- C.FSWSP, C.SDSP
        RVRepr RV32Repr (ExtensionsRepr _ _ _ FDYesRepr _) -> js $
          -- C.FSWSP
          let rs1 = 0b00010
              rs2 = bv ^. layoutLens slice2_6
              imm = bv ^. layoutLens sp_imm_w_s
              offset = szext $ sconcat imm (0 :: SizedBV 2)
          in Inst Fsw (Operands SRepr (rs1 :< rs2 :< offset :< Nil))
        RVRepr RV32Repr (ExtensionsRepr _ _ _ FYesDNoRepr _) -> js $
          -- C.FSWSP
          let rs1 = 0b00010
              rs2 = bv ^. layoutLens slice2_6
              imm = bv ^. layoutLens sp_imm_w_s
              offset = szext $ sconcat imm (0 :: SizedBV 2)
          in Inst Fsw (Operands SRepr (rs1 :< rs2 :< offset :< Nil))
        RVRepr RV64Repr _ -> js $
          -- C.SDSP
          let rs1 = 0b00010
              rs2 = bv ^. layoutLens slice2_6
              imm = bv ^. layoutLens sp_imm_d_s
              offset = szext $ sconcat imm (0 :: SizedBV 3)
          in Inst Sd (Operands SRepr (rs1 :< rs2 :< offset :< Nil))
        _ -> js illegal
      _ -> js illegal
    _ -> Nothing
  where js = Just . Some
        illegal = Inst Illegal (Operands XRepr (szext bv :< Nil))
        slice0_1   = singleChunk @0  @16 @2
        slice2_4   = singleChunk @2  @16 @3
        slice2_6   = singleChunk @2  @16 @5
        slice7_9   = singleChunk @7  @16 @3
        slice7_11  = singleChunk @7  @16 @5
        slice10_11 = singleChunk @10 @16 @2
        slice13_15 = singleChunk @13 @16 @3
        slice12_12 = singleChunk @12 @16 @1
        arithCtrl = chunk @1 @12 <:
                    chunk @2 @5 <:
                    (empty :: BitLayout 16 0)
        addi4spn_imm = chunk @4 @7 <:
                       chunk @2 @11 <:
                       chunk @1 @5 <:
                       chunk @1 @6 <:
                       (empty :: BitLayout 16 0)
        q0imm = (chunk @2 @5) <:
                (chunk @3 @10) <:
                (empty :: BitLayout 16 0)
        q0imm2 = chunk @1 @5 <:
                 chunk @3 @10 <:
                 chunk @1 @6 <:
                 (empty :: BitLayout 16 0)
        imm6    = chunk @1 @12 <:
                  chunk @5 @2 <:
                  (empty :: BitLayout 16 0)
        j_imm = chunk @1 @12 <:
                chunk @1 @8 <:
                chunk @2 @9 <:
                chunk @1 @6 <:
                chunk @1 @7 <:
                chunk @1 @2 <:
                chunk @1 @11 <:
                chunk @3 @3 <:
                (empty :: BitLayout 16 0)
        b_imm = chunk @1 @12 <:
                chunk @2 @5 <:
                chunk @1 @2 <:
                chunk @2 @10 <:
                chunk @2 @3 <:
                (empty :: BitLayout 16 0)
        addi16sp_imm = chunk @1 @12 <:
                       chunk @2 @3 <:
                       chunk @1 @5 <:
                       chunk @1 @2 <:
                       chunk @1 @6 <:
                       (empty :: BitLayout 16 0)
        sp_imm_w   = chunk @2 @2 <:
                     chunk @1 @12 <:
                     chunk @3 @4 <:
                     (empty :: BitLayout 16 0)
        sp_imm_d   = chunk @3 @2 <:
                     chunk @1 @12 <:
                     chunk @2 @5 <:
                     (empty :: BitLayout 16 0)
        sp_imm_w_s = chunk @2 @7 <:
                     chunk @4 @9 <:
                     (empty :: BitLayout 16 0)
        sp_imm_d_s = chunk @3 @7 <:
                     chunk @3 @10 <:
                     (empty :: BitLayout 16 0)
