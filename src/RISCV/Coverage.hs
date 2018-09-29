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
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-|
Module      : RISCV.Coverage
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Experimental -- With each opcode, we associate a collection of "test expressions,"
which capture an arbitrary notion of coverage in terms of the instruction operands
and the current machine state.

This module is being actively developed, and has not been adequately documented.
-}

module RISCV.Coverage where

import Data.BitVector.Sized.App
import qualified Data.Parameterized.Map as Map
import Data.Parameterized
import Data.Parameterized.List
import GHC.TypeLits

import RISCV.Semantics
import RISCV.Types

newtype InstExprList rv fmt = InstExprList [InstExpr fmt rv 1]

type CoverageMap rv = Map.MapF (Opcode rv) (InstExprList rv)

knownCoverageMap :: forall rv . KnownRV rv
                 => CoverageMap rv
knownCoverageMap = knownCoverageWithRepr knownRepr

knownCoverageWithRepr :: RVRepr rv -> CoverageMap rv
knownCoverageWithRepr rvRepr = case rvRepr of
  RVRepr archRepr ecRepr ->
    let base = case archRepr of
          RV32Repr -> baseCoverage
          RV64Repr -> baseCoverage `Map.union` base64Coverage
          RV128Repr -> error "RV128 not yet supported"
        m = case (archRepr, ecRepr) of
          (RV32Repr, ExtensionsRepr _ MYesRepr _ _ _) -> mCoverage
          (RV64Repr, ExtensionsRepr _ MYesRepr _ _ _) -> mCoverage `Map.union` m64Coverage
          _ -> Map.empty
        a = case (archRepr, ecRepr) of
          (RV32Repr, ExtensionsRepr _ _ AYesRepr _ _) -> aCoverage
          (RV64Repr, ExtensionsRepr _ _ AYesRepr _ _) -> aCoverage `Map.union` a64Coverage
          _ -> Map.empty
        f = case ecRepr of
          ExtensionsRepr _ _ _ FDNoRepr _ -> Map.empty
          _ -> Map.empty
    in base `Map.union` m `Map.union` a `Map.union` f

exprBitCoverage :: forall fmt rv w . KnownNat w => InstExpr fmt rv w -> [InstExpr fmt rv 1]
exprBitCoverage expr = concat $ bitTests <$> [0..width-1]
  where bitTests i = [extractEWithRepr (knownNat @1) i expr,
                      notE $ extractEWithRepr (knownNat @1) i expr]
        width = fromIntegral (natValue (knownNat @w))

immBitCoverage :: forall fmt rv . KnownRepr FormatRepr fmt => [InstExpr fmt rv 1]
immBitCoverage = case knownRepr :: FormatRepr fmt of
  RRepr -> []
  IRepr -> let _ :< _ :< imm12 :< Nil = operandEsWithRepr IRepr
           in exprBitCoverage imm12
  SRepr -> let _ :< _ :< imm12 :< Nil = operandEsWithRepr SRepr
           in exprBitCoverage imm12
  BRepr -> let _ :< _ :< imm12 :< Nil = operandEsWithRepr BRepr
           in exprBitCoverage imm12
  URepr -> let _ :< imm20 :< Nil = operandEsWithRepr URepr
           in exprBitCoverage imm20
  JRepr -> let _ :< imm20 :< Nil = operandEsWithRepr JRepr
           in exprBitCoverage imm20
  HRepr -> let _ :< _ :< imm7 :< Nil = operandEsWithRepr HRepr
           in exprBitCoverage imm7
  ARepr -> let _ :< _ :< _ :< a :< b :< Nil = operandEsWithRepr ARepr
           in exprBitCoverage a ++ exprBitCoverage b
  _ -> []

regBitCoverage :: forall fmt rv . (KnownRVWidth rv, KnownRepr FormatRepr fmt) => [InstExpr fmt rv 1]
regBitCoverage = case knownRepr :: FormatRepr fmt of
  RRepr -> let _ :< rb :< rc :< Nil = operandEsWithRepr RRepr
           in exprBitCoverage (readReg rb) ++
              exprBitCoverage (readReg rc)
  IRepr -> let _ :< rb :< _ :< Nil = operandEsWithRepr IRepr
           in exprBitCoverage (readReg rb)
  SRepr -> let rs1 :< rs2 :< _ :< Nil = operandEsWithRepr SRepr
           in exprBitCoverage (readReg rs1) ++
              exprBitCoverage (readReg rs2)
  BRepr -> let _ :< rb :< _ :< Nil = operandEsWithRepr BRepr
           in exprBitCoverage (readReg rb)
  URepr -> []
  JRepr -> []
  HRepr -> let _ :< rb :< _ :< Nil = operandEsWithRepr HRepr
           in exprBitCoverage (readReg rb)
  PRepr -> []
  ARepr -> let _ :< rb :< rc :< _ :< _ :< Nil = operandEsWithRepr ARepr
           in exprBitCoverage (readReg rb) ++
              exprBitCoverage (readReg rc)
  _ -> []

singleRidCoverage :: InstExpr fmt rv 5 -> [InstExpr fmt rv 1]
singleRidCoverage rid = ridCovExpr <$> [0..31]
  where ridCovExpr bv = rid `eqE` litBV bv

ridCoverage :: forall fmt rv . KnownRepr FormatRepr fmt => [InstExpr fmt rv 1]
ridCoverage = case knownRepr :: FormatRepr fmt of
  RRepr -> let ra :< rb :< rc :< Nil = operandEsWithRepr RRepr
           in singleRidCoverage ra ++
              singleRidCoverage rb ++
              singleRidCoverage rc
  IRepr -> let ra :< rb :< _ :< Nil = operandEsWithRepr IRepr
           in singleRidCoverage ra ++
              singleRidCoverage rb
  SRepr -> let ra :< rb :< _ :< Nil = operandEsWithRepr SRepr
           in singleRidCoverage ra ++
              singleRidCoverage rb
  BRepr -> let ra :< rb :< _ :< Nil = operandEsWithRepr BRepr
           in singleRidCoverage ra ++
              singleRidCoverage rb
  URepr -> let ra :< _ :< Nil = operandEsWithRepr URepr
           in singleRidCoverage ra
  JRepr -> let ra :< _ :< Nil = operandEsWithRepr JRepr
           in singleRidCoverage ra
  HRepr -> let ra :< rb :< _ :< Nil = operandEsWithRepr HRepr
           in singleRidCoverage ra ++
              singleRidCoverage rb
  PRepr -> []
  ARepr -> let ra :< rb :< rc :< _ :< _ :< Nil = operandEsWithRepr ARepr
           in singleRidCoverage ra ++
              singleRidCoverage rb ++
              singleRidCoverage rc
  _ -> []

generalCoverage :: forall fmt rv . (KnownRVWidth rv, KnownRepr FormatRepr fmt) => [InstExpr fmt rv 1]
generalCoverage = regBitCoverage ++ immBitCoverage

baseCoverage :: KnownRVWidth rv => CoverageMap rv
baseCoverage = Map.fromList
  [ -- RV32I
    -- R type
    Pair Add  (InstExprList generalCoverage)
  , Pair Sub  (InstExprList generalCoverage)
  , Pair Sll  (InstExprList generalCoverage)
  , Pair Slt  (InstExprList generalCoverage)
  , Pair Sltu (InstExprList generalCoverage)
  , Pair Xor  (InstExprList generalCoverage)
  , Pair Srl  (InstExprList generalCoverage)
  , Pair Sra  (InstExprList generalCoverage)
  , Pair Or   (InstExprList generalCoverage)
  , Pair And  (InstExprList generalCoverage)

  -- I type
  , Pair Jalr   (InstExprList generalCoverage)
  , Pair Lb     (InstExprList generalCoverage)
  , Pair Lh     (InstExprList generalCoverage)
  , Pair Lw     (InstExprList generalCoverage)
  , Pair Lbu    (InstExprList generalCoverage)
  , Pair Lhu    (InstExprList generalCoverage)
  , Pair Addi   (InstExprList generalCoverage)
  , Pair Slti   (InstExprList generalCoverage)
  , Pair Sltiu  (InstExprList generalCoverage)
  , Pair Xori   (InstExprList generalCoverage)
  , Pair Ori    (InstExprList generalCoverage)
  , Pair Andi   (InstExprList generalCoverage)
  , Pair Slli   (InstExprList generalCoverage)
  , Pair Srli   (InstExprList generalCoverage)
  , Pair Srai   (InstExprList generalCoverage)
  , Pair Fence  (InstExprList generalCoverage)
  , Pair FenceI (InstExprList generalCoverage)
  , Pair Csrrw  (InstExprList generalCoverage)
  , Pair Csrrs  (InstExprList generalCoverage)
  , Pair Csrrc  (InstExprList generalCoverage)
  , Pair Csrrwi (InstExprList generalCoverage)
  , Pair Csrrsi (InstExprList generalCoverage)
  , Pair Csrrci (InstExprList generalCoverage)

  -- S type
  , Pair Sb (InstExprList generalCoverage)
  , Pair Sh (InstExprList generalCoverage)
  , Pair Sw (InstExprList generalCoverage)

  -- B type
  , Pair Beq  (InstExprList generalCoverage)
  , Pair Bne  (InstExprList generalCoverage)
  , Pair Blt  (InstExprList generalCoverage)
  , Pair Bge  (InstExprList generalCoverage)
  , Pair Bltu (InstExprList generalCoverage)
  , Pair Bgeu (InstExprList generalCoverage)

  -- U type
  , Pair Lui   (InstExprList generalCoverage)
  , Pair Auipc (InstExprList generalCoverage)

  -- J type
  , Pair Jal (InstExprList generalCoverage)

  -- P type
  , Pair Ecall  (InstExprList generalCoverage)
  , Pair Ebreak (InstExprList generalCoverage)
  , Pair Mret   (InstExprList generalCoverage)
  , Pair Wfi    (InstExprList generalCoverage)

  -- X type
  , Pair Illegal (InstExprList generalCoverage)
  ]

base64Coverage :: (KnownRVWidth rv, 64 <= RVWidth rv) => CoverageMap rv
base64Coverage = Map.fromList
  [ Pair Addw  (InstExprList generalCoverage)
  , Pair Subw  (InstExprList generalCoverage)
  , Pair Sllw  (InstExprList generalCoverage)
  , Pair Srlw  (InstExprList generalCoverage)
  , Pair Sraw  (InstExprList generalCoverage)
  , Pair Lwu   (InstExprList generalCoverage)
  , Pair Ld    (InstExprList generalCoverage)
  , Pair Addiw (InstExprList generalCoverage)
  , Pair Slliw (InstExprList generalCoverage)
  , Pair Srliw (InstExprList generalCoverage)
  , Pair Sraiw (InstExprList generalCoverage)
  , Pair Sd    (InstExprList generalCoverage)
  ]

mCoverage :: (KnownRVWidth rv, MExt << rv) => CoverageMap rv
mCoverage = Map.fromList
  [ Pair Mul    (InstExprList generalCoverage)
  , Pair Mulh   (InstExprList generalCoverage)
  , Pair Mulhsu (InstExprList generalCoverage)
  , Pair Mulhu  (InstExprList generalCoverage)
  , Pair Div    (InstExprList generalCoverage)
  , Pair Divu   (InstExprList generalCoverage)
  , Pair Rem    (InstExprList generalCoverage)
  , Pair Remu   (InstExprList generalCoverage)
  ]

m64Coverage :: (KnownRVWidth rv, 64 <= RVWidth rv, MExt << rv) => CoverageMap rv
m64Coverage = Map.fromList
  [ Pair Mulw  (InstExprList generalCoverage)
  , Pair Divw  (InstExprList generalCoverage)
  , Pair Divuw (InstExprList generalCoverage)
  , Pair Remw  (InstExprList generalCoverage)
  , Pair Remuw (InstExprList generalCoverage)
  ]

aCoverage :: (KnownRVWidth rv, AExt << rv) => CoverageMap rv
aCoverage = Map.fromList
  [ Pair Lrw      (InstExprList generalCoverage)
  , Pair Scw      (InstExprList generalCoverage)
  , Pair Amoswapw (InstExprList generalCoverage)
  , Pair Amoaddw  (InstExprList generalCoverage)
  , Pair Amoxorw  (InstExprList generalCoverage)
  , Pair Amoandw  (InstExprList generalCoverage)
  , Pair Amoorw   (InstExprList generalCoverage)
  , Pair Amominw  (InstExprList generalCoverage)
  , Pair Amomaxw  (InstExprList generalCoverage)
  , Pair Amominuw (InstExprList generalCoverage)
  , Pair Amomaxuw (InstExprList generalCoverage)
  ]

a64Coverage :: (KnownRVWidth rv, 64 <= RVWidth rv, AExt << rv) => CoverageMap rv
a64Coverage = Map.fromList
  [ Pair Lrd      (InstExprList generalCoverage)
  , Pair Scd      (InstExprList generalCoverage)
  , Pair Amoswapd (InstExprList generalCoverage)
  , Pair Amoaddd  (InstExprList generalCoverage)
  , Pair Amoxord  (InstExprList generalCoverage)
  , Pair Amoandd  (InstExprList generalCoverage)
  , Pair Amoord   (InstExprList generalCoverage)
  , Pair Amomind  (InstExprList generalCoverage)
  , Pair Amomaxd  (InstExprList generalCoverage)
  , Pair Amominud (InstExprList generalCoverage)
  , Pair Amomaxud (InstExprList generalCoverage)
  ]
