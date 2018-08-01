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
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Experimental -- With each opcode, we associate a collection of "test expressions,"
which capture an arbitrary notion of coverage in terms of the instruction operands
and the current machine state.
-}

module RISCV.Coverage where

import Data.BitVector.Sized.App
import qualified Data.Parameterized.Map as Map
import Data.Parameterized
import Data.Parameterized.List
import GHC.TypeLits

import RISCV.Semantics
import RISCV.Types

newtype InstExprList arch fmt = InstExprList [InstExpr fmt arch 1]

type CoverageMap arch exts = Map.MapF (Opcode arch exts) (InstExprList arch)

knownCoverageMap :: forall arch exts
                   . (KnownArch arch, KnownExtensions exts)
                 => CoverageMap arch exts
knownCoverageMap = base `Map.union` m `Map.union` a `Map.union` f
  where archRepr = knownRepr :: BaseArchRepr arch
        ecRepr = knownRepr :: ExtensionsRepr exts
        base = case archRepr of
          RV32Repr -> baseCoverage
          RV64Repr -> baseCoverage `Map.union` base64Coverage
          RV128Repr -> error "RV128 not yet supported"
        m = case (archRepr, ecRepr) of
          (RV32Repr, ExtensionsRepr _ MYesRepr _ _) -> mCoverage
          (RV64Repr, ExtensionsRepr _ MYesRepr _ _) -> mCoverage `Map.union` m64Coverage
          _ -> Map.empty
        a = case (archRepr, ecRepr) of
          (RV32Repr, ExtensionsRepr _ _ AYesRepr _) -> aCoverage
          (RV64Repr, ExtensionsRepr _ _ AYesRepr _) -> aCoverage `Map.union` a64Coverage
          _ -> Map.empty
        f = case ecRepr of
          ExtensionsRepr _ _ _ FDNoRepr -> Map.empty
          _ -> error "Floating point not yet supported"

exprBitCoverage :: forall fmt arch w . KnownNat w => InstExpr fmt arch w -> [InstExpr fmt arch 1]
exprBitCoverage expr = concat $ bitTests <$> [0..width-1]
  where bitTests i = [extractEWithRepr (knownNat @1) i expr,
                      notE $ extractEWithRepr (knownNat @1) i expr]
        width = fromIntegral (natValue (knownNat @w))

immBitCoverage :: forall fmt arch . KnownRepr FormatRepr fmt => [InstExpr fmt arch 1]
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

regBitCoverage :: forall fmt arch . (KnownArch arch, KnownRepr FormatRepr fmt) => [InstExpr fmt arch 1]
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

singleRidCoverage :: InstExpr fmt arch 5 -> [InstExpr fmt arch 1]
singleRidCoverage rid = ridCovExpr <$> [0..31]
  where ridCovExpr bv = rid `eqE` litBV bv

ridCoverage :: forall fmt arch . KnownRepr FormatRepr fmt => [InstExpr fmt arch 1]
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

generalCoverage :: forall fmt arch . (KnownArch arch, KnownRepr FormatRepr fmt) => [InstExpr fmt arch 1]
generalCoverage = regBitCoverage ++ immBitCoverage

baseCoverage :: KnownArch arch => CoverageMap arch exts
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

base64Coverage :: (KnownArch arch, 64 <= ArchWidth arch) => CoverageMap arch exts
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

mCoverage :: (KnownArch arch, KnownExtensions exts, MExt << exts) => CoverageMap arch exts
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

m64Coverage :: (KnownArch arch, 64 <= ArchWidth arch, MExt << exts) => CoverageMap arch exts
m64Coverage = Map.fromList
  [ Pair Mulw  (InstExprList generalCoverage)
  , Pair Divw  (InstExprList generalCoverage)
  , Pair Divuw (InstExprList generalCoverage)
  , Pair Remw  (InstExprList generalCoverage)
  , Pair Remuw (InstExprList generalCoverage)
  ]

aCoverage :: (KnownArch arch, AExt << exts) => CoverageMap arch exts
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

a64Coverage :: (KnownArch arch, 64 <= ArchWidth arch, AExt << exts) => CoverageMap arch exts
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
