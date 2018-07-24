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

import RISCV.Semantics
import RISCV.Types

newtype InstExprList arch fmt = InstExprList [InstExpr fmt arch 1]

type CoverageMap arch exts = Map.MapF (Opcode arch exts) (InstExprList arch)

singleRegisterCoverage :: InstExpr fmt arch 5 -> [InstExpr fmt arch 1]
singleRegisterCoverage rid = regCovExpr <$> [0..31]
  where regCovExpr bv = rid `eqE` litBV bv

registerCoverage :: forall fmt arch . KnownRepr FormatRepr fmt => [InstExpr fmt arch 1]
registerCoverage = case knownRepr :: FormatRepr fmt of
  RRepr -> let ra :< rb :< rc :< Nil = operandEsWithRepr RRepr
           in singleRegisterCoverage ra ++
              singleRegisterCoverage rb ++
              singleRegisterCoverage rc
  IRepr -> let ra :< rb :< _ :< Nil = operandEsWithRepr IRepr
           in singleRegisterCoverage ra ++
              singleRegisterCoverage rb
  SRepr -> let ra :< rb :< _ :< Nil = operandEsWithRepr SRepr
           in singleRegisterCoverage ra ++
              singleRegisterCoverage rb
  BRepr -> let ra :< rb :< _ :< Nil = operandEsWithRepr BRepr
           in singleRegisterCoverage ra ++
              singleRegisterCoverage rb
  URepr -> let ra :< _ :< Nil = operandEsWithRepr URepr
           in singleRegisterCoverage ra
  JRepr -> let ra :< _ :< Nil = operandEsWithRepr JRepr
           in singleRegisterCoverage ra
  HRepr -> let ra :< rb :< _ :< Nil = operandEsWithRepr HRepr
           in singleRegisterCoverage ra ++
              singleRegisterCoverage rb
  PRepr -> []
  ARepr -> let ra :< rb :< rc :< _ :< _ :< Nil = operandEsWithRepr ARepr
           in singleRegisterCoverage ra ++
              singleRegisterCoverage rb ++
              singleRegisterCoverage rc
  _ -> []

baseCoverage :: CoverageMap arch exts
baseCoverage = Map.fromList
  [ -- RV32I
    -- R type
    Pair Add  (InstExprList registerCoverage)
  , Pair Sub  (InstExprList registerCoverage)
  , Pair Sll  (InstExprList registerCoverage)
  , Pair Slt  (InstExprList registerCoverage)
  , Pair Sltu (InstExprList registerCoverage)
  , Pair Xor  (InstExprList registerCoverage)
  , Pair Srl  (InstExprList registerCoverage)
  , Pair Sra  (InstExprList registerCoverage)
  , Pair Or   (InstExprList registerCoverage)
  , Pair And  (InstExprList registerCoverage)

  -- I type
  , Pair Jalr   (InstExprList registerCoverage)
  , Pair Lb     (InstExprList registerCoverage)
  , Pair Lh     (InstExprList registerCoverage)
  , Pair Lw     (InstExprList registerCoverage)
  , Pair Lbu    (InstExprList registerCoverage)
  , Pair Lhu    (InstExprList registerCoverage)
  , Pair Addi   (InstExprList registerCoverage)
  , Pair Slti   (InstExprList registerCoverage)
  , Pair Sltiu  (InstExprList registerCoverage)
  , Pair Xori   (InstExprList registerCoverage)
  , Pair Ori    (InstExprList registerCoverage)
  , Pair Andi   (InstExprList registerCoverage)
  , Pair Slli   (InstExprList registerCoverage)
  , Pair Srli   (InstExprList registerCoverage)
  , Pair Srai   (InstExprList registerCoverage)
  , Pair Fence  (InstExprList registerCoverage)
  , Pair FenceI (InstExprList registerCoverage)
  , Pair Csrrw  (InstExprList registerCoverage)
  , Pair Csrrs  (InstExprList registerCoverage)
  , Pair Csrrc  (InstExprList registerCoverage)
  , Pair Csrrwi (InstExprList registerCoverage)
  , Pair Csrrsi (InstExprList registerCoverage)
  , Pair Csrrci (InstExprList registerCoverage)

  -- S type
  , Pair Sb (InstExprList registerCoverage)
  , Pair Sh (InstExprList registerCoverage)
  , Pair Sw (InstExprList registerCoverage)

  -- B type
  , Pair Beq  (InstExprList registerCoverage)
  , Pair Bne  (InstExprList registerCoverage)
  , Pair Blt  (InstExprList registerCoverage)
  , Pair Bge  (InstExprList registerCoverage)
  , Pair Bltu (InstExprList registerCoverage)
  , Pair Bgeu (InstExprList registerCoverage)

  -- U type
  , Pair Lui   (InstExprList registerCoverage)
  , Pair Auipc (InstExprList registerCoverage)

  -- J type
  , Pair Jal (InstExprList registerCoverage)

  -- P type
  , Pair Ecall  (InstExprList registerCoverage)
  , Pair Ebreak (InstExprList registerCoverage)

  -- X type
  , Pair Illegal (InstExprList registerCoverage)
  ]
