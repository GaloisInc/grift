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

import qualified Data.Parameterized.Map as Map
import Data.Parameterized

import RISCV.Semantics
import RISCV.Types

newtype InstExprList arch fmt = InstExprList [InstExpr fmt arch 1]

type CoverageMap arch exts = Map.MapF (Opcode arch exts) (InstExprList arch)

baseCoverage :: KnownArch arch => CoverageMap arch exts
baseCoverage = Map.fromList
  [ -- RV32I
    -- R type
    Pair Add  (InstExprList [litBV 1])
  , Pair Sub  (InstExprList [litBV 1])
  , Pair Sll  (InstExprList [litBV 1])
  , Pair Slt  (InstExprList [litBV 1])
  , Pair Sltu (InstExprList [litBV 1])
  , Pair Xor  (InstExprList [litBV 1])
  , Pair Srl  (InstExprList [litBV 1])
  , Pair Sra  (InstExprList [litBV 1])
  , Pair Or   (InstExprList [litBV 1])
  , Pair And  (InstExprList [litBV 1])

  -- I type
  , Pair Jalr   (InstExprList [litBV 1])
  , Pair Lb     (InstExprList [litBV 1])
  , Pair Lh     (InstExprList [litBV 1])
  , Pair Lw     (InstExprList [litBV 1])
  , Pair Lbu    (InstExprList [litBV 1])
  , Pair Lhu    (InstExprList [litBV 1])
  , Pair Addi   (InstExprList [litBV 1])
  , Pair Slti   (InstExprList [litBV 1])
  , Pair Sltiu  (InstExprList [litBV 1])
  , Pair Xori   (InstExprList [litBV 1])
  , Pair Ori    (InstExprList [litBV 1])
  , Pair Andi   (InstExprList [litBV 1])
  , Pair Slli   (InstExprList [litBV 1])
  , Pair Srli   (InstExprList [litBV 1])
  , Pair Srai   (InstExprList [litBV 1])
  , Pair Fence  (InstExprList [litBV 1])
  , Pair FenceI (InstExprList [litBV 1])
  , Pair Csrrw  (InstExprList [litBV 1])
  , Pair Csrrs  (InstExprList [litBV 1])
  , Pair Csrrc  (InstExprList [litBV 1])
  , Pair Csrrwi (InstExprList [litBV 1])
  , Pair Csrrsi (InstExprList [litBV 1])
  , Pair Csrrci (InstExprList [litBV 1])

  -- S type
  , Pair Sb (InstExprList [litBV 1])
  , Pair Sh (InstExprList [litBV 1])
  , Pair Sw (InstExprList [litBV 1])

  -- B type
  , Pair Beq  (InstExprList [litBV 1])
  , Pair Bne  (InstExprList [litBV 1])
  , Pair Blt  (InstExprList [litBV 1])
  , Pair Bge  (InstExprList [litBV 1])
  , Pair Bltu (InstExprList [litBV 1])
  , Pair Bgeu (InstExprList [litBV 1])

  -- U type
  , Pair Lui   (InstExprList [litBV 1])
  , Pair Auipc (InstExprList [litBV 1])

  -- J type
  , Pair Jal (InstExprList [litBV 1])

  -- P type
  , Pair Ecall  (InstExprList [litBV 1])
  , Pair Ebreak (InstExprList [litBV 1])

  -- X type
  , Pair Illegal (InstExprList [litBV 1])
  ]
