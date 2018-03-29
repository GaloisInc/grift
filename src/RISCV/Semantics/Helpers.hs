{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : RISCV.Semantics.Helpers
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Helper functions for defining instruction semantics.
-}

module RISCV.Semantics.Helpers
  ( incrPC
  , ArithOp
  , rOp, rOp32
  ) where

import Data.Parameterized

import RISCV.Instruction
import RISCV.Semantics

-- | Increment the PC
incrPC :: KnownArch arch => FormulaBuilder arch fmt ()
incrPC = do
  ib' <- instBytes
  ib <- zextE ib'

  pc <- pcRead
  new_pc <- pc `addE` ib

  assignPC new_pc

-- | Type of arithmetic operator in 'FormulaBuilder'.
type ArithOp arch fmt w = BVExpr arch (ArchWidth arch)
                       -> BVExpr arch (ArchWidth arch)
                       -> FormulaBuilder arch fmt (BVExpr arch w)

-- | Define an R-type operation in 'FormulaBuilder' from an 'ArithOp'.
rOp :: KnownArch arch => ArithOp arch 'R (ArchWidth arch) -> FormulaBuilder arch 'R ()
rOp op = do
  (rd, rs1, rs2)  <- params

  x_rs1 <- regRead rs1
  x_rs2 <- regRead rs2

  res <- op x_rs1 x_rs2
  assignReg rd res
  incrPC

-- | Like 'rOp', but truncate the result to 32 bits before storing the result in the
-- destination register.
rOp32 :: KnownArch arch => ArithOp arch 'R w -> FormulaBuilder arch 'R ()
rOp32 op = do
  (rd, rs1, rs2)  <- params

  x_rs1 <- regRead rs1
  x_rs2 <- regRead rs2

  a  <- op x_rs1 x_rs2
  a' <- extractEWithRepr (knownNat :: NatRepr 32) 0 a

  res <- sextE a'
  assignReg rd res
  incrPC
