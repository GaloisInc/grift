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
  , rOp
  ) where

import GHC.TypeLits

import RISCV.Instruction
import RISCV.Semantics

incrPC :: KnownNat (ArchWidth arch) => FormulaBuilder arch fmt ()
incrPC = do
  ib' <- instBytes
  ib <- zextE ib'

  pc <- pcRead
  new_pc <- pc `addE` ib

  assignPC new_pc

type ArithOp arch fmt = BVExpr arch (ArchWidth arch)
                     -> BVExpr arch (ArchWidth arch)
                     -> FormulaBuilder arch fmt (BVExpr arch (ArchWidth arch))

rOp :: KnownNat (ArchWidth arch) => ArithOp arch 'R -> FormulaBuilder arch 'R ()
rOp op = do
  (rd, rs1, rs2)  <- params

  x_rs1 <- regRead rs1
  x_rs2 <- regRead rs2

  result <- op x_rs1 x_rs2
  assignReg rd result
  incrPC
