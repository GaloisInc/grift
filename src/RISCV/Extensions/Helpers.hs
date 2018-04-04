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
Module      : RISCV.Extensions.Helpers
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Helper functions for defining instruction semantics.
-}

module RISCV.Extensions.Helpers
  ( incrPC
  , ArithOp
  , rOp, rOp32
  , iOp
  , ls, lu
  , CompOp
  , b, s
  ) where

import Data.Parameterized
import GHC.TypeLits

import RISCV.Instruction
import RISCV.Semantics
import RISCV.Types

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

iOp :: KnownArch arch => ArithOp arch 'I (ArchWidth arch) -> FormulaBuilder arch 'I ()
iOp op = do
  (rd, rs1, imm12) <- params

  x_rs1 <- regRead rs1
  sext_imm12 <- sextE imm12

  result <- op x_rs1 sext_imm12
  assignReg rd result
  incrPC

ls :: KnownArch arch => NatRepr bytes -> FormulaBuilder arch 'I ()
ls bRepr = do
  (rd, rs1, offset) <- params

  x_rs1 <- regRead rs1
  sext_offset <- sextE offset
  addr <- x_rs1 `addE` sext_offset
  m_byte  <- memReadWithRepr bRepr addr
  sext_byte <- sextE m_byte

  assignReg rd sext_byte
  incrPC

lu :: KnownArch arch => NatRepr bytes -> FormulaBuilder arch 'I ()
lu bRepr = do
  (rd, rs1, offset) <- params

  x_rs1 <- regRead rs1
  sext_offset <- sextE offset
  addr <- x_rs1 `addE` sext_offset
  m_byte  <- memReadWithRepr bRepr addr
  zext_byte <- zextE m_byte

  assignReg rd zext_byte
  incrPC

type CompOp arch fmt = BVExpr arch (ArchWidth arch)
                    -> BVExpr arch (ArchWidth arch)
                    -> FormulaBuilder arch fmt (BVExpr arch 1)

b :: KnownArch arch => CompOp arch 'B -> FormulaBuilder arch 'B ()
b cmp = do
  (rs1, rs2, offset) <- params

  x_rs1 <- regRead rs1
  x_rs2 <- regRead rs2
  cond  <- x_rs1 `cmp` x_rs2

  pc <- pcRead
  ib' <- instBytes
  ib <- zextE ib'
  sext_offset <- sextE offset
  pc_branch <- pc `addE` sext_offset
  pc_incr <- pc `addE` ib

  new_pc <- iteE cond pc_branch pc_incr
  assignPC new_pc


s :: (KnownArch arch, KnownNat bytes) => NatRepr bytes -> FormulaBuilder arch 'S ()
s bRepr = do
  (rs1, rs2, offset) <- params

  x_rs1 <- regRead rs1
  x_rs2 <- regRead rs2

  x_rs2_byte <- extractEWithRepr ((knownNat :: NatRepr 8) `natMultiply` bRepr) 0 x_rs2
  sext_offset <- sextE offset
  addr <- x_rs1 `addE` sext_offset

  assignMem addr x_rs2_byte
  incrPC
