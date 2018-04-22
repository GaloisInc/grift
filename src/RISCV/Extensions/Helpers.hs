{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

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
  ( ArithOp, MemReadFn, MemWriteFn, ExtFn, CompOp
  , incrPC
  , rOp, rOp32 , iOp
  , l, s, b
  , memRead16, memRead32, memRead64
  , assignMem16, assignMem32, assignMem64
  ) where

import Data.BitVector.Sized.App
import Data.Parameterized
import Data.Parameterized.List
import GHC.TypeLits

import RISCV.Semantics
import RISCV.Types

-- | Increment the PC
incrPC :: KnownArch arch => FormulaBuilder arch fmt ()
incrPC = do
  ib <- instBytes
  pc <- pcRead
  assignPC $ pc `addE` (zextE ib)

-- | Type of arithmetic operator in 'FormulaBuilder'.
type ArithOp arch fmt w = Expr arch fmt (ArchWidth arch)
                       -> Expr arch fmt (ArchWidth arch)
                       -> FormulaBuilder arch fmt (Expr arch fmt w)

-- | Define an R-type operation in 'FormulaBuilder' from an 'ArithOp'.
rOp :: KnownArch arch => ArithOp arch R (ArchWidth arch) -> FormulaBuilder arch R ()
rOp op = do
  rd :< rs1 :< rs2 :< Nil <- operandEs

  x_rs1 <- regRead rs1
  x_rs2 <- regRead rs2
  res   <- x_rs1 `op` x_rs2

  assignReg rd res
  incrPC

-- | Like 'rOp', but truncate the result to 32 bits before storing the result in the
-- destination register.
rOp32 :: KnownArch arch => ArithOp arch R w -> FormulaBuilder arch R ()
rOp32 op = do
  rd :< rs1 :< rs2 :< Nil  <- operandEs

  x_rs1 <- regRead rs1
  x_rs2 <- regRead rs2
  res   <- x_rs1 `op` x_rs2

  assignReg rd $ sextE (extractEWithRepr (knownNat :: NatRepr 32) 0 res)
  incrPC

-- | Define an I-type arithmetic operation in 'FormulaBuilder' from an 'ArithOp'.
iOp :: KnownArch arch => ArithOp arch I (ArchWidth arch) -> FormulaBuilder arch I ()
iOp op = do
  rd :< rs1 :< imm12 :< Nil <- operandEs

  x_rs1 <- regRead rs1
  res   <- x_rs1 `op` (sextE imm12)

  assignReg rd res
  incrPC

type MemReadFn arch w fmt = KnownArch arch => Expr arch fmt (ArchWidth arch) -> FormulaBuilder arch fmt (Expr arch fmt w)
type ExtFn arch w fmt = KnownArch arch => Expr arch fmt w -> Expr arch fmt (ArchWidth arch)

memRead16 :: MemReadFn arch 16 fmt
memRead16 addr = do
  m0 <- memRead addr
  m1 <- memRead $ addr `addE` litBV 1

  return $ m1 `concatE` m0

memRead32 :: MemReadFn arch 32 fmt
memRead32 addr = do
  m0 <- memRead addr
  m1 <- memRead $ addr `addE` litBV 1
  m2 <- memRead $ addr `addE` litBV 2
  m3 <- memRead $ addr `addE` litBV 3

  return $ m3 `concatE` m2 `concatE` m1 `concatE` m0

memRead64 :: MemReadFn arch 64 fmt
memRead64 addr = do
  m0 <- memRead addr
  m1 <- memRead $ addr `addE` litBV 1
  m2 <- memRead $ addr `addE` litBV 2
  m3 <- memRead $ addr `addE` litBV 3
  m4 <- memRead $ addr `addE` litBV 4
  m5 <- memRead $ addr `addE` litBV 5
  m6 <- memRead $ addr `addE` litBV 6
  m7 <- memRead $ addr `addE` litBV 7

  return $
    m7 `concatE` m6 `concatE` m5 `concatE` m4 `concatE`
    m3 `concatE` m2 `concatE` m1 `concatE` m0

l :: KnownArch arch
  => MemReadFn arch w I
  -> ExtFn arch w I
  -> FormulaBuilder arch I ()
l rdFn extFn = do
  rd :< rs1 :< offset :< Nil <- operandEs

  x_rs1       <- regRead rs1
  mVal        <- rdFn (x_rs1 `addE` sextE offset)

  assignReg rd $ extFn mVal
  incrPC

type MemWriteFn arch w fmt = KnownArch arch => Expr arch fmt (ArchWidth arch) -> Expr arch fmt w -> FormulaBuilder arch fmt ()

assignMem16 :: MemWriteFn arch 16 fmt
assignMem16 addr val = do
  assignMem addr (extractE 0 val)
  assignMem (addr `addE` litBV 1) (extractE 8 val)

assignMem32 :: MemWriteFn arch 32 fmt
assignMem32 addr val = do
  assignMem addr (extractE 0 val)
  assignMem (addr `addE` litBV 1) (extractE 8 val)
  assignMem (addr `addE` litBV 2) (extractE 16 val)
  assignMem (addr `addE` litBV 3) (extractE 24 val)

assignMem64 :: MemWriteFn arch 64 fmt
assignMem64 addr val = do
  assignMem addr (extractE 0 val)
  assignMem (addr `addE` litBV 1) (extractE 8 val)
  assignMem (addr `addE` litBV 2) (extractE 16 val)
  assignMem (addr `addE` litBV 3) (extractE 24 val)
  assignMem (addr `addE` litBV 4) (extractE 32 val)
  assignMem (addr `addE` litBV 5) (extractE 40 val)
  assignMem (addr `addE` litBV 6) (extractE 48 val)
  assignMem (addr `addE` litBV 7) (extractE 56 val)


s :: (KnownArch arch, KnownNat w)
  => MemWriteFn arch w S
  -> FormulaBuilder arch S ()
s wrFn = do
  rs1 :< rs2 :< offset :< Nil <- operandEs

  x_rs1 <- regRead rs1
  x_rs2 <- regRead rs2

  wrFn (x_rs1 `addE` sextE offset) (extractE 0 x_rs2)
  incrPC

type CompOp arch fmt = Expr arch fmt (ArchWidth arch)
                    -> Expr arch fmt (ArchWidth arch)
                    -> Expr arch fmt 1

b :: KnownArch arch => CompOp arch B -> FormulaBuilder arch B ()
b cmp = do
  rs1 :< rs2 :< offset :< Nil <- operandEs

  x_rs1 <- regRead rs1
  x_rs2 <- regRead rs2

  pc <- pcRead
  ib <- instBytes

  assignPC (iteE (x_rs1 `cmp` x_rs2) (pc `addE` sextE (offset `sllE` litBV 1)) (pc `addE` zextE ib))

