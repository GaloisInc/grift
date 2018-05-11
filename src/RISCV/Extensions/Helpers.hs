{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
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
  , rOp, rOp32, iOp
  , l, s, b
  , readMem16, readMem32, readMem64
  , assignMem16, assignMem32, assignMem64
  , checkCSR
  ) where

import Data.BitVector.Sized.App
import Data.Parameterized
import Data.Parameterized.List
import GHC.TypeLits

import RISCV.Semantics
import RISCV.Semantics.Exceptions
import RISCV.Types

-- | Increment the PC
incrPC :: KnownArch arch => FormulaBuilder arch fmt ()
incrPC = do
  ib <- instBytes
  pc <- readPC
  assignPC $ pc `addE` (zextE ib)

-- | Type of arithmetic operator in 'FormulaBuilder'.
type ArithOp arch fmt w = Expr arch fmt (ArchWidth arch)
                       -> Expr arch fmt (ArchWidth arch)
                       -> FormulaBuilder arch fmt (Expr arch fmt w)

-- | Define an R-type operation in 'FormulaBuilder' from an 'ArithOp'.
rOp :: KnownArch arch => ArithOp arch R (ArchWidth arch) -> FormulaBuilder arch R ()
rOp op = do
  rd :< rs1 :< rs2 :< Nil <- operandEs

  x_rs1 <- readReg rs1
  x_rs2 <- readReg rs2
  res   <- x_rs1 `op` x_rs2

  assignReg rd res
  incrPC

-- | Like 'rOp', but truncate the result to 32 bits before storing the result in the
-- destination register.
rOp32 :: KnownArch arch => ArithOp arch R w -> FormulaBuilder arch R ()
rOp32 op = do
  rd :< rs1 :< rs2 :< Nil  <- operandEs

  x_rs1 <- readReg rs1
  x_rs2 <- readReg rs2
  res   <- x_rs1 `op` x_rs2

  assignReg rd $ sextE (extractEWithRepr (knownNat @32) 0 res)
  incrPC

-- | Define an I-type arithmetic operation in 'FormulaBuilder' from an 'ArithOp'.
iOp :: KnownArch arch => ArithOp arch I (ArchWidth arch) -> FormulaBuilder arch I ()
iOp op = do
  rd :< rs1 :< imm12 :< Nil <- operandEs

  x_rs1 <- readReg rs1
  res   <- x_rs1 `op` (sextE imm12)

  assignReg rd res
  incrPC

-- | Generic type for functions that read from memory.
type MemReadFn arch w fmt = KnownArch arch => Expr arch fmt (ArchWidth arch) -> FormulaBuilder arch fmt (Expr arch fmt w)

-- | Generic type for functions that extend a value to the register width.
type ExtFn arch w fmt = KnownArch arch => Expr arch fmt w -> Expr arch fmt (ArchWidth arch)

-- | Read two bytes from memory.
readMem16 :: MemReadFn arch 16 fmt
readMem16 addr = do
  m0 <- readMem addr
  m1 <- readMem $ addr `addE` litBV 1

  return $ m1 `concatE` m0

-- | Read four bytes from memory.
readMem32 :: MemReadFn arch 32 fmt
readMem32 addr = do
  m0 <- readMem addr
  m1 <- readMem $ addr `addE` litBV 1
  m2 <- readMem $ addr `addE` litBV 2
  m3 <- readMem $ addr `addE` litBV 3

  return $ m3 `concatE` m2 `concatE` m1 `concatE` m0

-- | Read eight bytes from memory.
readMem64 :: MemReadFn arch 64 fmt
readMem64 addr = do
  m0 <- readMem addr
  m1 <- readMem $ addr `addE` litBV 1
  m2 <- readMem $ addr `addE` litBV 2
  m3 <- readMem $ addr `addE` litBV 3
  m4 <- readMem $ addr `addE` litBV 4
  m5 <- readMem $ addr `addE` litBV 5
  m6 <- readMem $ addr `addE` litBV 6
  m7 <- readMem $ addr `addE` litBV 7

  return $
    m7 `concatE` m6 `concatE` m5 `concatE` m4 `concatE`
    m3 `concatE` m2 `concatE` m1 `concatE` m0

-- | Generic load.
l :: KnownArch arch
  => MemReadFn arch w I
  -> ExtFn arch w I
  -> FormulaBuilder arch I ()
l rdFn extFn = do
  rd :< rs1 :< offset :< Nil <- operandEs

  x_rs1       <- readReg rs1
  mVal        <- rdFn (x_rs1 `addE` sextE offset)

  assignReg rd $ extFn mVal
  incrPC

-- | Generic type for functions that write to memory.
type MemWriteFn arch w fmt = KnownArch arch => Expr arch fmt (ArchWidth arch) -> Expr arch fmt w -> FormulaBuilder arch fmt ()

-- | Write two bytes to memory.
assignMem16 :: MemWriteFn arch 16 fmt
assignMem16 addr val = do
  assignMem addr (extractE 0 val)
  assignMem (addr `addE` litBV 1) (extractE 8 val)

-- | Write four bytes to memory.
assignMem32 :: MemWriteFn arch 32 fmt
assignMem32 addr val = do
  assignMem addr (extractE 0 val)
  assignMem (addr `addE` litBV 1) (extractE 8 val)
  assignMem (addr `addE` litBV 2) (extractE 16 val)
  assignMem (addr `addE` litBV 3) (extractE 24 val)

-- | Write eight bytes to memory.
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

-- | Generic store.
s :: (KnownArch arch, KnownNat w) => MemWriteFn arch w S -> FormulaBuilder arch S ()
s wrFn = do
  rs1 :< rs2 :< offset :< Nil <- operandEs

  x_rs1 <- readReg rs1
  x_rs2 <- readReg rs2

  wrFn (x_rs1 `addE` sextE offset) (extractE 0 x_rs2)
  incrPC

-- | Generic comparison operator.
type CompOp arch fmt = Expr arch fmt (ArchWidth arch)
                    -> Expr arch fmt (ArchWidth arch)
                    -> Expr arch fmt 1

-- | Generic branch.
b :: KnownArch arch => CompOp arch B -> FormulaBuilder arch B ()
b cmp = do
  rs1 :< rs2 :< offset :< Nil <- operandEs

  x_rs1 <- readReg rs1
  x_rs2 <- readReg rs2

  pc <- readPC
  ib <- instBytes

  assignPC (iteE (x_rs1 `cmp` x_rs2) (pc `addE` sextE (offset `sllE` litBV 1)) (pc `addE` zextE ib))

-- | Check if a csr is accessible. The Boolean argument should be true if we need
-- write access, False if we are accessing in a read-only fashion.
checkCSR :: KnownArch arch
         => Expr arch fmt 1
         -> Expr arch fmt 12
         -> FormulaBuilder arch fmt ()
         -> FormulaBuilder arch fmt ()
checkCSR write csr rst = do
  priv <- readPriv
  let csrPriv = extractEWithRepr (knownNat @2) 10 csr
  let csrRW   = extractEWithRepr (knownNat @2) 8 csr
  let csrOK = (csrPriv `ltuE` priv) `andE` (iteE write (csrRW `ltuE` litBV 0b11) (litBV 0b1))

  branch (notE csrOK)
    $> raiseException IllegalInstruction
    $> rst

