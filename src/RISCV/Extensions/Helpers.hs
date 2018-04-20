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
  ( ArithOp, MemReadFn, MemWriteFn, ExtFn, CompOp
  , getArchWidth, getArchWidthBV
  , incrPC
  , rOp, rOp32 , iOp
  , l, s, b
  , memRead16, memRead32, memRead64
  , assignMem16, assignMem32, assignMem64
  ) where

import Data.BitVector.Sized
import Data.Parameterized
import Data.Parameterized.List
import GHC.TypeLits

import RISCV.Semantics
import RISCV.Types

-- | Get the architecture width as a NatRepr
getArchWidth :: KnownArch arch => FormulaBuilder arch fmt (NatRepr (ArchWidth arch))
getArchWidth = return knownNat

getArchWidthBV :: (KnownArch arch, KnownNat w) => FormulaBuilder arch fmt (Expr arch fmt w)
getArchWidthBV = do
  aw <- getArchWidth
  return $ litBV $ bitVector $ fromIntegral $ natValue aw

-- | Increment the PC
incrPC :: KnownArch arch => FormulaBuilder arch fmt ()
incrPC = do
  ib' <- instBytes
  ib <- zextE ib'

  pc <- pcRead
  new_pc <- pc `addE` ib

  assignPC new_pc

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

  res <- op x_rs1 x_rs2
  assignReg rd res
  incrPC

-- | Like 'rOp', but truncate the result to 32 bits before storing the result in the
-- destination register.
rOp32 :: KnownArch arch => ArithOp arch R w -> FormulaBuilder arch R ()
rOp32 op = do
  rd :< rs1 :< rs2 :< Nil  <- operandEs

  x_rs1 <- regRead rs1
  x_rs2 <- regRead rs2

  a  <- op x_rs1 x_rs2
  a' <- extractEWithRepr (knownNat :: NatRepr 32) 0 a

  res <- sextE a'
  assignReg rd res
  incrPC

-- | Define an I-type arithmetic operation in 'FormulaBuilder' from an 'ArithOp'.
iOp :: KnownArch arch => ArithOp arch I (ArchWidth arch) -> FormulaBuilder arch I ()
iOp op = do
  rd :< rs1 :< imm12 :< Nil <- operandEs

  x_rs1 <- regRead rs1
  sext_imm12 <- sextE imm12

  result <- op x_rs1 sext_imm12
  assignReg rd result
  incrPC

type MemReadFn arch w fmt = KnownArch arch => Expr arch fmt (ArchWidth arch) -> FormulaBuilder arch fmt (Expr arch fmt w)
type ExtFn arch w fmt = KnownArch arch => Expr arch fmt w -> FormulaBuilder arch fmt (Expr arch fmt (ArchWidth arch))

memRead16 :: MemReadFn arch 16 fmt
memRead16 addr = do
  addr_1 <- addr `addE` litBV 1

  m0 <- memRead addr
  m1 <- memRead addr_1

  mVal <- m1 `concatE` m0
  return mVal

memRead32 :: MemReadFn arch 32 fmt
memRead32 addr = do
  addr_1 <- addr `addE` litBV 1
  addr_2 <- addr `addE` litBV 2
  addr_3 <- addr `addE` litBV 3

  m0 <- memRead addr
  m1 <- memRead addr_1
  m2 <- memRead addr_2
  m3 <- memRead addr_3

  mVal1 <- m1 `concatE` m0
  mVal2 <- m2 `concatE` mVal1
  mVal  <- m3 `concatE` mVal2
  return mVal

memRead64 :: MemReadFn arch 64 fmt
memRead64 addr = do
  addr_1 <- addr `addE` litBV 1
  addr_2 <- addr `addE` litBV 2
  addr_3 <- addr `addE` litBV 3
  addr_4 <- addr `addE` litBV 4
  addr_5 <- addr `addE` litBV 5
  addr_6 <- addr `addE` litBV 6
  addr_7 <- addr `addE` litBV 7

  m0 <- memRead addr
  m1 <- memRead addr_1
  m2 <- memRead addr_2
  m3 <- memRead addr_3
  m4 <- memRead addr_4
  m5 <- memRead addr_5
  m6 <- memRead addr_6
  m7 <- memRead addr_7

  mVal1 <- m1 `concatE` m0
  mVal2 <- m2 `concatE` mVal1
  mVal3 <- m3 `concatE` mVal2
  mVal4 <- m4 `concatE` mVal3
  mVal5 <- m5 `concatE` mVal4
  mVal6 <- m6 `concatE` mVal5
  mVal  <- m7 `concatE` mVal6
  return mVal

l :: KnownArch arch
  => MemReadFn arch w I
  -> ExtFn arch w I
  -> FormulaBuilder arch I ()
l rdFn extFn = do
  rd :< rs1 :< offset :< Nil <- operandEs

  x_rs1       <- regRead rs1
  sext_offset <- sextE offset
  addr        <- x_rs1 `addE` sext_offset
  mVal        <- rdFn addr
  ext_byte    <- extFn mVal

  assignReg rd ext_byte
  incrPC

type MemWriteFn arch w fmt = KnownArch arch => Expr arch fmt (ArchWidth arch) -> Expr arch fmt w -> FormulaBuilder arch fmt ()

assignMem16 :: MemWriteFn arch 16 fmt
assignMem16 addr val = do
  addr_1 <- addr `addE` litBV 1

  m0 <- extractE 0 val
  m1 <- extractE 8 val

  assignMem addr   m0
  assignMem addr_1 m1

assignMem32 :: MemWriteFn arch 32 fmt
assignMem32 addr val = do
  addr_1 <- addr `addE` litBV 1
  addr_2 <- addr `addE` litBV 2
  addr_3 <- addr `addE` litBV 3

  m0 <- extractE 0 val
  m1 <- extractE 8 val
  m2 <- extractE 16 val
  m3 <- extractE 24 val

  assignMem addr   m0
  assignMem addr_1 m1
  assignMem addr_2 m2
  assignMem addr_3 m3

assignMem64 :: MemWriteFn arch 64 fmt
assignMem64 addr val = do
  addr_1 <- addr `addE` litBV 1
  addr_2 <- addr `addE` litBV 2
  addr_3 <- addr `addE` litBV 3
  addr_4 <- addr `addE` litBV 4
  addr_5 <- addr `addE` litBV 5
  addr_6 <- addr `addE` litBV 6
  addr_7 <- addr `addE` litBV 7

  m0 <- extractE 0 val
  m1 <- extractE 8 val
  m2 <- extractE 16 val
  m3 <- extractE 24 val
  m4 <- extractE 32 val
  m5 <- extractE 40 val
  m6 <- extractE 48 val
  m7 <- extractE 56 val

  assignMem addr   m0
  assignMem addr_1 m1
  assignMem addr_2 m2
  assignMem addr_3 m3
  assignMem addr_4 m4
  assignMem addr_5 m5
  assignMem addr_6 m6
  assignMem addr_7 m7


s :: (KnownArch arch, KnownNat w)
  => MemWriteFn arch w S
  -> FormulaBuilder arch S ()
s wrFn = do
  rs1 :< rs2 :< offset :< Nil <- operandEs

  x_rs1 <- regRead rs1
  x_rs2 <- regRead rs2

  mVal <- extractE 0 x_rs2 -- extract some bits from the register value
  sext_offset <- sextE offset
  addr <- x_rs1 `addE` sext_offset

  wrFn addr mVal
  incrPC

type CompOp arch fmt = Expr arch fmt (ArchWidth arch)
                    -> Expr arch fmt (ArchWidth arch)
                    -> FormulaBuilder arch fmt (Expr arch fmt 1)

b :: KnownArch arch => CompOp arch B -> FormulaBuilder arch B ()
b cmp = do
  rs1 :< rs2 :< offset' :< Nil <- operandEs
  -- Need to left shift the offset by 1
  offset <- offset' `sllE` litBV 1

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

