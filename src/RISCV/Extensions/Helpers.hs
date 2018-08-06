{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
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
  ( getArchWidth
  , incrPC
  , rOp, rOp32, iOp
  , b
  , checkCSR
  ) where

import Data.BitVector.Sized.App
import Data.Parameterized
import Data.Parameterized.List

import RISCV.Semantics
import RISCV.Semantics.Exceptions
import RISCV.Types

getArchWidth :: forall arch exts fmt . KnownArch arch => FormulaBuilder (InstExpr fmt arch exts) arch exts (NatRepr (ArchWidth arch))
getArchWidth = return (knownNat @(ArchWidth arch))

-- | Increment the PC
incrPC :: KnownArch arch => FormulaBuilder (InstExpr fmt arch exts) arch exts ()
incrPC = do
  ib <- instBytes
  let pc = readPC
  assignPC $ pc `addE` (zextE ib)

-- | Type of arithmetic operator in 'FormulaBuilder'.
type ArithOp arch exts fmt w
  =  InstExpr fmt arch exts (ArchWidth arch)
  -> InstExpr fmt arch exts (ArchWidth arch)
  -> FormulaBuilder (InstExpr fmt arch exts) arch exts (InstExpr fmt arch exts w)

-- | Define an R-type operation in 'FormulaBuilder' from an 'ArithOp'.
rOp :: KnownArch arch => ArithOp arch exts  R (ArchWidth arch) -> FormulaBuilder (InstExpr R arch exts) arch exts ()
rOp op = do
  rd :< rs1 :< rs2 :< Nil <- operandEs

  let x_rs1 = readReg rs1
  let x_rs2 = readReg rs2
  res   <- x_rs1 `op` x_rs2

  assignReg rd res
  incrPC

-- | Like 'rOp', but truncate the result to 32 bits before storing the result in the
-- destination register.
rOp32 :: KnownArch arch => ArithOp arch exts R w -> FormulaBuilder (InstExpr R arch exts) arch exts ()
rOp32 op = do
  rd :< rs1 :< rs2 :< Nil  <- operandEs

  let x_rs1 = readReg rs1
  let x_rs2 = readReg rs2
  res   <- x_rs1 `op` x_rs2

  assignReg rd $ sextE (extractEWithRepr (knownNat @32) 0 res)
  incrPC

-- | Define an I-type arithmetic operation in 'FormulaBuilder' from an 'ArithOp'.
iOp :: KnownArch arch => ArithOp arch exts I (ArchWidth arch) -> FormulaBuilder (InstExpr I arch exts) arch exts ()
iOp op = do
  rd :< rs1 :< imm12 :< Nil <- operandEs

  let x_rs1 = readReg rs1
  res   <- x_rs1 `op` (sextE imm12)

  assignReg rd res
  incrPC

-- | Generic comparison operator.
type CompOp arch exts fmt = InstExpr fmt arch exts (ArchWidth arch)
                    -> InstExpr fmt arch exts (ArchWidth arch)
                    -> InstExpr fmt arch exts 1

-- | Generic branch.
b :: KnownArch arch => CompOp arch exts B -> FormulaBuilder (InstExpr B arch exts) arch exts ()
b cmp = do
  rs1 :< rs2 :< offset :< Nil <- operandEs

  let x_rs1 = readReg rs1
  let x_rs2 = readReg rs2

  let pc = readPC
  ib <- instBytes

  assignPC (iteE (x_rs1 `cmp` x_rs2) (pc `addE` sextE (offset `sllE` litBV 1)) (pc `addE` zextE ib))

-- | Check if a csr is accessible. The Boolean argument should be true if we need
-- write access, False if we are accessing in a read-only fashion.
checkCSR :: KnownArch arch
         => InstExpr fmt arch exts 1
         -> InstExpr fmt arch exts 12
         -> FormulaBuilder (InstExpr fmt arch exts) arch exts ()
         -> FormulaBuilder (InstExpr fmt arch exts) arch exts ()
checkCSR write csr rst = do
  let priv = readPriv
  let csrRW = extractEWithRepr (knownNat @2) 10 csr
  let csrPriv = extractEWithRepr (knownNat @2) 8 csr
  let csrOK = (notE (priv `ltuE` csrPriv)) `andE` (iteE write (csrRW `ltuE` litBV 0b11) (litBV 0b1))

  iw <- instWord

  branch (notE csrOK)
    $> raiseException IllegalInstruction iw
    $> rst

