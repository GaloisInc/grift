{-
This file is part of GRIFT (Galois RISC-V ISA Formal Tools).

GRIFT is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GRIFT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero Public License for more details.

You should have received a copy of the GNU Affero Public License
along with GRIFT.  If not, see <https://www.gnu.org/licenses/>.
-}

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
Module      : RISCV.InstructionSet.Helpers
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Helper functions for defining instruction semantics.
-}

module RISCV.InstructionSet.Helpers
  ( getArchWidth
  , incrPC
  , CompOp
  , b
  , checkCSR
  ) where

import Data.BitVector.Sized.App
import Data.Parameterized
import Data.Parameterized.List

import RISCV.Semantics
import RISCV.Semantics.Exceptions
import RISCV.Types

-- | Recover the architecture width as a 'Nat' from the type context. The 'InstExpr'
-- should probably be generalized when we fully implement the privileged architecture.
getArchWidth :: forall rv fmt . KnownRV rv => SemanticsBuilder (InstExpr fmt rv) rv (NatRepr (RVWidth rv))
getArchWidth = return (knownNat @(RVWidth rv))

-- | Increment the PC
incrPC :: KnownRV rv => SemanticsBuilder (InstExpr fmt rv) rv ()
incrPC = do
  ib <- instBytes
  let pc = readPC
  assignPC $ pc `addE` (zextE ib)

-- TODO: Deprecate a lot of these helpers. I actually think it's better to be more
-- explicit in the instruction semantics themselves than to hide everything with
-- abbreviations.

-- | Generic comparison operator.
type CompOp rv fmt = InstExpr fmt rv (RVWidth rv)
                    -> InstExpr fmt rv (RVWidth rv)
                    -> InstExpr fmt rv 1

-- | Generic branch.
b :: KnownRV rv => CompOp rv B -> SemanticsBuilder (InstExpr B rv) rv ()
b cmp = do
  rs1 :< rs2 :< offset :< Nil <- operandEs

  let x_rs1 = readReg rs1
  let x_rs2 = readReg rs2

  let pc = readPC
  ib <- instBytes

  assignPC (iteE (x_rs1 `cmp` x_rs2) (pc `addE` sextE (offset `sllE` litBV 1)) (pc `addE` zextE ib))

-- | Check if a csr is accessible. The Boolean argument should be true if we need
-- write access, False if we are accessing in a read-only fashion.
checkCSR :: KnownRV rv
         => InstExpr fmt rv 1
         -> InstExpr fmt rv 12
         -> SemanticsBuilder (InstExpr fmt rv) rv ()
         -> SemanticsBuilder (InstExpr fmt rv) rv ()
checkCSR write csr rst = do
  let priv = readPriv
  let csrRW = extractEWithRepr (knownNat @2) 10 csr
  let csrPriv = extractEWithRepr (knownNat @2) 8 csr
  let csrOK = (notE (priv `ltuE` csrPriv)) `andE` (iteE write (csrRW `ltuE` litBV 0b11) (litBV 0b1))

  iw <- instWord

  branch (notE csrOK)
    $> raiseException IllegalInstruction iw
    $> rst

