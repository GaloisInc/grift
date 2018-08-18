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
Module      : RISCV.InstructionSet.Utils
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Helper functions for defining instruction semantics.
-}

module RISCV.InstructionSet.Utils
  ( -- * General
    getArchWidth
  , incrPC
  , cases
  , branches
  , CompOp
  , b
    -- * CSRs and Exceptions
  , CSR(..)
  , Exception(..)
  , encodeCSR
  , resetCSRs
  , checkCSR
  , readCSR
  , writeCSR
  , raiseException
  , getMCause
    -- * Floating point
  , raiseFPExceptions
  , withRM
  , getFResCanonical
  ) where

import Data.BitVector.Sized
import Data.BitVector.Sized.App
import Data.BitVector.Sized.Float.App
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Parameterized
import Data.Parameterized.List
import GHC.TypeLits

import RISCV.Semantics
import RISCV.Types

-- | Recover the architecture width as a 'Nat' from the type context. The 'InstExpr'
-- should probably be generalized when we fully implement the privileged architecture.
getArchWidth :: forall rv fmt . KnownRV rv => SemanticsM (InstExpr fmt rv) rv (NatRepr (RVWidth rv))
getArchWidth = return (knownNat @(RVWidth rv))

-- | Increment the PC
incrPC :: KnownRV rv => SemanticsM (InstExpr fmt rv) rv ()
incrPC = do
  ib <- instBytes
  let pc = readPC
  assignPC $ pc `addE` ib

-- TODO: get rid of CompOp and b.
-- | Generic comparison operator.
type CompOp rv fmt = InstExpr fmt rv (RVWidth rv)
                    -> InstExpr fmt rv (RVWidth rv)
                    -> InstExpr fmt rv 1

-- | Generic branch.
b :: KnownRV rv => CompOp rv B -> SemanticsM (InstExpr B rv) rv ()
b cmp = do
  rs1 :< rs2 :< offset :< Nil <- operandEs

  let x_rs1 = readReg rs1
  let x_rs2 = readReg rs2

  let pc = readPC
  ib <- instBytes

  assignPC (iteE (x_rs1 `cmp` x_rs2) (pc `addE` sextE (offset `sllE` litBV 1)) (pc `addE` zextE ib))

cases :: BVExpr expr
      => [(expr 1, expr w)] -- ^ list of guarded results
      -> expr w             -- ^ default result
      -> expr w
cases cs d = foldr (uncurry iteE) d cs

branches :: [(expr 1, SemanticsM expr rv ())]
         -> SemanticsM expr rv ()
         -> SemanticsM expr rv ()
branches cs d = foldr (uncurry branch) d cs

-- | Check if a csr is accessible. The Boolean argument should be true if we need
-- write access, False if we are accessing in a read-only fashion.
checkCSR :: KnownRV rv
         => InstExpr fmt rv 1
         -> InstExpr fmt rv 12
         -> SemanticsM (InstExpr fmt rv) rv ()
         -> SemanticsM (InstExpr fmt rv) rv ()
checkCSR write csr rst = do
  let priv = readPriv
  let csrRW = extractEWithRepr (knownNat @2) 10 csr
  let csrPriv = extractEWithRepr (knownNat @2) 8 csr
  let csrOK = (notE (priv `ltuE` csrPriv)) `andE` (iteE write (csrRW `ltuE` litBV 0b11) (litBV 0b1))

  iw <- instWord

  branch (notE csrOK)
    $> raiseException IllegalInstruction iw
    $> rst

-- | Maps each CSR to an expression representing what value is returned when software
-- attempts to read it.
readCSR :: (StateExpr expr, BVExpr (expr rv), KnownRV rv)
        => expr rv 12 -> expr rv (RVWidth rv)
readCSR csr = cases
  [ (csr `eqE` (litBV $ encodeCSR FFlags)
    , let fcsr = rawReadCSR (litBV $ encodeCSR FCSR)
          flags = extractEWithRepr (knownNat @5) 0 fcsr
      in zextE flags
    )
  , (csr `eqE` (litBV $ encodeCSR FRm)
    , let fcsr = rawReadCSR (litBV $ encodeCSR FCSR)
          rm = extractEWithRepr (knownNat @3) 5 fcsr
      in zextE rm
    )
  ]
  (rawReadCSR csr)

-- | Maps each CSR to a function taking a 'BVExpr' expression to a semantic action,
-- determining what happens if you try to write a particular value to that CSR.  The
-- idea is that if you try to write a value to a CSR, what actually ends up getting
-- written is dependent on the particular CSR and the input.
--
-- At the moment, this function is little more than an alias for
-- 'assignCSR', with a few aliasing cases (like for fflags and frm, as well as the
-- various aliased registers in S- and U-mode of M-mode registers).
writeCSR :: (StateExpr expr, BVExpr (expr rv), KnownRV rv)
              => expr rv 12 -> expr rv (RVWidth rv) -> SemanticsM (expr rv) rv ()
writeCSR csr val = branches
  [ (csr `eqE` (litBV $ encodeCSR FFlags)
    , do let val' = extractEWithRepr (knownNat @5) 0 val
             fcsr = rawReadCSR (litBV $ encodeCSR FCSR)
             writeVal = extractEWithRepr (knownNat @27) 5 fcsr `concatE` val'
         assignCSR (litBV $ encodeCSR FCSR) (zextE writeVal)
    )
  , (csr `eqE` (litBV $ encodeCSR FRm)
    , do let val' = extractEWithRepr (knownNat @3) 5 val
             fcsr = rawReadCSR (litBV $ encodeCSR FCSR)
             writeVal = extractEWithRepr (knownNat @24) 8 fcsr `concatE`
                        val' `concatE`
                        extractEWithRepr (knownNat @5) 0 fcsr
         assignCSR (litBV $ encodeCSR FCSR) (zextE writeVal)
    )
  ]
  (assignCSR csr val)

-- TODO: Annotate appropriate exceptions with an Mcause.
-- | Runtime exception. This is a convenience type for calls to 'raiseException'.
data Exception = EnvironmentCall
               | Breakpoint
               | IllegalInstruction
               | LoadAccessFault
               | StoreAccessFault
  deriving (Show)

-- | Map an 'Exception' to its 'BitVector' representation.
getMCause :: KnownNat w => Exception -> BitVector w
getMCause IllegalInstruction = 2
getMCause Breakpoint         = 3
getMCause LoadAccessFault    = 5
getMCause StoreAccessFault   = 7
getMCause EnvironmentCall    = 11 -- This is only true for M mode.

-- | Abstract datatype for CSR.
data CSR = MVendorID
         | MArchID
         | MImpID
         | MHartID
         | MStatus
         | MISA
         | MEDeleg
         | MIDeleg
         | MIE
         | MTVec
         | MCounterEn
         | MScratch
         | MEPC
         | MCause
         | MTVal
         | MIP
         -- Skipping PMP registers
         | MCycle
         | MInstRet
         -- Skipping MHPM counters
         | MCycleh
         | MInstReth
         -- Skipping MHPMh counters
         -- Skipping MHPMEvents
         -- Skipping debug/trace registers
         -- Skipping debug mode registers
         | FRm
         | FFlags
         | FCSR
         -- TODO: special semantics handling for FFlags, FRm
  deriving (Eq, Ord, Bounded, Enum)

-- | Translate a CSR to its 'BitVector' code.
encodeCSR :: CSR -> BitVector 12

encodeCSR MVendorID  = 0xF11
encodeCSR MArchID    = 0xF12
encodeCSR MImpID     = 0xF13
encodeCSR MHartID    = 0xF14

encodeCSR MStatus    = 0x300
encodeCSR MISA       = 0x301
encodeCSR MEDeleg    = 0x302
encodeCSR MIDeleg    = 0x303
encodeCSR MIE        = 0x304
encodeCSR MTVec      = 0x305
encodeCSR MCounterEn = 0x306

encodeCSR MScratch   = 0x340
encodeCSR MEPC       = 0x341
encodeCSR MCause     = 0x342
encodeCSR MTVal      = 0x343
encodeCSR MIP        = 0x344

encodeCSR MCycle     = 0xB00
encodeCSR MInstRet   = 0xB02
encodeCSR MCycleh    = 0xB80
encodeCSR MInstReth  = 0xB82

encodeCSR FFlags     = 0x001
encodeCSR FRm        = 0x002
encodeCSR FCSR       = 0x003

data Privilege = MPriv | SPriv | UPriv

-- | State of CSRs on reset.
resetCSRs :: KnownNat w => Map (BitVector 12) (BitVector w)
resetCSRs = Map.mapKeys encodeCSR $ Map.fromList
  [ (MVendorID, 0x0) -- implementation defined
  , (MArchID, 0x0) -- implemenation defined
  , (MImpID, 0x0) -- implementation defined
  , (MHartID, 0x0) -- implementation defined

  -- TODO: Finish this.
  ]

-- TODO: It is actually an optional architectural feature to propagate certain values
-- through to mtval, so this should be a configurable option at the type level.

getPrivCode :: Privilege -> BitVector 2
getPrivCode MPriv = 3
getPrivCode SPriv = 1
getPrivCode UPriv = 0

-- | Semantics for raising an exception.
raiseException :: (BVExpr (expr rv), StateExpr expr, KnownRV rv)
               => Exception
               -> expr rv (RVWidth rv)
               -> SemanticsM (expr rv) rv ()
raiseException e info = do
  -- Exception handling TODO:
  -- - For interrupts, PC should be incremented.
  -- - mtval should be an argument to this function based on the exception
  -- - MIE and MPIE need to be set appropriately, but we are not worrying about this
  --   for now
  -- - MPP (don't need this until we have other privilege modes)
  -- - We are assuming we do not have supervisor mode

  let pc      = readPC
  let priv    = readPriv
  let mtVec   = rawReadCSR (litBV $ encodeCSR MTVec)
  let mstatus = rawReadCSR (litBV $ encodeCSR MStatus)

  let mtVecBase = (mtVec `srlE` litBV 2) `sllE` litBV 2 -- ignore mode for now
      mcause = getMCause e

  assignPriv (litBV $ getPrivCode MPriv)
  assignCSR (litBV $ encodeCSR MTVal)   info -- TODO: actually thread info in here
  assignCSR (litBV $ encodeCSR MStatus) (mstatus `orE` sllE (zextE priv) (litBV 11))
  assignCSR (litBV $ encodeCSR MEPC)    pc
  assignCSR (litBV $ encodeCSR MCause)  (litBV mcause)

  assignPC mtVecBase

raiseFPExceptions :: (BVExpr (expr rv), StateExpr expr, KnownRV rv)
                  => expr rv 5 -- ^ The exception flags
                  -> SemanticsM (expr rv) rv ()
raiseFPExceptions flags = do
  let fcsr = rawReadCSR (litBV $ encodeCSR FCSR)
  assignCSR (litBV $ encodeCSR FCSR) (fcsr `orE` (zextE flags))

dynamicRM :: (BVExpr (expr rv), StateExpr expr, KnownRV rv) => expr rv 3
dynamicRM = let fcsr = rawReadCSR (litBV $ encodeCSR FCSR)
            in extractE 5 fcsr

withRM :: KnownRV rv
       => InstExpr fmt rv 3
       -> (InstExpr fmt rv 3 -> SemanticsM (InstExpr fmt rv) rv ())
       -> SemanticsM (InstExpr fmt rv) rv ()
withRM rm action = do
  let rm' = iteE (rm `eqE` litBV 0b111) dynamicRM rm
  branch ((rm' `eqE` litBV 0b101) `orE` (rm' `eqE` litBV 0b110))
    $> do iw <- instWord
          raiseException IllegalInstruction iw
    $> action rm

getFResCanonical :: BVExpr expr => expr 37 -> (expr 32, expr 5)
getFResCanonical e = let (res, flags) = getFRes e
                         res' = iteE (isNaN32 res) canonicalNaN32 res
                     in (res', flags)
