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
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : GRIFT.Semantics.Utils
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Helper functions for defining instruction semantics.
-}

module GRIFT.Semantics.Utils
  ( -- * General
    getArchWidth
  , incrPC
  , jump
  , nanBox32
  , unBox32
  , cases
  , branches
    -- * CSRs and Exceptions
  , CSR(..)
  , Exception(..)
  , Privilege(..)
  , encodeCSR
  , resetCSRs
  , checkCSR
  , readCSR
  , writeCSR
  , raiseException
  , getMCause
  , getPrivCode
    -- * Floating point
  , raiseFPExceptions
  , withRM
  , getFResCanonical32
  , getFResCanonical64
  ) where

import Data.BitVector.Sized
import Data.BitVector.Sized.App
import Data.BitVector.Sized.Float.App
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Parameterized
import GHC.TypeLits

import GRIFT.Semantics
import GRIFT.Types

-- | Recover the architecture width as a 'Nat' from the type context. The 'InstExpr'
-- should probably be generalized when we fully implement the privileged architecture.
getArchWidth :: forall rv fmt . KnownRV rv => SemanticsM (InstExpr fmt) rv (NatRepr (RVWidth rv))
getArchWidth = return (knownNat @(RVWidth rv))

-- | Increment the PC
incrPC :: KnownRV rv => SemanticsM (InstExpr fmt) rv ()
incrPC = do
  ib <- instBytes
  let pc = readPC
  assignPC $ pc `addE` ib

-- | Semantics for setting the PC. This adds a "wrapper" expression around 'assignPC'
-- that raises a misaligned exception if the C extension is not present, and the jump
-- destination is not 4-byte-aligned.
jump :: forall expr rv . (BVExpr (expr rv), StateExpr expr, KnownRV rv)
     => expr rv (RVWidth rv) -- ^ address of the jump target
     -> SemanticsM expr rv ()
jump pc = case extsC (rvExts (knownRepr :: RVRepr rv)) of
  CYesRepr -> assignPC pc
  CNoRepr -> do
    let addrValid = (pc `andE` litBV 0b11) `eqE` litBV 0
    branch addrValid
      $> assignPC pc
      $> raiseException InstructionAddressMisaligned pc

-- | NaN-box a 32-bit expression to fit in a floating point register.
nanBox32 :: forall expr rv . (BVExpr (expr rv), KnownRV rv, FExt << rv)
         => expr rv 32
         -> SemanticsM expr rv (expr rv (RVFloatWidth rv))
nanBox32 e = case extsFD (rvExts (knownRepr :: RVRepr rv)) of
  FDYesRepr   -> return $ (litBV (-1) :: expr rv 32) `concatE` e
  FYesDNoRepr -> return e
  FDNoRepr    -> undefined

-- | Take a value from a floating-point register and get a 32-bit value, checking
-- that the original value was properly NaN-boxed.
unBox32 :: forall expr rv . (BVExpr (expr rv), KnownRV rv, FExt << rv)
        => expr rv (RVFloatWidth rv)
        -> SemanticsM expr rv (expr rv 32)
unBox32 e = case extsFD (rvExts (knownRepr :: RVRepr rv)) of
  FDYesRepr -> return $
    iteE (extractE' (knownNat @32) 32 e `eqE` litBV 0xFFFFFFFF) (extractE 0 e) canonicalNaN32
  FYesDNoRepr -> return e
  FDNoRepr -> undefined

-- | Convenience function for creating a giant nested 'iteE' expression.
cases :: BVExpr expr
      => [(expr 1, expr w)] -- ^ list of guarded results
      -> expr w             -- ^ default result
      -> expr w
cases cs d = foldr (uncurry iteE) d cs

-- | Convenience function for creating a giant nested 'branch' in a semantics
-- definition.
branches :: [(expr rv 1, SemanticsM expr rv ())]
         -> SemanticsM expr rv ()
         -> SemanticsM expr rv ()
branches cs d = foldr (uncurry branch) d cs

-- | Check if a csr is accessible. The Boolean argument should be true if we need
-- write access, False if we are accessing in a read-only fashion.
checkCSR :: KnownRV rv
         => InstExpr fmt rv 1
         -> InstExpr fmt rv 12
         -> SemanticsM (InstExpr fmt) rv ()
         -> SemanticsM (InstExpr fmt) rv ()
checkCSR write csr rst = do
  let priv = readPriv
  let csrRW = extractE' (knownNat @2) 10 csr
  let csrPriv = extractE' (knownNat @2) 8 csr
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
          flags = extractE' (knownNat @5) 0 fcsr
      in zextE flags
    )
  , (csr `eqE` (litBV $ encodeCSR FRm)
    , let fcsr = rawReadCSR (litBV $ encodeCSR FCSR)
          rm = extractE' (knownNat @3) 5 fcsr
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
         => expr rv 12 -> expr rv (RVWidth rv) -> SemanticsM expr rv ()
writeCSR csr val = branches
  [ (csr `eqE` (litBV $ encodeCSR FFlags)
    , do let val' = extractE' (knownNat @5) 0 val
             fcsr = rawReadCSR (litBV $ encodeCSR FCSR)
             writeVal = extractE' (knownNat @27) 5 fcsr `concatE` val'
         assignCSR (litBV $ encodeCSR FCSR) (zextE writeVal))
  , (csr `eqE` (litBV $ encodeCSR FRm)
    , do let val' = extractE' (knownNat @3) 0 val
             fcsr = rawReadCSR (litBV $ encodeCSR FCSR)
             writeVal = extractE' (knownNat @24) 8 fcsr `concatE`
                        val' `concatE`
                        extractE' (knownNat @5) 0 fcsr
         assignCSR (litBV $ encodeCSR FCSR) (zextE writeVal))
  , (csr `eqE` (litBV $ encodeCSR FCSR)
    , do let writeVal = val `andE` (litBV 0xFF)
         assignCSR (litBV $ encodeCSR FCSR) (zextE writeVal))
  ]
  (assignCSR csr val)

-- TODO: Annotate appropriate exceptions with an Mcause.
-- | Runtime exception. This is a convenience type for calls to 'raiseException'.
data Exception = EnvironmentCall
               | Breakpoint
               | IllegalInstruction
               | InstructionAddressMisaligned
               | LoadAccessFault
               | StoreAccessFault
  deriving (Show)

-- | Map an 'Exception' to its 'BitVector' representation.
getMCause :: KnownNat w => Exception -> BitVector w
getMCause InstructionAddressMisaligned = 0
getMCause IllegalInstruction           = 2
getMCause Breakpoint                   = 3
getMCause LoadAccessFault              = 5
getMCause StoreAccessFault             = 7
getMCause EnvironmentCall              = 11 -- This is only true for M mode.

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

getPrivCode :: Privilege -> BitVector 2
getPrivCode MPriv = 3
getPrivCode SPriv = 1
getPrivCode UPriv = 0

-- TODO: It is actually an optional architectural feature to propagate certain values
-- through to mtval, so this should be a configurable option at the type level.
-- | Semantics for raising an exception.
raiseException :: (BVExpr (expr rv), StateExpr expr, KnownRV rv)
               => Exception -- ^ The exception to raise
               -> expr rv (RVWidth rv) -- ^ the value for MTVal
               -> SemanticsM expr rv ()
raiseException e info = addStmt $ AbbrevStmt (RaiseException (getMCause e) info)

-- | Raise floating point exceptions. This ORs the current fflags with the supplied
-- 5-bit value.
raiseFPExceptions :: (BVExpr (expr rv), StateExpr expr, KnownRV rv)
                  => expr rv 5 -- ^ The exception flags
                  -> SemanticsM expr rv ()
raiseFPExceptions flags = do
  let fcsr = rawReadCSR (litBV $ encodeCSR FCSR)
  assignCSR (litBV $ encodeCSR FCSR) (fcsr `orE` (zextE flags))

dynamicRM :: (BVExpr (expr rv), StateExpr expr, KnownRV rv) => expr rv 3
dynamicRM = let fcsr = rawReadCSR (litBV $ encodeCSR FCSR)
            in extractE 5 fcsr

-- | Perform a computation that requires a rounding mode by supplying a rounding
-- mode. This handles the situation where the rounding mode is invalid or dynamic; in
-- the former case an illegal instruction is raised, and in the latter, we select the
-- rounding mode in frm.
withRM :: KnownRV rv
       => InstExpr fmt rv 3
       -> (InstExpr fmt rv 3 -> SemanticsM (InstExpr fmt) rv ())
       -> SemanticsM (InstExpr fmt) rv ()
withRM rm action = do
  let rm' = iteE (rm `eqE` litBV 0b111) dynamicRM rm
  branch ((rm' `eqE` litBV 0b101) `orE` (rm' `eqE` litBV 0b110))
    $> do iw <- instWord
          raiseException IllegalInstruction iw
    $> action rm

-- | Unpack a 32-bit floating point result into the returned value and exception
-- flags. If the input is a NaN, return the canonical NaN.
getFResCanonical32 :: BVExpr expr => expr 37 -> (expr 32, expr 5)
getFResCanonical32 e = let (res, flags) = getFRes e
                           res' = iteE (isNaN32 res) canonicalNaN32 res
                       in (res', flags)

-- | Unpack a 64-bit floating point result into the returned value and exception
-- flags. If the input is a NaN, return the canonical NaN.
getFResCanonical64 :: BVExpr expr => expr 69 -> (expr 64, expr 5)
getFResCanonical64 e = let (res, flags) = getFRes e
                           res' = iteE (isNaN64 res) canonicalNaN64 res
                       in (res', flags)
