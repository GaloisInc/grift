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
  , decodeCSR
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
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Parameterized
import GHC.TypeLits

import GRIFT.BitVector.BVApp
import GRIFT.BitVector.BVFloatApp
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
    let addrValid = (pc `andE` bvInteger 0b11) `eqE` bvInteger 0
    branch addrValid
      $> assignPC pc
      $> raiseException InstructionAddressMisaligned pc

-- | NaN-box a 32-bit expression to fit in a floating point register.
nanBox32 :: forall expr rv . (BVExpr (expr rv), KnownRV rv, FExt << rv, AbbrevExpr expr)
         => expr rv 32
         -> SemanticsM expr rv (expr rv (RVFloatWidth rv))
nanBox32 e = case extsFD (rvExts (knownRepr :: RVRepr rv)) of
  FDYesRepr   -> return $ abbrevExpr (NanBox32App knownNat e)
  FYesDNoRepr -> return e
  FDNoRepr    -> undefined

-- | Take a value from a floating-point register and get a 32-bit value, checking
-- that the original value was properly NaN-boxed.
unBox32 :: forall expr rv . (BVExpr (expr rv), KnownRV rv, FExt << rv, AbbrevExpr expr)
        => expr rv (RVFloatWidth rv)
        -> SemanticsM expr rv (expr rv 32)
unBox32 e = case extsFD (rvExts (knownRepr :: RVRepr rv)) of
  FDYesRepr -> return $ abbrevExpr (UnNanBox32App knownNat e)
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
         -- ^ 1 if we are writing to a CSR, 0 otherwise.
         -> InstExpr fmt rv 12
         -- ^ which CSR we are accessing.
         -> SemanticsM (InstExpr fmt) rv ()
         -- ^ semantics action to execute if we have access to the CSR.
         -> SemanticsM (InstExpr fmt) rv ()
checkCSR write csr rst = do
  let priv = readPriv
  let csrRW = extractE' (knownNat @2) (knownNat @10) csr
  let csrPriv = extractE' (knownNat @2) (knownNat @8) csr
  -- let csrBad = (notE (notE (priv `ltuE` csrPriv)) `andE` (iteE write (csrRW `ltuE` bvInteger 0b11) (bvInteger 0b1)))
  let csrBad = (priv `ltuE` csrPriv) `orE` (write `andE` notE (csrRW `ltuE` bvInteger 0b11))

  iw <- instWord

  branch csrBad
    $> raiseException IllegalInstruction iw
    $> rst

-- | Maps each CSR to an expression representing what value is returned when software
-- attempts to read it.
readCSR ::
  (w ~ RVWidth rv, 5 <= w) =>
  (StateExpr expr, BVExpr (expr rv), AbbrevExpr expr, KnownRV rv) =>
  expr rv 12 -> expr rv (RVWidth rv)
readCSR csr = abbrevExpr $ ReadCSRApp knownNat csr

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
writeCSR csr val = addStmt $ AbbrevStmt (WriteCSR csr val)

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
getMCause :: KnownNat w => Exception -> BV w
getMCause InstructionAddressMisaligned = mkBV' 0
getMCause IllegalInstruction           = mkBV' 2
getMCause Breakpoint                   = mkBV' 3
getMCause LoadAccessFault              = mkBV' 5
getMCause StoreAccessFault             = mkBV' 7
getMCause EnvironmentCall              = mkBV' 11 -- This is only true for M mode.

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
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Translate a CSR to its 'BitVector' code.
encodeCSR :: CSR -> BV 12
encodeCSR MVendorID  = mkBV' 0xF11
encodeCSR MArchID    = mkBV' 0xF12
encodeCSR MImpID     = mkBV' 0xF13
encodeCSR MHartID    = mkBV' 0xF14
encodeCSR MStatus    = mkBV' 0x300
encodeCSR MISA       = mkBV' 0x301
encodeCSR MEDeleg    = mkBV' 0x302
encodeCSR MIDeleg    = mkBV' 0x303
encodeCSR MIE        = mkBV' 0x304
encodeCSR MTVec      = mkBV' 0x305
encodeCSR MCounterEn = mkBV' 0x306
encodeCSR MScratch   = mkBV' 0x340
encodeCSR MEPC       = mkBV' 0x341
encodeCSR MCause     = mkBV' 0x342
encodeCSR MTVal      = mkBV' 0x343
encodeCSR MIP        = mkBV' 0x344
encodeCSR MCycle     = mkBV' 0xB00
encodeCSR MInstRet   = mkBV' 0xB02
encodeCSR MCycleh    = mkBV' 0xB80
encodeCSR MInstReth  = mkBV' 0xB82
encodeCSR FFlags     = mkBV' 0x001
encodeCSR FRm        = mkBV' 0x002
encodeCSR FCSR       = mkBV' 0x003

-- | Translate a 'BitVector' CSR code into a 'CSR'.
decodeCSR :: BV 12 -> Maybe CSR
decodeCSR (BV 0xF11) = Just MVendorID
decodeCSR (BV 0xF12) = Just MArchID
decodeCSR (BV 0xF13) = Just MImpID
decodeCSR (BV 0xF14) = Just MHartID
decodeCSR (BV 0x300) = Just MStatus
decodeCSR (BV 0x301) = Just MISA
decodeCSR (BV 0x302) = Just MEDeleg
decodeCSR (BV 0x303) = Just MIDeleg
decodeCSR (BV 0x304) = Just MIE
decodeCSR (BV 0x305) = Just MTVec
decodeCSR (BV 0x306) = Just MCounterEn
decodeCSR (BV 0x340) = Just MScratch
decodeCSR (BV 0x341) = Just MEPC
decodeCSR (BV 0x342) = Just MCause
decodeCSR (BV 0x343) = Just MTVal
decodeCSR (BV 0x344) = Just MIP
decodeCSR (BV 0xB00) = Just MCycle
decodeCSR (BV 0xB02) = Just MInstRet
decodeCSR (BV 0xB80) = Just MCycleh
decodeCSR (BV 0xB82) = Just MInstReth
decodeCSR (BV 0x001) = Just FFlags
decodeCSR (BV 0x002) = Just FRm
decodeCSR (BV 0x003) = Just FCSR
decodeCSR _ = Nothing

data Privilege = MPriv | SPriv | UPriv

-- | State of CSRs on reset.
resetCSRs :: KnownNat w => Map (BV 12) (BV w)
resetCSRs = Map.mapKeys encodeCSR $ Map.fromList
  [ (MVendorID, mkBV' 0x0) -- implementation defined
  , (MArchID, mkBV' 0x0) -- implemenation defined
  , (MImpID, mkBV' 0x0) -- implementation defined
  , (MHartID, mkBV' 0x0) -- implementation defined

  -- TODO: Finish this.
  ]

getPrivCode :: Privilege -> BV 2
getPrivCode MPriv = mkBV' 3
getPrivCode SPriv = mkBV' 1
getPrivCode UPriv = mkBV' 0

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
raiseFPExceptions :: (BVExpr (expr rv), StateExpr expr, KnownRV rv, 5 <= RVWidth rv)
                  => expr rv 5 -- ^ The exception flags
                  -> SemanticsM expr rv ()
raiseFPExceptions flags = do
  let fcsr = rawReadCSR (litBV knownNat $ encodeCSR FCSR)
  assignCSR (litBV knownNat $ encodeCSR FCSR) (fcsr `orE` zextEOrId flags)

dynamicRM :: (BVExpr (expr rv), StateExpr expr, KnownRV rv) => expr rv 3
dynamicRM = let fcsr = rawReadCSR (litBV knownNat $ encodeCSR FCSR)
            in extractE (knownNat @5) fcsr

-- | Perform a computation that requires a rounding mode by supplying a rounding
-- mode. This handles the situation where the rounding mode is invalid or dynamic; in
-- the former case an illegal instruction is raised, and in the latter, we select the
-- rounding mode in frm.
withRM :: KnownRV rv
       => InstExpr fmt rv 3
       -> (InstExpr fmt rv 3 -> SemanticsM (InstExpr fmt) rv ())
       -> SemanticsM (InstExpr fmt) rv ()
withRM rm action = do
  let rm' = iteE (rm `eqE` bvInteger 0b111) dynamicRM rm
  branch ((rm' `eqE` bvInteger 0b101) `orE` (rm' `eqE` bvInteger 0b110))
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
