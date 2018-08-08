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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : RISCV.Semantics.Exceptions
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module provides types and functions for the semantics of exception handling in
RISC-V. It is currently being developed, so it is not as clean as it will be. This is
mainly hacked together to get something working quickly, and it does not represent
the full functionality.
-}

module RISCV.Semantics.Exceptions
  ( -- ** CSRs and Exceptions
    CSR(..)
  , encodeCSR
  , resetCSRs
  , Exception(..)
  , raiseException
  , getMCause
  ) where

import Data.BitVector.Sized
import Data.BitVector.Sized.App
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Parameterized
import GHC.TypeLits

import RISCV.Semantics
import RISCV.Types

-- TODO: Annotate appropriate exceptions with an Mcause.
-- | Runtime exception. This is a convenience type for calls to 'raiseException'.
data Exception = EnvironmentCall
               | Breakpoint
               | IllegalInstruction
               | LoadAccessFault
               | StoreAccessFault
  deriving (Show)

interruptBV :: forall w . KnownNat w => BitVector w
interruptBV = 1 `bvShiftL` fromIntegral (natValue (knownNat @w) - 1)

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
  deriving (Eq, Ord)

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

data Privilege = MPriv | SPriv | UPriv

getPrivCode :: Privilege -> BitVector 2
getPrivCode MPriv = 3
getPrivCode SPriv = 1
getPrivCode UPriv = 0

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

-- | Semantics for raising an exception.
raiseException :: (BVExpr (expr rv), RVStateExpr expr, KnownRV rv)
               => Exception
               -> expr rv (RVWidth rv)
               -> FormulaBuilder (expr rv) rv ()
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
  let mtVec   = readCSR (litBV $ encodeCSR MTVec)
  let mstatus = readCSR (litBV $ encodeCSR MStatus)

  let mtVecBase = (mtVec `srlE` litBV 2) `sllE` litBV 2 -- ignore mode for now
      mcause = getMCause e

  assignPriv (litBV $ getPrivCode MPriv)
  assignCSR (litBV $ encodeCSR MTVal)   info -- TODO: actually thread info in here
  assignCSR (litBV $ encodeCSR MStatus) (mstatus `orE` sllE (zextE priv) (litBV 11))
  assignCSR (litBV $ encodeCSR MEPC)    pc
  assignCSR (litBV $ encodeCSR MCause)  (litBV mcause)

  assignPC mtVecBase
