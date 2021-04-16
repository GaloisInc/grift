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
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module GRIFT.Semantics.Expand where

import Control.Lens ((^.))
import Data.Parameterized ( LeqProof(LeqProof), testLeq )
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import GHC.TypeLits ( type (<=) )

import GRIFT.BitVector.BVApp
import GRIFT.BitVector.BVFloatApp ( canonicalNaN32 )
import GRIFT.Semantics
import GRIFT.Semantics.Utils
import GRIFT.Types ( KnownRV, RVWidth, getSizedBV )

-- | Expand an 'AbbrevApp' into its unabbreviated version.
expandAbbrevApp ::
  forall rv w expr.
  BVExpr (expr rv) =>
  -- Num (expr rv 32) =>
  KnownRV rv =>
  AbbrevApp expr rv w -> expr rv w
expandAbbrevApp (SafeGPRApp wRepr ridE) = iteE
  (ridE `eqE` bvInteger 0)
  (bvInteger 0)
  (stateExpr (LocApp (GPRApp wRepr ridE)))
expandAbbrevApp (ReadCSRApp _ csr) =
  -- NOTE: the following code z-extends a 5-bit value, thus requiring '5 <= w'.
  -- However, adding this as a pre-condition is a bit of a nightmare, so let's see
  -- if we can get away with a dynamic check until we have a better solution.
  case testLeq (knownNat @5) (knownNat @w) of
    Just LeqProof ->
      withLeqTrans (knownNat @3) (knownNat @5) (knownNat @w) $
      cases
        [ (csr `eqE` bvExpr (encodeCSR FFlags)
          , let fcsr = rawReadCSR (bvExpr $ encodeCSR FCSR)
                flags = extractE' (knownNat @5) (knownNat @0) fcsr
            in zextEOrId flags
          )
        , (csr `eqE` bvExpr (encodeCSR FRm)
          , let fcsr = rawReadCSR (bvExpr $ encodeCSR FCSR)
                rm = extractE' (knownNat @3) (knownNat @5) fcsr
            in zextEOrId rm
          )
        ]
        (rawReadCSR csr)
    Nothing -> error "TODO"
expandAbbrevApp (NanBox32App _ e) = (bvExpr (getSizedBV (-1)) :: expr rv 32) `concatE` e
expandAbbrevApp (UnNanBox32App _ e) = iteE
  (extractE' (knownNat @32) (knownNat @32) e `eqE` bvInteger 0xFFFFFFFF)
  (extractE (knownNat @0) e)
  canonicalNaN32

-- | Expand an 'AbbrevStmt' into the statement it abbreviates.
expandAbbrevStmt ::
  forall expr rv w.
  BVExpr (expr rv) =>
  KnownRV rv =>
  (w ~ RVWidth rv, 32 <= w) =>
  AbbrevStmt expr rv -> Seq (Stmt expr rv)
expandAbbrevStmt (SafeGPRAssign ridE e) = Seq.singleton $
  BranchStmt (ridE `eqE` bvInteger 0)
  $> Seq.empty
  $> Seq.singleton (AssignStmt (GPRApp (exprWidth e) ridE) e)
expandAbbrevStmt (RaiseException code info) = flip (^.) semStmts $ getSemantics $
  withLeqTrans (knownNat @1) (knownNat @32) (knownNat @w) $
  withLeqTrans (knownNat @3) (knownNat @32) (knownNat @w) $
  withLeqTrans (knownNat @13) (knownNat @32) (knownNat @w) $
  do
  -- Exception handling TODO:
  -- - For interrupts, PC should be incremented.
  -- - mtval should be an argument to this function based on the exception
  -- - MIE and MPIE need to be set appropriately, but we are not worrying about this
  --   for now
  -- - MPP (don't need this until we have other privilege modes)
  -- - We are assuming we do not have supervisor mode

  let pc      = readPC
  let priv    = readPriv
  let mtVec   = rawReadCSR (bvExpr $ encodeCSR MTVec)
  let mstatus = rawReadCSR (bvExpr $ encodeCSR MStatus)

  let mtVecBase = (mtVec `srlE` bvInteger 2) `sllE` bvInteger 2 -- ignore mode for now
      -- mcause = getMCause e
      mcause = code

  assignPriv (bvExpr $ getPrivCode MPriv)
  assignCSR (bvExpr $ encodeCSR MTVal)   info -- TODO: actually thread info in here
  assignCSR (bvExpr $ encodeCSR MStatus) (mstatus `orE` sllE (zextE priv) (bvInteger 11))
  assignCSR (bvExpr $ encodeCSR MEPC)    pc
  assignCSR (bvExpr $ encodeCSR MCause)  (bvExpr (zext knownNat mcause))

  assignPC mtVecBase
expandAbbrevStmt (WriteCSR csr val) = flip (^.) semStmts $ getSemantics $ branches
  [ (csr `eqE` bvExpr (encodeCSR FFlags)
  , do let val' = extractE' (knownNat @5) (knownNat @0) val
           fcsr = rawReadCSR (bvExpr $ encodeCSR FCSR)
           writeVal = extractE' (knownNat @27) (knownNat @5) fcsr `concatE` val'
       assignCSR (bvExpr $ encodeCSR FCSR) (zextEOrId writeVal))
  , (csr `eqE` bvExpr (encodeCSR FRm)
  , do let val' = extractE' (knownNat @3) (knownNat @0) val
           fcsr = rawReadCSR (bvExpr $ encodeCSR FCSR)
           writeVal = extractE' (knownNat @24) (knownNat @8) fcsr `concatE`
                      val' `concatE`
                      extractE' (knownNat @5) (knownNat @0) fcsr
       assignCSR (bvExpr $ encodeCSR FCSR) (zextEOrId writeVal))
  , (csr `eqE` bvExpr (encodeCSR FCSR)
    , do let writeVal = val `andE` bvInteger 0xFF
         assignCSR (bvExpr $ encodeCSR FCSR) (zextEOrId writeVal))
  ]
  (assignCSR csr val)
