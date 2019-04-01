{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module GRIFT.Semantics.Expand where

import Control.Lens ((^.))
import Data.BitVector.Sized
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

import GRIFT.Semantics
import GRIFT.Semantics.Utils
import GRIFT.Types

-- | Expand an 'AbbrevApp' into its unabbreviated version.
expandAbbrevApp :: AbbrevApp expr rv w -> expr rv w
expandAbbrevApp (SafeGPRApp wRepr ridE) = iteE
  (ridE `eqE` litBV 0)
  (litBV (bitVector' wRepr (0 :: Integer)))
  (stateExpr (LocApp (GPRApp wRepr ridE)))

-- | Expand an 'AbbrevStmt' into the statement it abbreviates.
expandAbbrevStmt :: KnownRV rv => AbbrevStmt expr rv -> Seq (Stmt expr rv)
expandAbbrevStmt (SafeGPRAssign ridE e) = Seq.singleton $
  BranchStmt (ridE `eqE` litBV 0)
  $> Seq.empty
  $> Seq.singleton (AssignStmt (GPRApp (exprWidth e) ridE) e)
expandAbbrevStmt (RaiseException code info) = flip (^.) semStmts $ getSemantics $ do
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
      -- mcause = getMCause e
      mcause = code

  assignPriv (litBV $ getPrivCode MPriv)
  assignCSR (litBV $ encodeCSR MTVal)   info -- TODO: actually thread info in here
  assignCSR (litBV $ encodeCSR MStatus) (mstatus `orE` sllE (zextE priv) (litBV 11))
  assignCSR (litBV $ encodeCSR MEPC)    pc
  assignCSR (litBV $ encodeCSR MCause)  (litBV (bvZext mcause))

  assignPC mtVecBase
