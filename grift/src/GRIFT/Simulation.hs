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

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

{-|
Module      : GRIFT.Simulation
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

A type class for simulating RISC-V code.
-}

module GRIFT.Simulation
  ( -- * State monad
    RVStateM(..)
  , evalLocApp
  , evalStateApp
  , evalPureStateExpr
  , evalInstExpr
  , Loc(..)
  , Assignment(..)
  , execAssignment
  , execSemantics
  , stepRV
  , stepRVLog
  , runRV
  , runRVLog
  ) where

import Control.Lens ( (^.) )
import Data.BitVector.Sized hiding ( concat )
import Data.Foldable
import Data.Parameterized
import Data.Parameterized.List
import Data.Traversable
import qualified GHC.TypeLits as T
import Prelude hiding ( (!!), concat )

import GRIFT.BitVector.BVApp
import GRIFT.BitVector.BVFloatApp
import GRIFT.Decode
import GRIFT.InstructionSet
import GRIFT.InstructionSet.Known
import GRIFT.Semantics hiding ( concat )
import GRIFT.Semantics.Expand
import GRIFT.Semantics.Utils
import GRIFT.Types

-- TODO: Should getMem/setMem return a possibly exceptional value, so that we can
-- handle the actual exception handling in this module rather than on the
-- implementation side? How would that work? Punting on this until we need to go
-- deeper with the exception handling stuff.
-- | State monad for simulating RISC-V code
class Monad m => RVStateM m (rv :: RV) | m -> rv where
  -- | Get the RISC-V configuration.
  getRV :: m (RVRepr rv)

  -- | Get the current PC.
  getPC   :: m (BV (RVWidth rv))
  -- | Get the value of a register. This function shouldn't ever be called with an
  -- argument of 0, so there is no need to hardwire it to 0 in an instance of this
  -- class.
  getGPR  :: BV 5 -> m (BV (RVWidth rv))
  -- | Read some number of bytes from memory.
  getMem  :: NatRepr bytes -> BV (RVWidth rv) -> m (BV (8 T.* bytes))
  -- | Get the value of a CSR.
  getCSR  :: BV 12 -> m (BV (RVWidth rv))
  -- | Get the current privilege level.
  getPriv :: m (BV 2)
  -- | Get the value of a floating point register.
  getFPR :: FExt << rv => BV 5 -> m (BV (RVFloatWidth rv))

  -- | Set the PC.
  setPC   :: BV (RVWidth rv) -> m ()
  -- | Write to a register.
  setGPR  :: BV 5 -> BV (RVWidth rv) -> m ()
  -- | Write a single byte to memory.
  setMem  :: NatRepr bytes -> BV (RVWidth rv) -> BV (8 T.* bytes) -> m ()
  -- | Write to a CSR.
  setCSR  :: BV 12 -> BV (RVWidth rv) -> m ()
  -- | Set the privilege level.
  setPriv :: BV 2 -> m ()
  -- | Set the value of a floating point register.
  setFPR :: FExt << rv => BV 5 -> BV (RVFloatWidth rv) -> m ()

  -- | Condition for halting simulation.
  isHalted :: m Bool
  -- | Log the execution of a particular instruction.
  logInstruction :: InstructionSet rv -> Instruction rv fmt -> Integer -> m ()

-- TODO: Is there some way to wher ein (FExt << rv) => RVFStateM m rv) to the below signature?
-- | Evaluate a 'LocApp', given an 'RVStateM' implementation.
evalLocApp :: forall m expr rv w . (RVStateM m rv)
           => (forall w' . expr w' -> m (BV w')) -- ^ evaluator for internal expressions
           -> LocApp expr rv w
           -> m (BV w)
evalLocApp _ (PCApp _) = getPC
evalLocApp eval (GPRApp _ ridE) = eval ridE >>= getGPR
evalLocApp eval (FPRApp _ ridE) = eval ridE >>= getFPR
evalLocApp eval (MemApp bytes addrE) = eval addrE >>= getMem bytes
-- TODO: When we do SMP, implement memory reservations.
evalLocApp _ (ResApp _) = return (unSized 1)
evalLocApp eval (CSRApp _ csrE) = eval csrE >>= getCSR
evalLocApp _ PrivApp = getPriv

-- | Evaluate a 'StateApp', given an 'RVStateM' implementation.
evalStateApp :: forall m expr rv w . (RVStateM m rv)
              => (forall w'. expr w' -> m (BV w')) -- ^ evaluator for internal expressions
              -> StateApp expr rv w
              -> m (BV w)
evalStateApp eval (LocApp e) = evalLocApp eval e
evalStateApp eval (AppExpr e) = evalBVAppM eval e
evalStateApp eval (FloatAppExpr e) = evalBVFloatAppM eval e

-- | Evaluate a 'PureStateExpr', given an 'RVStateM' implementation.
evalPureStateExpr :: forall m rv w . (RVStateM m rv, KnownRV rv) => PureStateExpr rv w -> m (BV w)
evalPureStateExpr (PureStateLitBV _ bv) = return bv
evalPureStateExpr (PureStateApp e) = evalStateApp evalPureStateExpr e
evalPureStateExpr (PureAbbrevApp abbrevApp) = evalPureStateExpr (expandAbbrevApp abbrevApp)

-- | Evaluate an 'InstExpr', given an 'RVStateM' implementation and the instruction context.
evalInstExpr :: forall m rv fmt w . (RVStateM m rv, KnownRV rv)
             => InstructionSet rv
             -> Instruction rv fmt -- ^ instruction
             -> Integer            -- ^ Instruction width (in bytes)
             -> InstExpr fmt rv w  -- ^ Expression to be evaluated
             -> m (BV w)
evalInstExpr _ _ _ (InstLitBV _ bv) = return bv
evalInstExpr iset inst ib (InstAbbrevApp abbrevApp) =
  evalInstExpr iset inst ib (expandAbbrevApp abbrevApp)
evalInstExpr _ (Inst _ (Operands _ operands)) _ (OperandExpr _ (OperandID p)) =
  return (unSized (operands !! p))
evalInstExpr _ _ ib (InstBytes _) = do
  rv <- getRV
  return $ withRV rv $ mkBV' ib
evalInstExpr iset inst _ (InstWord _) =
  -- TODO: this is *very* awkward, because we z-extend, which requires '32 <=
  -- w', but adding the constraint makes it akward when 'evalInstExpr' is passed
  -- to 'evalStateApp' in the next case.
  case testLeq (knownNat @32) (knownNat @w) of
    Just LeqProof ->
      do
        rv <- getRV
        return $ withRV rv $ zextOrId $ unSized $ encode iset inst
    Nothing -> error "TODO"
evalInstExpr iset inst ib (InstStateApp e) = evalStateApp (evalInstExpr iset inst ib) e

-- | This type represents a concrete component of the global state, after all
-- expressions have been evaluated. It is in direct correspondence with the 'LocApp'
-- type from 'GRIFT.Semantics'. Unlike that type, however, this will never appear on
-- the right-hand side of an assignment, only the left-hand side.
data Loc rv w where
  PC :: Loc rv (RVWidth rv)
  GPR :: BV 5 -> Loc rv (RVWidth rv)
  FPR :: FExt << rv => BV 5 -> Loc rv (RVFloatWidth rv)
  Mem :: NatRepr bytes -> BV (RVWidth rv) -> Loc rv (8 T.* bytes)
  Res :: BV (RVWidth rv) -> Loc rv 1
  CSR :: BV 12 -> Loc rv (RVWidth rv)
  Priv :: Loc rv 2

-- | This type represents a concrete assignment statement, where the left-hand side
-- is a known location and the right-hand side is a known BV value.
data Assignment (rv :: RV) where
  Assignment :: Loc rv w -> BV w -> Assignment rv

-- | Convert a 'Stmt' into an 'Assignment' by evaluating its right-hand sides.
buildAssignment :: (RVStateM m rv, KnownRV rv, w ~ RVWidth rv, 32 <= w, BVExpr (expr rv))
                => (forall w'. expr rv w' -> m (BV w'))
                -> Stmt expr rv
                -> m [Assignment rv]
buildAssignment eval (AssignStmt (PCApp _) pcE) = do
  pcVal <- eval pcE
  return [Assignment PC pcVal]
buildAssignment eval (AssignStmt (GPRApp _ ridE) e) = do
  rid  <- eval ridE
  eVal <- eval e
  return [Assignment (GPR rid) eVal]
buildAssignment eval (AssignStmt (FPRApp _ ridE) e) = do
  rid  <- eval ridE
  eVal <- eval e
  return [Assignment (FPR rid) eVal]
buildAssignment eval (AssignStmt (MemApp bytes addrE) e) = do
  addr <- eval addrE
  eVal <- eval e
  return [Assignment (Mem bytes addr) eVal]
buildAssignment eval (AssignStmt (ResApp addrE) e) = do
  addr <- eval addrE
  eVal <- eval e
  return [Assignment (Res addr) eVal]
buildAssignment eval (AssignStmt (CSRApp _ csrE) e) = do
  csr  <- eval csrE
  eVal <- eval e
  return [Assignment (CSR csr) eVal]
buildAssignment eval (AssignStmt PrivApp privE) = do
  privVal <- eval privE
  return [Assignment Priv privVal]
buildAssignment eval (AbbrevStmt abbrevStmt) = concat <$> traverse (buildAssignment eval) (expandAbbrevStmt abbrevStmt)
buildAssignment eval (BranchStmt condE tStmts fStmts) = do
  condVal <- eval condE
  tAssignments <- traverse (buildAssignment eval) tStmts
  fAssignments <- traverse (buildAssignment eval) fStmts
  case condVal of
    BV 0 -> return $ concat fAssignments
    _ -> return $ concat tAssignments

-- | Execute an assignment.
execAssignment :: RVStateM m rv => Assignment rv -> m ()
execAssignment (Assignment PC val) = setPC val
execAssignment (Assignment (GPR rid) val) = setGPR rid val
execAssignment (Assignment (FPR rid) val) = setFPR rid val
execAssignment (Assignment (Mem bytes addr) val) = setMem bytes addr val
-- TODO: When we do SMP, implement memory reservations.
execAssignment (Assignment (Res _) _) = return ()
execAssignment (Assignment (CSR csr) val) = do
  setCSR csr val
execAssignment (Assignment Priv val) = setPriv val

-- | Execute a formula, given an 'RVStateM' implementation.
execSemantics :: forall m expr rv w. (RVStateM m rv, KnownRV rv, w ~ RVWidth rv, 32 <= w, BVExpr (expr rv))
             => (forall w'. expr rv w' -> m (BV w'))
             -> Semantics expr rv
             -> m ()
execSemantics eval f = do
  assignments <- concat <$> traverse (buildAssignment eval) (f ^. semStmts)
  traverse_ execAssignment assignments

-- | 'select' over 'SizedBV's
sselect :: forall w w' idx.
  T.KnownNat w =>
  T.KnownNat w' =>
  idx + w' <= w =>
  NatRepr idx -> SizedBV w -> SizedBV w'
sselect idx (SizedBV _ bv) =
  SizedBV knownNat (select idx knownNat bv)

-- | Fetch, decode, and execute a single instruction.
stepRV :: forall m rv w. (RVStateM m rv, KnownRV rv, w ~ RVWidth rv, 32 <= w)
       => InstructionSet rv
       -> m ()
stepRV iset = do
  rv <- getRV
  withRV rv $ do
    -- Fetch
    pcVal  <- getPC
    instBV <- SizedBV knownNat <$> getMem (knownNat @4) pcVal

    -- Decode
    (iw, Some inst@(Inst opcode _)) <- do
      case rv of
        RVRepr _ (ExtensionsRepr _ _ _ _ CYesRepr) -> do
          let cinst = decodeC rv (sselect (knownNat @0) instBV)
          case cinst of
            Just i -> return (2, i)
            _ -> return (4, decode iset instBV)
        _ -> return (4, decode iset instBV)

    -- Execute
    execSemantics (evalInstExpr iset inst iw) (getInstSemantics $ semanticsFromOpcode iset opcode)

    -- Record cycle count
    execSemantics evalPureStateExpr $ getSemantics $ do
      let minstret = rawReadCSR (bvExpr $ encodeCSR MInstRet)
      assignCSR (bvExpr $ encodeCSR MInstRet) (minstret `addE` bvInteger 1)

-- | Like stepRV, but also log the instruction.
stepRVLog :: forall m rv w. (RVStateM m rv, KnownRV rv, w ~ RVWidth rv, 32 <= w)
          => InstructionSet rv
          -> m ()
stepRVLog iset = do
  rv <- getRV
  withRV rv $ do
    -- Fetch
    pcVal  <- getPC
    instBV <- SizedBV knownNat <$> getMem (knownNat @4) pcVal

    -- Decode
    (iw, Some inst@(Inst opcode _)) <- do
      case rv of
        RVRepr _ (ExtensionsRepr _ _ _ _ CYesRepr) -> do
          let cinst = decodeC rv (sselect (knownNat @0) instBV)
          case cinst of
            Just i -> return (2, i)
            _ -> return (4, decode iset instBV)
        _ -> return (4, decode iset instBV)

    -- Log instruction BEFORE execution
    logInstruction iset inst iw

    -- Execute
    execSemantics (evalInstExpr iset inst iw) (getInstSemantics $ semanticsFromOpcode iset opcode)

    -- Record cycle count
    execSemantics evalPureStateExpr $ getSemantics $ do
      let minstret = rawReadCSR (bvExpr $ encodeCSR MInstRet)
      assignCSR (bvExpr $ encodeCSR MInstRet) (minstret `addE` bvInteger 1)

runRV' ::
  (RVStateM m rv, KnownRV rv, w ~ RVWidth rv, 32 <= w) =>
  InstructionSet rv -> Int -> Int -> m Int
runRV' _ currSteps maxSteps | currSteps >= maxSteps = return currSteps
runRV' iset currSteps maxSteps = do
  halted <- isHalted
  if halted
    then return currSteps
    else stepRV iset >> runRV' iset (currSteps+1) maxSteps

-- | Run for a given number of steps.
runRV ::
  (RVStateM m rv, KnownRV rv, w ~ RVWidth rv, 32 <= w) =>
  Int -> m Int
runRV steps = do
  rv <- getRV
  withRV rv $ runRV' (knownISetWithRepr rv) 0 steps

runRVLog' ::
  (RVStateM m rv, KnownRV rv, w ~ RVWidth rv, 32 <= w) =>
  InstructionSet rv -> Int -> Int -> m Int
runRVLog' _ currSteps maxSteps | currSteps >= maxSteps = return currSteps
runRVLog' iset currSteps maxSteps = do
  halted <- isHalted
  if halted
    then return currSteps
    else stepRVLog iset >> runRVLog' iset (currSteps+1) maxSteps

-- | Like runRV, but log each instruction.
runRVLog ::
  (RVStateM m rv, KnownRV rv, w ~ RVWidth rv, 32 <= w) =>
  Int -> m Int
runRVLog steps = do
  rv <- getRV
  withRV rv $ runRVLog' (knownISetWithRepr rv) 0 steps
