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
import Data.BitVector.Sized
import Data.BitVector.Sized.App
import Data.BitVector.Sized.Float.App
import Data.Foldable
import Data.Parameterized
import Data.Parameterized.List
import Data.Traversable
import qualified GHC.TypeLits as T
import Prelude hiding ((!!))

import GRIFT.Decode
import GRIFT.InstructionSet
import GRIFT.InstructionSet.Known
import GRIFT.InstructionSet.Utils
import GRIFT.Semantics
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
  getPC   :: m (BitVector (RVWidth rv))
  -- | Get the value of a register. This function shouldn't ever be called with an
  -- argument of 0, so there is no need to hardwire it to 0 in an instance of this
  -- class.
  getGPR  :: BitVector 5 -> m (BitVector (RVWidth rv))
  -- | Read some number of bytes from memory.
  getMem  :: NatRepr bytes -> BitVector (RVWidth rv) -> m (BitVector (8 T.* bytes))
  -- | Get the value of a CSR.
  getCSR  :: BitVector 12 -> m (BitVector (RVWidth rv))
  -- | Get the current privilege level.
  getPriv :: m (BitVector 2)
  -- | Get the value of a floating point register.
  getFPR :: FExt << rv => BitVector 5 -> m (BitVector (RVFloatWidth rv))

  -- | Set the PC.
  setPC   :: BitVector (RVWidth rv) -> m ()
  -- | Write to a register.
  setGPR  :: BitVector 5 -> BitVector (RVWidth rv) -> m ()
  -- | Write a single byte to memory.
  setMem  :: NatRepr bytes -> BitVector (RVWidth rv) -> BitVector (8 T.* bytes) -> m ()
  -- | Write to a CSR.
  setCSR  :: BitVector 12 -> BitVector (RVWidth rv) -> m ()
  -- | Set the privilege level.
  setPriv :: BitVector 2 -> m ()
  -- | Set the value of a floating point register.
  setFPR :: FExt << rv => BitVector 5 -> BitVector (RVFloatWidth rv) -> m ()

  -- | Condition for halting simulation.
  isHalted :: m Bool
  -- | Log the execution of a particular instruction.
  logInstruction :: InstructionSet rv -> Instruction rv fmt -> Integer -> m ()

-- TODO: Is there some way to wher ein (FExt << rv) => RVFStateM m rv) to the below signature?
-- | Evaluate a 'LocApp', given an 'RVStateM' implementation.
evalLocApp :: forall m expr rv w . (RVStateM m rv)
           => (forall w' . expr w' -> m (BitVector w')) -- ^ evaluator for internal expressions
           -> LocApp expr rv w
           -> m (BitVector w)
evalLocApp _ (PCApp _) = getPC
evalLocApp eval (GPRApp _ ridE) = eval ridE >>= getGPR
evalLocApp eval (FPRApp _ ridE) = eval ridE >>= getFPR
evalLocApp eval (MemApp bytes addrE) = eval addrE >>= getMem bytes
-- TODO: When we do SMP, implement memory reservations.
evalLocApp _ (ResApp _) = return 1
evalLocApp eval (CSRApp _ csrE) = eval csrE >>= getCSR
evalLocApp _ PrivApp = getPriv

-- | Evaluate a 'StateApp', given an 'RVStateM' implementation.
evalStateApp :: forall m expr rv w . (RVStateM m rv)
              => (forall w' . expr w' -> m (BitVector w')) -- ^ evaluator for internal expressions
              -> StateApp expr rv w
              -> m (BitVector w)
evalStateApp eval (LocApp e) = evalLocApp eval e
evalStateApp eval (AppExpr e) = evalBVAppM eval e
evalStateApp eval (FloatAppExpr e) = evalBVFloatAppM eval e

-- | Evaluate a 'PureStateExpr', given an 'RVStateM' implementation.
evalPureStateExpr :: forall m rv w . (RVStateM m rv) => PureStateExpr rv w -> m (BitVector w)
evalPureStateExpr (PureStateLitBV bv) = return bv
evalPureStateExpr (PureStateApp e) = evalStateApp evalPureStateExpr e

-- | Evaluate an 'InstExpr', given an 'RVStateM' implementation and the instruction context.
evalInstExpr :: forall m rv fmt w . RVStateM m rv
             => InstructionSet rv
             -> Instruction rv fmt -- ^ instruction
             -> Integer            -- ^ Instruction width (in bytes)
             -> InstExpr fmt rv w  -- ^ Expression to be evaluated
             -> m (BitVector w)
evalInstExpr _ _ _ (InstLitBV bv) = return bv
evalInstExpr _ (Inst _ (Operands _ operands)) _ (OperandExpr _ (OperandID p)) = return (operands !! p)
evalInstExpr _ _ ib (InstBytes _) = do
  rv <- getRV
  return $ withRVWidth rv $ bitVector ib
evalInstExpr iset inst _ (InstWord _) = do
  rv <- getRV
  return $ (withRVWidth rv $ bvZext $ encode iset inst)
evalInstExpr iset inst ib (InstStateApp e) = evalStateApp (evalInstExpr iset inst ib) e

-- | This type represents a concrete component of the global state, after all
-- expressions have been evaluated. It is in direct correspondence with the 'LocApp'
-- type from 'GRIFT.Semantics'. Unlike that type, however, this will never appear on
-- the right-hand side of an assignment, only the left-hand side.
data Loc rv w where
  PC :: Loc rv (RVWidth rv)
  GPR :: BitVector 5 -> Loc rv (RVWidth rv)
  FPR :: FExt << rv => BitVector 5 -> Loc rv (RVFloatWidth rv)
  Mem :: NatRepr bytes -> BitVector (RVWidth rv) -> Loc rv (8 T.* bytes)
  Res :: BitVector (RVWidth rv) -> Loc rv 1
  CSR :: BitVector 12 -> Loc rv (RVWidth rv)
  Priv :: Loc rv 2

-- | This type represents a concrete assignment statement, where the left-hand side
-- is a known location and the right-hand side is a known BitVector value.
data Assignment (rv :: RV) where
  Assignment :: Loc rv w -> BitVector w -> Assignment rv

-- | Convert a 'Stmt' into an 'Assignment' by evaluating its right-hand sides.
buildAssignment :: (RVStateM m rv)
                     => (forall w . expr w -> m (BitVector w))
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
buildAssignment eval (BranchStmt condE tStmts fStmts) = do
  condVal <- eval condE
  tAssignments <- traverse (buildAssignment eval) tStmts
  fAssignments <- traverse (buildAssignment eval) fStmts
  case condVal of
    0 -> return $ concat fAssignments
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
execSemantics :: forall m expr rv . (RVStateM m rv)
             => (forall w . expr w -> m (BitVector w))
             -> Semantics expr rv
             -> m ()
execSemantics eval f = do
  assignments <- concat <$> traverse (buildAssignment eval) (f ^. semStmts)
  traverse_ execAssignment assignments

-- | Fetch, decode, and execute a single instruction.
stepRV :: forall m rv . (RVStateM m rv)
       => InstructionSet rv
       -> m ()
stepRV iset = do
  rv <- getRV
  withRVWidth rv $ do
    -- Fetch
    pcVal  <- getPC
    instBV <- getMem (knownNat @4) pcVal

    -- Decode
    (iw, Some inst@(Inst opcode _)) <- do
      case rv of
        RVRepr _ (ExtensionsRepr _ _ _ _ CYesRepr) -> do
          let cinst = decodeC rv (bvExtract 0 instBV)
          case cinst of
            Just i -> return (2, i)
            _ -> return $ (4, decode iset instBV)
        _ -> return $ (4, decode iset instBV)

    -- Execute
    execSemantics (evalInstExpr iset inst iw) (getInstSemantics $ semanticsFromOpcode iset opcode)

    -- Record cycle count
    execSemantics evalPureStateExpr $ getSemantics $ do
      let minstret = rawReadCSR (litBV $ encodeCSR MInstRet)
      assignCSR (litBV $ encodeCSR MInstRet) (minstret `addE` litBV 1)

-- | Like stepRV, but also log the instruction.
stepRVLog :: forall m rv . (RVStateM m rv)
          => InstructionSet rv
          -> m ()
stepRVLog iset = do
  rv <- getRV
  withRVWidth rv $ do
    -- Fetch
    pcVal  <- getPC
    instBV <- getMem (knownNat @4) pcVal

    -- Decode
    (iw, Some inst@(Inst opcode _)) <- do
      case rv of
        RVRepr _ (ExtensionsRepr _ _ _ _ CYesRepr) -> do
          let cinst = decodeC rv (bvExtract 0 instBV)
          case cinst of
            Just i -> return (2, i)
            _ -> return $ (4, decode iset instBV)
        _ -> return $ (4, decode iset instBV)

    -- Log instruction BEFORE execution
    logInstruction iset inst iw

    -- Execute
    execSemantics (evalInstExpr iset inst iw) (getInstSemantics $ semanticsFromOpcode iset opcode)

    -- Record cycle count
    execSemantics evalPureStateExpr $ getSemantics $ do
      let minstret = rawReadCSR (litBV $ encodeCSR MInstRet)
      assignCSR (litBV $ encodeCSR MInstRet) (minstret `addE` litBV 1)

runRV' :: forall m rv . (RVStateM m rv, KnownRVWidth rv) => InstructionSet rv -> Int -> Int -> m Int
runRV' _ currSteps maxSteps | currSteps >= maxSteps = return currSteps
runRV' iset currSteps maxSteps = do
  halted <- isHalted
  case halted of
    True  -> return currSteps
    False -> stepRV iset >> runRV' iset (currSteps+1) maxSteps

-- | Run for a given number of steps.
runRV :: forall m rv . (RVStateM m rv) => Int -> m Int
runRV steps = do
  rv <- getRV
  withRVWidth rv $ runRV' (knownISetWithRepr rv) 0 steps

runRVLog' :: forall m rv . (RVStateM m rv, KnownRVWidth rv) => InstructionSet rv -> Int -> Int -> m Int
runRVLog' _ currSteps maxSteps | currSteps >= maxSteps = return currSteps
runRVLog' iset currSteps maxSteps = do
  halted <- isHalted
  case halted of
    True  -> return currSteps
    False -> stepRVLog iset >> runRVLog' iset (currSteps+1) maxSteps

-- | Like runRV, but log each instruction.
runRVLog :: forall m rv . (RVStateM m rv) => Int -> m Int
runRVLog steps = do
  rv <- getRV
  withRVWidth rv $ runRVLog' (knownISetWithRepr rv) 0 steps
