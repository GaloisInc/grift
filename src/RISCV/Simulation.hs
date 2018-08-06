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
Module      : RISCV.Simulation
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

A type class for simulating RISC-V code.
-}

module RISCV.Simulation
  ( -- * State monad
    RVStateM(..)
  , evalPureStateExpr
  , evalInstExpr
  , Loc(..)
  , Assignment(..)
  , execAssignment
  , execFormula
  , runRV
  , getTests
  ) where

import Control.Lens ( (^.) )
import Data.BitVector.Sized
import Data.BitVector.Sized.App
import Data.Foldable
import Data.List (nub)
import Data.Parameterized
import Data.Parameterized.List
import Data.Traversable
import Data.Sequence (Seq)
import Prelude hiding ((!!))

import RISCV.Decode
import RISCV.Extensions
import RISCV.InstructionSet
import RISCV.Semantics
import RISCV.Semantics.Exceptions
import RISCV.Types

-- TODO: Should getMem/setMem return a possibly exceptional value, so that we can
-- handle the actual exception handling in this module rather than on the
-- implementation side? How would that work? Punting on this until we need to go
-- deeper with the exception handling stuff.
-- | State monad for simulating RISC-V code
class (Monad m) => RVStateM m (arch :: BaseArch) (exts :: Extensions) | m -> arch, m -> exts where
  -- | Get the current PC.
  getPC   :: m (BitVector (ArchWidth arch))
  -- | Get the value of a register. This function shouldn't ever be called with an
  -- argument of 0, so there is no need to hardwire it to 0 in an implementation.
  getReg  :: BitVector 5 -> m (BitVector (ArchWidth arch))
  -- | Read some number of bytes from memory.
  getMem  :: NatRepr bytes -> BitVector (ArchWidth arch) -> m (BitVector (8*bytes))
  -- | Get the value of a CSR.
  getCSR  :: BitVector 12 -> m (BitVector (ArchWidth arch))
  -- | Get the current privilege level.
  getPriv :: m (BitVector 2)

  -- | Set the PC.
  setPC   :: BitVector (ArchWidth arch) -> m ()
  -- | Write to a register.
  setReg  :: BitVector 5 -> BitVector (ArchWidth arch) -> m ()
  -- | Write a single byte to memory.
  setMem  :: NatRepr bytes -> BitVector (ArchWidth arch) -> BitVector (8*bytes) -> m ()
  -- | Write to a CSR.
  setCSR  :: BitVector 12 -> BitVector (ArchWidth arch) -> m ()
  -- | Set the privilege level.
  setPriv :: BitVector 2 -> m ()

  -- | Log the execution of a particular instruction.
  logInstruction :: InstructionSet arch exts -> Instruction arch exts fmt -> m ()

-- | Evaluate a 'LocExpr', given an 'RVStateM' implementation.
evalLocExpr :: forall m expr arch exts w
               . (RVStateM m arch exts, KnownArch arch)
            => (forall w' . expr w' -> m (BitVector w')) -- ^ evaluator for internal expressions
            -> LocExpr expr arch exts w
            -> m (BitVector w)
evalLocExpr _ PCExpr = getPC
evalLocExpr eval (RegExpr ridE) = eval ridE >>= getReg
evalLocExpr eval (MemExpr bytes addrE) = eval addrE >>= getMem bytes
-- TODO: When we do SMP, implement memory reservations.
evalLocExpr _ (ResExpr _) = return 1
evalLocExpr eval (CSRExpr csrE) = eval csrE >>= getCSR
evalLocExpr _ PrivExpr = getPriv

-- | Evaluate a 'StateExpr', given an 'RVStateM' implementation.
evalStateExpr :: forall m expr arch exts w
                 . (RVStateM m arch exts, KnownArch arch)
              => (forall w' . expr w' -> m (BitVector w'))
                 -- ^ evaluator for internal expressions
              -> StateExpr expr arch exts w
              -> m (BitVector w)
evalStateExpr eval (LocExpr e) = evalLocExpr eval e
evalStateExpr eval (AppExpr e) = evalBVAppM eval e

-- | Evaluate a 'PureStateExpr', given an 'RVStateM' implementation.
evalPureStateExpr :: forall m arch exts w
                 . (RVStateM m arch exts, KnownArch arch)
              => PureStateExpr arch exts w
              -> m (BitVector w)
evalPureStateExpr (PureStateExpr e) = evalStateExpr evalPureStateExpr e

-- | Evaluate an 'InstExpr', given an 'RVStateM' implementation and the instruction context.
evalInstExpr :: forall m arch exts fmt w
            . (RVStateM m arch exts, KnownArch arch)
             => InstructionSet arch exts
             -> Instruction arch exts fmt -- ^ instruction
             -> Integer          -- ^ Instruction width (in bytes)
             -> InstExpr fmt arch exts w  -- ^ Expression to be evaluated
             -> m (BitVector w)
evalInstExpr _ (Inst _ (Operands _ operands)) _ (OperandExpr (OperandID p)) = return (operands !! p)
evalInstExpr _ _ ib InstBytes = return $ bitVector ib
evalInstExpr iset inst _ InstWord = return $ (bvZext $ encode iset inst)
evalInstExpr iset inst ib (InstStateExpr e) = evalStateExpr (evalInstExpr iset inst ib) e

-- | This type represents a concrete component of the global state, after all
-- expressions have been evaluated. It is in direct correspondence with the 'LocExpr'
-- type from RISCV.Semantics. Unlike that type, however, this will never appear on
-- the right-hand side of an assignment, only the left-hand side.
data Loc arch exts w where
  PC :: Loc arch exts (ArchWidth arch)
  Reg :: BitVector 5 -> Loc arch exts (ArchWidth arch)
  Mem :: NatRepr bytes -> BitVector (ArchWidth arch) -> Loc arch exts (8*bytes)
  Res :: BitVector (ArchWidth arch) -> Loc arch exts 1
  CSR :: BitVector 12 -> Loc arch exts (ArchWidth arch)
  Priv :: Loc arch exts 2

-- | This type represents a concrete assignment statement, where the left-hand side
-- is a known location and the right-hand side is a known BitVector value. Because we
-- are still abstracting out the notion of throwing an exception, we also have a
-- constructor for that; however, this will be removed once we wire exception
-- handling into the semantics themselves.
data Assignment (arch :: BaseArch) (exts :: Extensions) where
  Assignment :: Loc arch exts w -> BitVector w -> Assignment arch exts
  Branch :: BitVector 1 -> Seq (Assignment arch exts) -> Seq (Assignment arch exts) -> Assignment arch exts

-- | Convert a 'Stmt' into an 'Assignment' by evaluating its right-hand sides.
buildAssignment :: (RVStateM m arch exts, KnownArch arch)
                     => (forall w . expr w -> m (BitVector w))
                     -> Stmt expr arch exts
                     -> m (Assignment arch exts)
buildAssignment eval (AssignStmt PCExpr pcE) = do
  pcVal <- eval pcE
  return (Assignment PC pcVal)
buildAssignment eval (AssignStmt (RegExpr ridE) e) = do
  rid  <- eval ridE
  eVal <- eval e
  return (Assignment (Reg rid) eVal)
buildAssignment eval (AssignStmt (MemExpr bytes addrE) e) = do
  addr <- eval addrE
  eVal <- eval e
  return (Assignment (Mem bytes addr) eVal)
buildAssignment eval (AssignStmt (ResExpr addrE) e) = do
  addr <- eval addrE
  eVal <- eval e
  return (Assignment (Res addr) eVal)
buildAssignment eval (AssignStmt (CSRExpr csrE) e) = do
  csr  <- eval csrE
  eVal <- eval e
  return (Assignment (CSR csr) eVal)
buildAssignment eval (AssignStmt PrivExpr privE) = do
  privVal <- eval privE
  return (Assignment Priv privVal)
buildAssignment eval (BranchStmt condE tStmts fStmts) = do
  condVal <- eval condE
  tAssignments <- traverse (buildAssignment eval) tStmts
  fAssignments <- traverse (buildAssignment eval) fStmts
  return (Branch condVal tAssignments fAssignments)

-- | Execute an assignment.
execAssignment :: (RVStateM m arch exts, KnownArch arch) => Assignment arch exts -> m ()
execAssignment (Assignment PC val) = setPC val
execAssignment (Assignment (Reg rid) val) = setReg rid val
execAssignment (Assignment (Mem bytes addr) val) = setMem bytes addr val
-- TODO: When we do SMP, implement memory reservations.
execAssignment (Assignment (Res _) _) = return ()
execAssignment (Assignment (CSR csr) val) = do
  setCSR csr val
execAssignment (Assignment Priv val) = setPriv val
execAssignment (Branch condVal tAssignments fAssignments) =
  case condVal of
    1 -> traverse_ execAssignment tAssignments
    _ -> traverse_ execAssignment fAssignments

-- | Execute a formula, given an 'RVStateM' implementation.
execFormula :: forall m expr arch exts . (RVStateM m arch exts, KnownArch arch)
             => (forall w . expr w -> m (BitVector w))
             -> Formula expr arch exts
             -> m ()
execFormula eval f = do
  assignments <- traverse (buildAssignment eval) (f ^. fDefs)
  traverse_ execAssignment assignments

-- | Fetch, decode, and execute a single instruction.
stepRV :: forall m arch exts
          . (RVStateM m arch exts, KnownArch arch, KnownExtensions exts)
       => InstructionSet arch exts
       -> m ()
stepRV iset = do
  -- Fetch
  pcVal  <- getPC
  instBV <- getMem (knownNat @4) pcVal

  -- Decode
  -- TODO: When we add compression ('C' extension), we'll need to modify this code.
  Some inst@(Inst opcode _) <- return $ decode iset instBV

  -- Log instruction BEFORE execution
  -- TODO: switch argument order here
  logInstruction iset inst

  -- Execute
  execFormula (evalInstExpr iset inst 4) (getInstFormula $ semanticsFromOpcode iset opcode)

  -- Record cycle count
  execFormula evalPureStateExpr $ getFormula $ do
    let minstret = readCSR (litBV $ encodeCSR MInstRet)
    assignCSR (litBV $ encodeCSR MInstRet) (minstret `addE` litBV 1)

-- | Check whether the machine has halted.
isHalted :: (RVStateM m arch exts, KnownArch arch) => m Bool
isHalted = do
  mcause <- getCSR (encodeCSR MCause)
  return (mcause == 2 || mcause == 3 || mcause == 5 ||
          mcause == 7 || mcause == 8 || mcause == 11)

-- | Run for a given number of steps.
runRV :: forall m arch exts
         . (RVStateM m arch exts, KnownArch arch, KnownExtensions exts)
      => Int
      -> m Int
runRV = runRV' knownISet 0
  where runRV' _ currSteps maxSteps | currSteps >= maxSteps = return currSteps
        runRV' iset currSteps maxSteps = do
          halted <- isHalted
          case halted of
            True  -> return currSteps
            False -> stepRV iset >> runRV' iset (currSteps+1) maxSteps


----------------------------------------
-- Analysis

-- | Given a formula, constructs a list of all the tests that affect the execution of
-- that formula.
getTests :: Formula (InstExpr fmt arch exts) arch exts -> [InstExpr fmt arch exts 1]
getTests formula = nub (concat $ getTestsStmt <$> formula ^. fDefs)

getTestsStmt :: Stmt (InstExpr fmt arch exts) arch exts -> [InstExpr fmt arch exts 1]
getTestsStmt (AssignStmt le e) = getTestsLocExpr le ++ getTestsInstExpr e
getTestsStmt (BranchStmt t l r) =
  t : concat ((toList $ getTestsStmt <$> l) ++ (toList $ getTestsStmt <$> r))

getTestsLocExpr :: LocExpr (InstExpr fmt arch exts) arch exts w -> [InstExpr fmt arch exts 1]
getTestsLocExpr (RegExpr   e) = getTestsInstExpr e
getTestsLocExpr (MemExpr _ e) = getTestsInstExpr e
getTestsLocExpr (ResExpr   e) = getTestsInstExpr e
getTestsLocExpr (CSRExpr   e) = getTestsInstExpr e
getTestsLocExpr _ = []

getTestsStateExpr :: StateExpr (InstExpr fmt arch exts) arch exts w -> [InstExpr fmt arch exts 1]
getTestsStateExpr (LocExpr e) = getTestsLocExpr e
getTestsStateExpr (AppExpr e) = getTestsBVApp e

getTestsInstExpr :: InstExpr fmt arch exts w -> [InstExpr fmt arch exts 1]
getTestsInstExpr (OperandExpr _) = []
getTestsInstExpr InstBytes = []
getTestsInstExpr InstWord = []
getTestsInstExpr (InstStateExpr e) = getTestsStateExpr e

getTestsBVApp :: BVApp (InstExpr fmt arch exts) w -> [InstExpr fmt arch exts 1]
getTestsBVApp (IteApp t l r) = t : getTestsInstExpr l ++ getTestsInstExpr r
getTestsBVApp app = foldMapFC getTestsInstExpr app
