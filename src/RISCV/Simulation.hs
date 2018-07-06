{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
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
  , evalInstExpr
  , Loc(..)
  , Assignment(..)
  , buildAssignment
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
  logInstruction :: Some (Instruction arch exts) -> InstructionSet arch exts -> m ()

-- | Evaluate a 'LocExpr', given an 'RVStateM' implementation.
evalLocExpr :: forall m expr arch exts w
               . (RVStateM m arch exts, KnownArch arch)
            => (forall w' . expr w' -> m (BitVector w')) -- ^ evaluator for internal expressions
            -> LocExpr expr arch w
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
              -> StateExpr expr arch w
              -> m (BitVector w)
evalStateExpr eval (LocExpr e) = evalLocExpr eval e
evalStateExpr eval (AppExpr e) = evalBVAppM eval e

-- | Evaluate a 'Expr', given an 'RVStateM' implementation and the instruction context.
evalInstExpr :: forall m arch exts fmt w
            . (RVStateM m arch exts, KnownArch arch)
             => Operands fmt     -- ^ Operands
             -> Integer          -- ^ Instruction width (in bytes)
             -> InstExpr arch fmt w  -- ^ Expression to be evaluated
             -> m (BitVector w)
evalInstExpr (Operands _ operands) _ (OperandExpr (OperandID p)) = return (operands !! p)
evalInstExpr _ ib InstBytes = return $ bitVector ib
evalInstExpr operands ib (StateExpr e) = evalStateExpr (evalInstExpr operands ib) e

-- | This type represents a concrete component of the global state, after all
-- expressions have been evaluated. It is in direct correspondence with the 'LocExpr'
-- type from RISCV.Semantics. Unlike that type, however, this will never appear on
-- the right-hand side of an assignment, only the left-hand side.
data Loc arch w where
  PC :: Loc arch (ArchWidth arch)
  Reg :: BitVector 5 -> Loc arch (ArchWidth arch)
  Mem :: NatRepr bytes -> BitVector (ArchWidth arch) -> Loc arch (8*bytes)
  Res :: BitVector (ArchWidth arch) -> Loc arch 1
  CSR :: BitVector 12 -> Loc arch (ArchWidth arch)
  Priv :: Loc arch 2

-- | This type represents a concrete assignment statement, where the left-hand side
-- is a known location and the right-hand side is a known BitVector value. Because we
-- are still abstracting out the notion of throwing an exception, we also have a
-- constructor for that; however, this will be removed once we wire exception
-- handling into the semantics themselves.
data Assignment (arch :: BaseArch) where
  Assignment :: Loc arch w -> BitVector w -> Assignment arch
  Branch :: BitVector 1 -> Seq (Assignment arch) -> Seq (Assignment arch) -> Assignment arch

-- TODO: Create a separate data type, like Stmt, but without reference
-- to a particular instruction, in order to enable the simulation to
-- execute actual semantic assignments based on the current state
-- outside the context of executing a particular instruction.

-- | Convert a 'Stmt' into an 'Assignment' by evaluating its right-hand sides.
buildAssignment :: (RVStateM m arch exts, KnownArch arch)
                => Operands fmt
                -> Integer
                -> Stmt arch fmt
                -> m (Assignment arch)
buildAssignment operands ib (AssignStmt PCExpr pcE) = do
  pcVal <- evalInstExpr operands ib pcE
  return (Assignment PC pcVal)
buildAssignment operands ib (AssignStmt (RegExpr ridE) e) = do
  rid  <- evalInstExpr operands ib ridE
  eVal <- evalInstExpr operands ib e
  return (Assignment (Reg rid) eVal)
buildAssignment operands ib (AssignStmt (MemExpr bytes addrE) e) = do
  addr <- evalInstExpr operands ib addrE
  eVal <- evalInstExpr operands ib e
  return (Assignment (Mem bytes addr) eVal)
buildAssignment operands ib (AssignStmt (ResExpr addrE) e) = do
  addr <- evalInstExpr operands ib addrE
  eVal <- evalInstExpr operands ib e
  return (Assignment (Res addr) eVal)
buildAssignment operands ib (AssignStmt (CSRExpr csrE) e) = do
  csr  <- evalInstExpr operands ib csrE
  eVal <- evalInstExpr operands ib e
  return (Assignment (CSR csr) eVal)
buildAssignment operands ib (AssignStmt PrivExpr privE) = do
  privVal <- evalInstExpr operands ib privE
  return (Assignment Priv privVal)
buildAssignment operands ib (BranchStmt condE tStmts fStmts) = do
  condVal <- evalInstExpr operands ib condE
  tAssignments <- traverse (buildAssignment operands ib) tStmts
  fAssignments <- traverse (buildAssignment operands ib) fStmts
  return (Branch condVal tAssignments fAssignments)

-- | Execute an assignment.
execAssignment :: (RVStateM m arch exts, KnownArch arch) => Assignment arch -> m ()
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

-- | Execute a formula, given an 'RVStateM' implementation. This function represents
-- the "execute" state in a fetch\/decode\/execute sequence.
execFormula :: forall m arch fmt exts . (RVStateM m arch exts, KnownArch arch)
            => Operands fmt
            -> Integer
            -> Formula arch fmt
            -> m ()
execFormula operands ib f = do
  assignments <- traverse (buildAssignment operands ib) (f ^. fDefs)
  traverse_ execAssignment assignments

-- | Fetch, decode, and execute a single instruction.
stepRV :: forall m arch exts
          . (RVStateM m arch exts, KnownArch arch, KnownExtensions exts)
       => InstructionSet arch exts
--       -> LatencyMap arch exts
       -> m ()
stepRV iset = do
  -- Fetch
  pcVal  <- getPC
  instBV <- getMem (knownNat @4) pcVal

  -- Decode
  -- TODO: When we add compression ('C' extension), we'll need to modify this code.
  inst@(Some (Inst opcode operands)) <- return $ decode iset instBV

  -- Log instruction
  logInstruction inst iset

  -- Execute
  execFormula operands 4 (semanticsFromOpcode iset opcode)

  -- Record cycle count
  -- cc <- evalInstExpr operands 4 (Map.lookup opcode latencyMap)

-- | Check whether the machine has halted.
isHalted :: (RVStateM m arch exts, KnownArch arch) => m Bool
isHalted = do
  mcause <- getCSR (encodeCSR MCause)
  return (mcause == 2 || mcause == 3 || mcause == 5 ||
          mcause == 7 || mcause == 8 )

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
getTests :: Formula arch fmt -> [InstExpr arch fmt 1]
getTests formula = nub (concat $ getTestsStmt <$> formula ^. fDefs)

getTestsStmt :: Stmt arch fmt -> [InstExpr arch fmt 1]
getTestsStmt (AssignStmt le e) = getTestsLocExpr le ++ getTestsInstExpr e
getTestsStmt (BranchStmt t l r) =
  t : concat ((toList $ getTestsStmt <$> l) ++ (toList $ getTestsStmt <$> r))

getTestsLocExpr :: LocExpr (InstExpr arch fmt) arch w -> [InstExpr arch fmt 1]
getTestsLocExpr (RegExpr   e) = getTestsInstExpr e
getTestsLocExpr (MemExpr _ e) = getTestsInstExpr e
getTestsLocExpr (ResExpr   e) = getTestsInstExpr e
getTestsLocExpr (CSRExpr   e) = getTestsInstExpr e
getTestsLocExpr _ = []

getTestsStateExpr :: StateExpr (InstExpr arch fmt) arch w -> [InstExpr arch fmt 1]
getTestsStateExpr (LocExpr e) = getTestsLocExpr e
getTestsStateExpr (AppExpr e) = getTestsBVApp e

getTestsInstExpr :: InstExpr arch fmt w -> [InstExpr arch fmt 1]
getTestsInstExpr (OperandExpr _) = []
getTestsInstExpr InstBytes = []
getTestsInstExpr (StateExpr e) = getTestsStateExpr e
-- getTestsExpr (LocExpr le) = getTestsLocExpr le
-- getTestsExpr (AppExpr bvApp) = getTestsBVApp bvApp

getTestsBVApp :: BVApp (InstExpr arch fmt) w -> [InstExpr arch fmt 1]
getTestsBVApp (IteApp t l r) = t : getTestsInstExpr l ++ getTestsInstExpr r
getTestsBVApp app = foldMapFC getTestsInstExpr app
