{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

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
  , evalExpr
  , Loc(..)
  , Assignment(..)
  , buildAssignment
  , execAssignment
  , execFormula
  , runRV
  ) where

import Control.Lens ( (^.) )
import Data.BitVector.Sized
import Data.BitVector.Sized.App
import Data.Foldable
import Data.Parameterized
import Data.Parameterized.List
import Data.Traversable
import Data.Sequence (Seq)
import Prelude hiding ((!!))

import RISCV.Decode
import RISCV.Extensions
import RISCV.InstructionSet
import RISCV.Semantics
import RISCV.Types

import Debug.Trace (traceM)

-- | State monad for simulating RISC-V code
class (Monad m) => RVStateM m (arch :: BaseArch) (exts :: Extensions) | m -> arch, m -> exts where
  -- | Get the current PC.
  getPC   :: m (BitVector (ArchWidth arch))
  -- | Get the value of a register. This function shouldn't ever be called with an
  -- argument of 0, so there is no need to hardwire it to 0 in an implementation.
  getReg  :: BitVector 5 -> m (BitVector (ArchWidth arch))
  -- | Read a single byte from memory.
  getMem  :: BitVector (ArchWidth arch) -> m (BitVector 8)
  -- | Get the value of a CSR.
  getCSR  :: BitVector 12 -> m (BitVector (ArchWidth arch))
  -- | Get the current privilege level.
  getPriv :: m (BitVector 2)

  -- | Set the PC.
  setPC   :: BitVector (ArchWidth arch) -> m ()
  -- | Write to a register.
  setReg  :: BitVector 5 -> BitVector (ArchWidth arch) -> m ()
  -- | Write a single byte to memory.
  setMem  :: BitVector (ArchWidth arch) -> BitVector 8 -> m ()
  -- | Write to a CSR.
  setCSR  :: BitVector 12 -> BitVector (ArchWidth arch) -> m ()
  -- | Set the privilege level.
  setPriv :: BitVector 2 -> m ()

-- | Evaluate a 'Expr', given an 'RVStateM' implementation.
evalExpr :: forall m arch exts fmt w
            . (RVStateM m arch exts, KnownArch arch)
         => Operands fmt     -- ^ Operands
         -> Integer          -- ^ Instruction width (in bytes)
         -> Expr arch fmt w  -- ^ Expression to be evaluated
         -> m (BitVector w)
evalExpr (Operands _ operands) _ (OperandExpr p) = return (operands !! p)
evalExpr _ ib InstBytes = return $ bitVector ib
evalExpr _ _ (LocExpr PCExpr) = getPC
evalExpr operands ib (LocExpr (RegExpr ridE)) =
  evalExpr operands ib ridE >>= getReg
evalExpr operands ib (LocExpr (MemExpr addrE)) =
  evalExpr operands ib addrE >>= getMem
evalExpr operands ib (LocExpr (CSRExpr csrE)) =
  evalExpr operands ib csrE >>= getCSR
evalExpr _ _ (LocExpr PrivExpr) = getPriv
evalExpr operands ib (AppExpr bvApp) =
  evalBVAppM (evalExpr operands ib) bvApp

-- | This type represents a concrete component of the global state, after all
-- expressions have been evaluated. It is in direct correspondence with the 'LocExpr'
-- type from RISCV.Semantics. Unlike that type, however, this will never appear on
-- the right-hand side of an assignment, only the left-hand side.
data Loc arch w where
  PC :: Loc arch (ArchWidth arch)
  Reg :: BitVector 5 -> Loc arch (ArchWidth arch)
  Mem :: BitVector (ArchWidth arch) -> Loc arch 8
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

-- | Convert a 'Stmt' into an 'Assignment' by evaluating its right-hand sides.
buildAssignment :: (RVStateM m arch exts, KnownArch arch)
                => Operands fmt
                -> Integer
                -> Stmt arch fmt
                -> m (Assignment arch)
buildAssignment operands ib (AssignStmt PCExpr pcE) = do
  pcVal <- evalExpr operands ib pcE
  return (Assignment PC pcVal)
buildAssignment operands ib (AssignStmt (RegExpr ridE) e) = do
  rid  <- evalExpr operands ib ridE
  eVal <- evalExpr operands ib e
  return (Assignment (Reg rid) eVal)
buildAssignment operands ib (AssignStmt (MemExpr addrE) e) = do
  addr <- evalExpr operands ib addrE
  eVal <- evalExpr operands ib e
  return (Assignment (Mem addr) eVal)
buildAssignment operands ib (AssignStmt (CSRExpr csrE) e) = do
  csr  <- evalExpr operands ib csrE
  eVal <- evalExpr operands ib e
  return (Assignment (CSR csr) eVal)
buildAssignment operands ib (AssignStmt PrivExpr privE) = do
  privVal <- evalExpr operands ib privE
  return (Assignment Priv privVal)
buildAssignment operands ib (BranchStmt condE tStmts fStmts) = do
  condVal <- evalExpr operands ib condE
  tAssignments <- traverse (buildAssignment operands ib) tStmts
  fAssignments <- traverse (buildAssignment operands ib) fStmts
  return (Branch condVal tAssignments fAssignments)

-- | Execute an assignment.
execAssignment :: (RVStateM m arch exts, KnownArch arch) => Assignment arch -> m ()
execAssignment (Assignment PC val) = setPC val
execAssignment (Assignment (Reg rid) val) = setReg rid val
execAssignment (Assignment (Mem addr) val) = setMem addr val
execAssignment (Assignment (CSR csr) val) = do
  traceM $ "CSR[" ++ show csr ++ "] := " ++ show val
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

getMem32 :: (KnownArch arch, RVStateM m arch exts) => BitVector (ArchWidth arch) -> m (BitVector 32)
getMem32 addr = do
  b0 <- getMem addr
  b1 <- getMem (addr+1)
  b2 <- getMem (addr+2)
  b3 <- getMem (addr+3)
  return $ b3 <:> b2 <:> b1 <:> b0

-- | Fetch, decode, and execute a single instruction.
stepRV :: forall m arch exts
          . (RVStateM m arch exts, KnownArch arch, KnownExtensions exts)
       => InstructionSet arch exts
       -> m ()
stepRV iset = do
  -- Fetch
  pcVal  <- getPC
  instBV <- getMem32 pcVal

  -- Decode
  -- TODO: When we add compression ('C' extension), we'll need to modify this code.
  Some (Inst opcode operands) <- return $ decode iset instBV

  -- Execute
  execFormula operands 4 (semanticsFromOpcode iset opcode)

-- | Check whether the machine has halted.
isHalted :: (RVStateM m arch exts, KnownArch arch) => m Bool
isHalted = do
  mcause <- getCSR (encodeCSR MCause)
  return (mcause == 2 || mcause == 3 || mcause == 8)

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
