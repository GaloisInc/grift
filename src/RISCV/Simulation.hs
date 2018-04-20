{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
    RVState(..)
  , evalExpr
  , execFormula
  , runRV
  ) where

import Control.Lens ( (^.) )
import Control.Monad ( forM_, when )
import Data.BitVector.Sized
import Data.BitVector.Sized.App
import Data.Parameterized
import Data.Parameterized.List
import Prelude hiding ((!!))

import RISCV.Decode
import RISCV.Extensions
import RISCV.InstructionSet
import RISCV.Semantics
import RISCV.Types

-- | State monad for simulating RISC-V code
class (Monad m) => RVState m (arch :: BaseArch) (exts :: Extensions) | m -> arch, m -> exts where
  -- | Get the current PC.
  getPC  :: m (BitVector (ArchWidth arch))
  -- | Get the value of a register. Note that for all valid implementations, we
  -- require that getReg 0 = return 0.
  getReg :: BitVector 5 -> m (BitVector (ArchWidth arch))
  -- | Read a single byte from memory.
  getMem :: BitVector (ArchWidth arch) -> m (BitVector 8)

  -- | Set the PC.
  setPC :: BitVector (ArchWidth arch) -> m ()
  -- | Write to a register. Note that for all valid implementations, we require that
  -- setReg 0 = return ().
  setReg :: BitVector 5 -> BitVector (ArchWidth arch) -> m ()
  -- | Write a single byte to memory.
  setMem :: BitVector (ArchWidth arch) -> BitVector 8 -> m ()

  throwException :: Exception -> m ()
  exceptionStatus :: m (Maybe Exception)

getMem32 :: (KnownArch arch, RVState m arch exts) => BitVector (ArchWidth arch) -> m (BitVector 32)
getMem32 addr = do
  b0 <- getMem addr
  b1 <- getMem (addr+1)
  b2 <- getMem (addr+2)
  b3 <- getMem (addr+3)
  return $ b3 <:> b2 <:> b1 <:> b0

-- | Evaluate a 'Expr', given an 'RVState' implementation.
evalExpr :: forall m arch exts fmt w
            . (RVState m arch exts, KnownArch arch)
         => Operands fmt     -- ^ Operands
         -> Integer          -- ^ Instruction width (in bytes)
         -> Expr arch fmt w  -- ^ Expression to be evaluated
         -> m (BitVector w)
evalExpr (Operands _ operands) _ (OperandExpr p) = return (operands !! p)
evalExpr _ _ PCRead = getPC
evalExpr _ ib InstBytes = return $ bitVector ib
evalExpr operands ib (RegRead ridE) =
  evalExpr operands ib ridE >>= getReg
evalExpr operands ib (MemRead addrE) =
  evalExpr operands ib addrE >>= getMem
evalExpr operands ib (AppExpr bvApp) =
  evalBVAppM (evalExpr operands ib) bvApp

-- | Execute an assignment statement, given an 'RVState' implementation.
execStmt :: (RVState m arch exts, KnownArch arch)
         => Operands fmt  -- ^ Operands
         -> Integer       -- ^ Instruction width (in bytes)
         -> Stmt arch fmt -- ^ Statement to be executed
         -> m ()
execStmt operands ib (AssignReg ridE e) = do
  rid  <- evalExpr operands ib ridE
  eVal <- evalExpr operands ib e
  setReg rid eVal
execStmt operands ib (AssignMem addrE e) = do
  addr <- evalExpr operands ib addrE
  eVal <- evalExpr operands ib e
  setMem addr eVal
execStmt operands ib (AssignPC pcE) = do
  pcVal <- evalExpr operands ib pcE
  setPC pcVal
-- TODO: How do we want to throw exceptions?
execStmt operands ib (RaiseException cond e) = do
  condVal <- evalExpr operands ib cond
  when (condVal == 1) $ throwException e

-- | Execute a formula, given an 'RVState' implementation. This function represents
-- the "execute" state in a fetch\/decode\/execute sequence.
execFormula :: (RVState m arch exts, KnownArch arch)
            => Operands fmt
            -> Integer
            -> Formula arch fmt
            -> m ()
execFormula operands ib f = forM_ (f ^. fDefs) $ execStmt operands ib

-- | Fetch, decode, and execute a single instruction.
stepRV :: forall m arch exts
          . (RVState m arch exts, KnownArch arch, KnownExtensions exts)
       => InstructionSet arch exts
       -> m ()
stepRV iset = do
  -- Fetch
  pcVal  <- getPC
  instBV <- getMem32 pcVal

  -- Decode
  -- TODO: When we add compression ('C' extension), we'll need to modify this code.
  Some inst <- return $ decode iset instBV

  let operands = instOperands inst
      formula  = semanticsFromOpcode iset (instOpcode inst)

  -- Execute
  execFormula operands 4 formula

-- | Run for a given number of steps.
runRV :: forall m arch exts
         . (RVState m arch exts, KnownArch arch, KnownExtensions exts)
      => Int
      -> m ()
runRV n = runRV' knownISet n
  where runRV' _ i | i <= 0 = return ()
        runRV' iset i = do
          e <- exceptionStatus
          case e of
            Just _' -> return ()
            Nothing -> stepRV iset >> runRV' iset (i-1)
