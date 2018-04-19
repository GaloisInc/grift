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
  , evalParam
  , evalRVExpr
  , execFormula
  , runRV
  ) where

import Control.Lens ( (^.) )
import Control.Monad ( forM_, when )
import Data.BitVector.Sized
import Data.Parameterized
import Foreign.Marshal.Utils (fromBool)

import RISCV.Decode
import RISCV.Extensions
import RISCV.Instruction
import RISCV.InstructionSet
import RISCV.Semantics
import RISCV.Types

import Debug.Trace (traceM)

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

-- | Evaluate a parameter's value from an 'Operands'.
evalParam :: OperandID fmt otp
          -> Operands fmt
          -> BitVector (OperandWidth otp)
evalParam RRd    (ROperands  rd   _   _) = rd
evalParam RRs1   (ROperands   _ rs1   _) = rs1
evalParam RRs2   (ROperands   _   _ rs2) = rs2
evalParam IRd    (IOperands  rd   _   _) = rd
evalParam IRs1   (IOperands   _ rs1   _) = rs1
evalParam IImm12 (IOperands   _   _ imm) = imm
evalParam SRs1   (SOperands rs1   _   _) = rs1
evalParam SRs2   (SOperands   _ rs2   _) = rs2
evalParam SImm12 (SOperands   _   _ imm) = imm
evalParam BRs1   (BOperands rs1   _   _) = rs1
evalParam BRs2   (BOperands   _ rs2   _) = rs2
evalParam BImm12 (BOperands   _   _ imm) = imm
evalParam URd    (UOperands  rd       _) = rd
evalParam UImm20 (UOperands   _     imm) = imm
evalParam JRd    (JOperands  rd       _) = rd
evalParam JImm20 (JOperands   _     imm) = imm
evalParam XImm32 (XOperands         imm) = imm

-- | Evaluate a 'RVExpr', given an 'RVState' implementation.
evalRVExpr :: forall m arch exts fmt w
            . (RVState m arch exts, KnownArch arch)
         => Operands fmt    -- ^ Operands
         -> Integer         -- ^ Instruction width (in bytes)
         -> RVExpr arch fmt w   -- ^ Expression to be evaluated
         -> m (BitVector w)
evalRVExpr operands _ (ParamBV p) = return (evalParam p operands)
evalRVExpr _ _ PCRead = getPC
evalRVExpr _ ib InstBytes = return $ bitVector ib
evalRVExpr operands ib (RegRead ridE) =
  evalRVExpr operands ib ridE >>= getReg
evalRVExpr operands ib (MemRead addrE) =
  evalRVExpr operands ib addrE >>= getMem

evalRVExpr _ _ (BVAppVal (LitBVApp bv)) = return bv
evalRVExpr operands ib (BVAppVal (AndApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvAnd` e2Val
evalRVExpr operands ib (BVAppVal (OrApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvOr` e2Val
evalRVExpr operands ib (BVAppVal (XorApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvXor` e2Val
evalRVExpr operands ib (BVAppVal (NotApp e)) =
  bvComplement <$> evalRVExpr operands ib e
evalRVExpr operands ib (BVAppVal (AddApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvAdd` e2Val
evalRVExpr operands ib (BVAppVal (SubApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvAdd` bvNegate e2Val
evalRVExpr operands ib (BVAppVal (MulSApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvMulFS` e2Val
evalRVExpr operands ib (BVAppVal (MulUApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvMulFU` e2Val
evalRVExpr operands ib (BVAppVal (MulSUApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvMulFSU` e2Val
evalRVExpr operands ib (BVAppVal (DivSApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvQuotS` e2Val
evalRVExpr operands ib (BVAppVal (DivUApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvQuotU` e2Val
evalRVExpr operands ib (BVAppVal (RemSApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvRemS` e2Val
evalRVExpr operands ib (BVAppVal (RemUApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvRemU` e2Val
evalRVExpr operands ib (BVAppVal (SllApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvShiftL` fromIntegral (bvIntegerU e2Val)
evalRVExpr operands ib (BVAppVal (SrlApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvShiftRL` fromIntegral (bvIntegerU e2Val)
evalRVExpr operands ib (BVAppVal (SraApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvShiftRA` fromIntegral (bvIntegerU e2Val)
evalRVExpr operands ib (BVAppVal (EqApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ fromBool (e1Val == e2Val)
evalRVExpr operands ib (BVAppVal (LtuApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ fromBool (e1Val `bvLTU` e2Val)
evalRVExpr operands ib (BVAppVal (LtsApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ fromBool (e1Val `bvLTS` e2Val)
evalRVExpr operands ib (BVAppVal (ZExtApp wRepr e)) =
  bvZextWithRepr wRepr <$> evalRVExpr operands ib e
evalRVExpr operands ib (BVAppVal (SExtApp wRepr e)) =
  bvSextWithRepr wRepr <$> evalRVExpr operands ib e
evalRVExpr operands ib (BVAppVal (ExtractApp wRepr base e)) =
  bvExtractWithRepr wRepr base <$> evalRVExpr operands ib e
evalRVExpr operands ib (BVAppVal (ConcatApp e1 e2)) = do
  e1Val <- evalRVExpr operands ib e1
  e2Val <- evalRVExpr operands ib e2
  return $ e1Val `bvConcat` e2Val
evalRVExpr operands ib (BVAppVal (IteApp testE tE fE)) = do
  testVal <- evalRVExpr operands ib testE
  tVal <- evalRVExpr operands ib tE
  fVal <- evalRVExpr operands ib fE
  return $ if testVal == 1 then tVal else fVal

-- | Execute an assignment statement, given an 'RVState' implementation.
execStmt :: (RVState m arch exts, KnownArch arch)
         => Operands fmt  -- ^ Operands
         -> Integer       -- ^ Instruction width (in bytes)
         -> Stmt arch fmt -- ^ Statement to be executed
         -> m ()
execStmt operands ib (AssignReg ridE e) = do
  rid  <- evalRVExpr operands ib ridE
  eVal <- evalRVExpr operands ib e
  setReg rid eVal
execStmt operands ib (AssignMem addrE e) = do
  addr <- evalRVExpr operands ib addrE
  eVal <- evalRVExpr operands ib e

  setMem addr eVal

execStmt operands ib (AssignPC pcE) = do
  pcVal <- evalRVExpr operands ib pcE
  setPC pcVal
-- TODO: How do we want to throw exceptions?
execStmt operands ib (RaiseException cond e) = do
  condVal <- evalRVExpr operands ib cond
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
