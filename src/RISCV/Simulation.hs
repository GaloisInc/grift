-- {-# LANGUAGE AllowAmbiguousTypes #-}
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
  , evalExpr
  , execFormula
  , stepRV
  ) where

import Control.Lens ( (^.) )
import Control.Monad ( forM_ )
import Data.BitVector.Sized
import Data.Parameterized
import Foreign.Marshal.Utils (fromBool)

import RISCV.Decode
import RISCV.Instruction
import RISCV.InstructionSet
import RISCV.Semantics

----------------------------------------
-- Running semantics against a state monad

-- | State monad for simulating RISC-V code
class (Monad m) => RVState m arch | m -> arch where
  -- | The instruction set supported by this RVState
  getInstructionSet   :: m (InstructionSet arch)

  getPC  :: m (BitVector (ArchWidth arch))
  getReg :: BitVector 5 -> m (BitVector (ArchWidth arch))
  getMem :: NatRepr bytes
         -> BitVector (ArchWidth arch)
         -> m (BitVector (8*bytes))

  setPC :: BitVector (ArchWidth arch) -> m ()
  setReg :: BitVector 5 -> BitVector (ArchWidth arch) -> m ()
  setMem :: NatRepr bytes
         -> BitVector (ArchWidth arch)
         -> BitVector (8*bytes)
         -> m ()

-- | Evaluate a parameter's value from an 'Operands'.
evalParam :: OperandParam arch oid
          -> Operands fmt
          -> BitVector (OperandIDWidth oid)
evalParam (OperandParam RdRepr)    (ROperands  rd   _   _) = rd
evalParam (OperandParam Rs1Repr)   (ROperands   _ rs1   _) = rs1
evalParam (OperandParam Rs2Repr)   (ROperands   _   _ rs2) = rs2
evalParam (OperandParam RdRepr)    (IOperands  rd   _   _) = rd
evalParam (OperandParam Rs1Repr)   (IOperands   _ rs1   _) = rs1
evalParam (OperandParam Imm12Repr) (IOperands   _   _ imm) = imm
evalParam (OperandParam Rs1Repr)   (SOperands rs1   _   _) = rs1
evalParam (OperandParam Rs2Repr)   (SOperands   _ rs2   _) = rs2
evalParam (OperandParam Imm12Repr) (SOperands   _   _ imm) = imm
evalParam (OperandParam Rs1Repr)   (BOperands rs1   _   _) = rs1
evalParam (OperandParam Rs2Repr)   (BOperands   _ rs2   _) = rs2
evalParam (OperandParam Imm12Repr) (BOperands   _   _ imm) = imm
evalParam (OperandParam RdRepr)    (UOperands  rd       _) = rd
evalParam (OperandParam Imm20Repr) (UOperands   _     imm) = imm
evalParam (OperandParam RdRepr)    (JOperands  rd       _) = rd
evalParam (OperandParam Imm20Repr) (JOperands   _     imm) = imm
evalParam (OperandParam Imm32Repr) (XOperands         imm) = imm
evalParam oidRepr operands = error $
  "No operand " ++ show oidRepr ++ " in operands " ++ show operands

-- | Evaluate a 'BVExpr', given an 'RVState' implementation.
evalExpr :: (RVState m arch, KnownArch arch)
         => Operands fmt    -- ^ Operands
         -> Integer         -- ^ Instruction width (in bytes)
         -> BVExpr arch w   -- ^ Expression to be evaluated
         -> m (BitVector w)
evalExpr _ _ (LitBV bv) = return bv
evalExpr operands _ (ParamBV p) = return (evalParam p operands)
evalExpr _ _ PCRead = getPC
evalExpr _ ib InstBytes = return $ bitVector ib
evalExpr operands ib (RegRead ridE) =
  evalExpr operands ib ridE >>= getReg
evalExpr operands ib (MemRead bRepr addrE) =
  evalExpr operands ib addrE >>= getMem bRepr
evalExpr operands ib (AndE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ e1Val `bvAnd` e2Val
evalExpr operands ib (OrE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ e1Val `bvOr` e2Val
evalExpr operands ib (XorE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ e1Val `bvXor` e2Val
evalExpr operands ib (NotE e) =
  bvComplement <$> evalExpr operands ib e
evalExpr operands ib (AddE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ e1Val `bvAdd` e2Val
evalExpr operands ib (SubE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ e1Val `bvAdd` bvNegate e2Val
evalExpr operands ib (MulSE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ e1Val `bvMulFS` e2Val
evalExpr operands ib (MulUE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ e1Val `bvMulFU` e2Val
evalExpr operands ib (MulSUE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ e1Val `bvMulFSU` e2Val
evalExpr operands ib (DivSE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ e1Val `bvQuotS` e2Val
evalExpr operands ib (DivUE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ e1Val `bvQuotU` e2Val
evalExpr operands ib (RemSE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ e1Val `bvRemS` e2Val
evalExpr operands ib (RemUE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ e1Val `bvRemU` e2Val
-- TODO: throw some kind of exception if the shifter operand is larger than the
-- architecture width?
evalExpr operands ib (SllE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ e1Val `bvShiftL` fromIntegral (bvIntegerU e2Val)
evalExpr operands ib (SrlE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ e1Val `bvShiftRL` fromIntegral (bvIntegerU e2Val)
evalExpr operands ib (SraE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ e1Val `bvShiftRA` fromIntegral (bvIntegerU e2Val)
evalExpr operands ib (EqE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ fromBool (e1Val == e2Val)
evalExpr operands ib (LtuE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ fromBool (e1Val `bvLTU` e2Val)
evalExpr operands ib (LtsE e1 e2) = do
  e1Val <- evalExpr operands ib e1
  e2Val <- evalExpr operands ib e2
  return $ fromBool (e1Val `bvLTS` e2Val)
evalExpr operands ib (ZExtE wRepr e) =
  bvZextWithRepr wRepr <$> evalExpr operands ib e
evalExpr operands ib (SExtE wRepr e) =
  bvSextWithRepr wRepr <$> evalExpr operands ib e
evalExpr operands ib (ExtractE wRepr base e) =
  bvExtractWithRepr wRepr base <$> evalExpr operands ib e
evalExpr operands ib (IteE testE tE fE) = do
  testVal <- evalExpr operands ib testE
  tVal <- evalExpr operands ib tE
  fVal <- evalExpr operands ib fE
  return $ if testVal == 1 then tVal else fVal

-- | Execute an assignment statement, given an 'RVState' implementation.
execStmt :: (RVState m arch, KnownArch arch)
         => Operands fmt -- ^ Operands
         -> Integer      -- ^ Instruction width (in bytes)
         -> Stmt arch    -- ^ Statement to be executed
         -> m ()
execStmt operands ib (AssignReg ridE e) = do
  rid  <- evalExpr operands ib ridE
  eVal <- evalExpr operands ib e
  setReg rid eVal
execStmt operands ib (AssignMem bRepr addrE e) = do
  addr <- evalExpr operands ib addrE
  eVal <- evalExpr operands ib e
  setMem bRepr addr eVal
execStmt operands ib (AssignPC pcE) = do
  pcVal <- evalExpr operands ib pcE
  setPC pcVal
-- TODO: How do we want to throw exceptions?
execStmt _ _ _ = undefined

-- | Execute a formula, given an 'RVState' implementation. This function represents
-- the "execute" state in a fetch\/decode\/execute sequence.
execFormula :: (RVState m arch, KnownArch arch)
            => Operands fmt
            -> Integer
            -> Formula arch fmt
            -> m ()
execFormula operands ib f = forM_ (f ^. fDefs) $ execStmt operands ib

-- | Fetch, decode, and execute a single instruction.
stepRV :: (RVState m arch, KnownArch arch) => m ()
stepRV = do
  -- Fetch
  pcVal  <- getPC
  instBV <- getMem (knownRepr :: NatRepr 4) pcVal

  -- Decode
  -- TODO: When we add compression ('C' extension), we'll need to modify this code.
  iset      <- getInstructionSet
  Some inst <- return $ decode iset instBV
  let operands = instOperands inst
      formula  = semanticsFromOpcode iset (instOpcode inst)

  -- Execute
  execFormula operands 4 formula
