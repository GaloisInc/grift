{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : RISCV.Simulation.MapMachine
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

An Map-based simulation backend for RISC-V machines. We use the 'BitVector' type
directly for the underlying values, which allows us to keep the architecture width
unspecified.
-}

module RISCV.Simulation.MapMachine
  ( MapMachine(..)
  , mkMapMachine
  , MapMachineM
  , runMapMachine
  ) where

import           Control.Lens ((^.))
import qualified Control.Monad.State.Class as S
import           Control.Monad.Trans.State
import           Data.BitVector.Sized
import           Data.BitVector.Sized.App
import qualified Data.ByteString as BS
import           Data.Foldable (for_, toList)
import           Data.List (nub, union)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Parameterized
import           Data.Traversable (for)

import RISCV.InstructionSet
import RISCV.Types
import RISCV.Simulation
import RISCV.Semantics
import RISCV.Semantics.Exceptions

-- | IO-based machine state.
data MapMachine (arch :: BaseArch) (exts :: Extensions) = MapMachine
  { pc        :: BitVector (ArchWidth arch)
  , registers :: Map (BitVector 5) (BitVector (ArchWidth arch))
  , memory    :: Map (BitVector (ArchWidth arch)) (BitVector 8)
  , csrs      :: Map (BitVector 12) (BitVector (ArchWidth arch))
  , priv      :: BitVector 2
  , maxAddr   :: BitVector (ArchWidth arch)
  , exception :: Maybe Exception
  , steps     :: Int
  , testMap   :: Map (Some (Opcode arch)) [[BitVector 1]]
  }

-- | Construct an MapMachine with a given maximum address, entry point, and list of
-- (addr, bytestring) pairs to load into the memory.
mkMapMachine :: (KnownArch arch, KnownExtensions exts)
          => BitVector (ArchWidth arch)
          -> BitVector (ArchWidth arch)
          -> [(BitVector (ArchWidth arch), BS.ByteString)]
          -> MapMachine arch exts
mkMapMachine maxAddr' entryPoint byteStrings =
  MapMachine { pc = entryPoint
             , registers = Map.fromList (zip [1..31] (repeat 0))
             , memory = Map.fromList memoryAssocs
             , csrs = Map.empty
             , priv = 0b00
             , maxAddr = maxAddr'
             , exception = Nothing
             , steps = 0
             , testMap = Map.empty
             }
  where memoryAssocs = concat (map zipBS byteStrings)
        zipBS (start, bs) = zip [start..] (map (bitVector . fromIntegral) (BS.unpack bs))

-- | The 'MapMachineM' monad instantiates the 'RVState' monad type class, tying the
-- 'RVState' interface functions to actual transformations on the underlying mutable
-- state.
newtype MapMachineM (arch :: BaseArch) (exts :: Extensions) a =
  MapMachineM { runMapMachineM :: State (MapMachine arch exts) a }
  deriving (Functor, Applicative, Monad, S.MonadState (MapMachine arch exts))

instance KnownArch arch => RVStateM (MapMachineM arch exts) arch exts where
  getPC = MapMachineM $ pc <$> get
  getReg rid = MapMachineM $ Map.findWithDefault 0 rid <$> registers <$> get
  getMem bytes addr = MapMachineM $ do
    mem <- memory <$> get
    ma <- maxAddr <$> get
    case addr + fromIntegral (natValue bytes) < ma of
      True -> do
        val <- for [addr..addr+(fromIntegral (natValue bytes-1))] $ \a ->
          return $ Map.findWithDefault 0 a mem
        return $ bvConcatManyWithRepr ((knownNat @8) `natMultiply` bytes) val
      False -> do
        S.modify $ \m -> m { exception = Just LoadAccessFault }
        return (BV ((knownNat @8) `natMultiply` bytes) 0)
  getCSR csr = MapMachineM $ Map.findWithDefault 0 csr <$> csrs <$> get
  getPriv = MapMachineM $ priv <$> get

  setPC pcVal = MapMachineM $ do
    S.modify $ \m -> m { pc = pcVal, steps = steps m + 1 }
  setReg rid regVal = MapMachineM $ S.modify $ \m ->
    m { registers = Map.insert rid regVal (registers m) }
  setMem bytes addr val = MapMachineM $ do
    ma <- maxAddr <$> get
    case addr < ma of
      True -> do
        for_ addrValPairs $ \(a, byte) -> S.modify $ \m ->
          m { memory = Map.insert a byte (memory m) }
        where addrValPairs = zip
                [addr..addr+(fromIntegral (natValue bytes-1))]
                (bvGetBytesU (fromIntegral (natValue bytes)) val)
      False -> undefined
  setCSR csr csrVal = MapMachineM $ S.modify $ \m ->
    m { csrs = Map.insert csr csrVal (csrs m) }
  setPriv privVal = MapMachineM $ S.modify $ \m -> m { priv = privVal }

  logInstruction (Some (Inst opcode operands)) iset = do
    let formula = semanticsFromOpcode iset opcode
        tests = getTests formula
    testVals <- traverse (evalExpr operands 4) tests
    MapMachineM $ S.modify $ \m ->
      m { testMap = Map.insertWith union (Some opcode) [testVals] (testMap m) }

-- | Run the simulator for a given number of steps.
runMapMachine :: (KnownArch arch, KnownExtensions exts)
             => Int
             -> MapMachine arch exts
             -> (Int, MapMachine arch exts)
runMapMachine maxSteps m = flip runState m $ runMapMachineM $ runRV maxSteps

----------------------------------------
-- Analysis

-- | Given a formula, constructs a list of all the tests that affect the execution of
-- that formula.
getTests :: Formula arch fmt -> [Expr arch fmt 1]
getTests formula = nub (concat $ getTestsStmt <$> formula ^. fDefs)

getTestsStmt :: Stmt arch fmt -> [Expr arch fmt 1]
getTestsStmt (AssignStmt le e) = getTestsLocExpr le ++ getTestsExpr e
getTestsStmt (BranchStmt t l r) =
  t : concat ((toList $ getTestsStmt <$> l) ++ (toList $ getTestsStmt <$> r))

getTestsLocExpr :: LocExpr arch fmt w -> [Expr arch fmt 1]
getTestsLocExpr (RegExpr   e) = getTestsExpr e
getTestsLocExpr (MemExpr _ e) = getTestsExpr e
getTestsLocExpr (ResExpr   e) = getTestsExpr e
getTestsLocExpr (CSRExpr   e) = getTestsExpr e
getTestsLocExpr _ = []

getTestsExpr :: Expr arch fmt w -> [Expr arch fmt 1]
getTestsExpr (OperandExpr _) = []
getTestsExpr InstBytes = []
getTestsExpr (LocExpr le) = getTestsLocExpr le
getTestsExpr (AppExpr bvApp) = getTestsBVApp bvApp

getTestsBVApp :: BVApp (Expr arch fmt) w -> [Expr arch fmt 1]
getTestsBVApp (IteApp t l r) = t : getTestsExpr l ++ getTestsExpr r
getTestsBVApp app = foldMapFC getTestsExpr app
