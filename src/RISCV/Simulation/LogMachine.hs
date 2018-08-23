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

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : RISCV.Simulation.LogMachine
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

An IO-based simulation backend for RISC-V machines. We use the 'BitVector' type
directly for the underlying values, which allows us to keep the architecture width
unspecified.

This variant of LogMachine runs slower because it also logs coverage statistics.
-}

module RISCV.Simulation.LogMachine
  ( LogMachine(..)
  , mkLogMachine
  , LogMachineM
  , freezeRegisters
  , freezeFRegisters
  , runLogMachine
  ) where

import           Control.Monad (forM_)
import qualified Control.Monad.Reader.Class as R
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.Array.IArray
import           Data.Array.IO
import           Data.BitVector.Sized
import qualified Data.ByteString as BS
import           Data.Foldable (for_)
import           Data.IORef
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Parameterized
import qualified Data.Parameterized.Map as MapF
import           Data.Traversable (for)

import RISCV.Coverage
import RISCV.InstructionSet.Utils
import RISCV.Types
import RISCV.Simulation

import Debug.Trace (traceM)

-- TODO: get rid of unused IORefs
-- | IO-based machine state.
data LogMachine (rv :: RV) = LogMachine
  { lmRV        :: RVRepr rv
  , lmPC        :: IORef (BitVector (RVWidth rv))
  , lmRegisters :: IOArray (BitVector 5) (BitVector (RVWidth rv))
  , lmFRegisters :: IOArray (BitVector 5) (BitVector (RVFloatWidth rv))
  , lmMemory    :: IOArray (BitVector (RVWidth rv)) (BitVector 8)
  , lmCSRs      :: IORef (Map (BitVector 12) (BitVector (RVWidth rv)))
  , lmPriv      :: IORef (BitVector 2)
  , lmMaxAddr   :: BitVector (RVWidth rv)
  , lmTestMap   :: IORef (Map (Some (Opcode rv)) [BitVector 1])
  }

writeBS :: (Enum i, Num i, Ix i) => i -> BS.ByteString -> IOArray i (BitVector 8) -> IO ()
writeBS ix bs arr = do
  case BS.null bs of
    True -> return ()
    _    -> do
      writeArray arr ix (fromIntegral (BS.head bs))
      writeBS (ix+1) (BS.tail bs) arr

-- | Construction a 'LogMachine'.
mkLogMachine :: RVRepr rv
             -> BitVector (RVWidth rv)
             -> BitVector (RVWidth rv)
             -> BitVector (RVWidth rv)
             -> [(BitVector (RVWidth rv), BS.ByteString)]
             -> IO (LogMachine rv)
mkLogMachine rvRepr maxAddr entryPoint sp byteStrings = do
  pc        <- newIORef entryPoint
  registers <- withRVWidth rvRepr $ newArray (1, 31) 0
  fregisters <- withRVFloatWidth rvRepr $ newArray (0, 31) 0
  memory    <- withRVWidth rvRepr $ newArray (0, maxAddr) 0
  csrs      <- newIORef $ Map.fromList [ ]
  priv      <- newIORef 0b11 -- M mode by default.
  let f (Pair oc (InstExprList exprs)) = (Some oc, replicate (length exprs) 0)
  testMap   <- newIORef $ Map.fromList $ []-- f <$> MapF.toList knownCoverageMap

  -- set up stack pointer
  writeArray registers 2 sp

  forM_ byteStrings $ \(addr, bs) ->
    withRVWidth rvRepr $ writeBS addr bs memory
  return (LogMachine rvRepr pc registers fregisters memory csrs priv maxAddr testMap)

-- | The 'LogMachineM' monad instantiates the 'RVState' monad type class, tying the
-- 'RVState' interface functions to actual transformations on the underlying mutable
-- state.
newtype LogMachineM (rv :: RV) a =
  LogMachineM { runLogMachineM :: ReaderT (LogMachine rv) IO a }
  deriving (Functor, Applicative, Monad, R.MonadReader (LogMachine rv))

instance RVStateM (LogMachineM rv) rv where
  getRV = LogMachineM $ lmRV <$> ask
  getPC = LogMachineM $ do
    pcRef <- lmPC <$> ask
    pcVal <- lift $ readIORef pcRef
    return pcVal
  getReg rid = LogMachineM $ do
    regArray <- lmRegisters <$> ask
    regVal   <- lift $ readArray regArray rid
    return regVal
  getFReg rid = LogMachineM $ do
    regArray <- lmFRegisters <$> ask
    regVal   <- lift $ readArray regArray rid
    return regVal
  getMem bytes addr = LogMachineM $ do
    memArray <- lmMemory <$> ask
    maxAddr  <- lmMaxAddr <$> ask
    rv <- lmRV <$> ask
    withRVWidth rv $
      case addr + fromIntegral (natValue bytes) < maxAddr of
        True -> do
          val <- lift $
            for [addr..addr+(fromIntegral (natValue bytes-1))] $ \a -> readArray memArray a
          return (bvConcatManyWithRepr ((knownNat @8) `natMultiply` bytes) val)
        False -> do
          -- TODO: We need to change this code to actually execute the semantics
          -- we've pre-defined in RISCV.Semantics.Exceptions
          traceM $ "Tried to read from memory location " ++ show addr
          csrsRef <- lmCSRs <$> ask
          csrMap  <- lift $ readIORef csrsRef
          lift $ writeIORef csrsRef (Map.insert (encodeCSR MCause) (getMCause StoreAccessFault) csrMap)
          return (BV ((knownNat @8) `natMultiply` bytes) 0)
  getCSR csr = LogMachineM $ do
    csrsRef <- lmCSRs <$> ask
    csrMap  <- lift $ readIORef csrsRef
    rv <- lmRV <$> ask
    let csrVal = case Map.lookup csr csrMap of
          Just val -> val
          Nothing  -> withRVWidth rv 0 -- TODO: throw exception?
    return csrVal
  getPriv = LogMachineM $ do
    privRef <- lmPriv <$> ask
    privVal <- lift $ readIORef privRef
    return privVal

  setPC pcVal = LogMachineM $ do
    pcRef <- lmPC <$> ask
    lift $ writeIORef pcRef pcVal
  setReg rid regVal = LogMachineM $ do
    regArray <- lmRegisters <$> ask
    lift $ writeArray regArray rid regVal
  setFReg rid regVal = LogMachineM $ do
    regArray <- lmFRegisters <$> ask
    lift $ writeArray regArray rid regVal
  setMem bytes addr val = LogMachineM $ do
    memArray <- lmMemory <$> ask
    maxAddr <- lmMaxAddr <$> ask
    rv <- lmRV <$> ask
    withRVWidth rv $
      case addr < maxAddr of
        True -> lift $
          for_ addrValPairs $ \(a, byte) -> writeArray memArray a byte
          where addrValPairs = zip
                  [addr..addr+(fromIntegral (natValue bytes-1))]
                  (bvGetBytesU (fromIntegral (natValue bytes)) val)
        False -> do
          traceM $ "Tried to write to memory location " ++ show addr
          csrsRef <- lmCSRs <$> ask
          csrMap  <- lift $ readIORef csrsRef
          lift $ writeIORef csrsRef (Map.insert (encodeCSR MCause) (getMCause StoreAccessFault) csrMap)
  setCSR csr csrVal = LogMachineM $ do
    csrsRef <- lmCSRs <$> ask
    csrMap  <- lift $ readIORef csrsRef
    case Map.member csr csrMap of
      True -> lift $ writeIORef csrsRef (Map.insert csr csrVal csrMap)
      False -> lift $ writeIORef csrsRef (Map.insert csr csrVal csrMap) -- TODO:
               -- throw exception in this case
  setPriv privVal = LogMachineM $ do
    privRef <- lmPriv <$> ask
    lift $ writeIORef privRef privVal

  logInstruction iset inst@(Inst opcode _) = do
    return ()
    -- testMap <- LogMachineM (ioTestMap <$> ask)
    -- case MapF.lookup opcode knownCoverageMap of
    --   Nothing -> return ()
    --   Just (InstExprList exprs) -> do
    --     exprVals <- traverse (evalInstExpr iset inst 4) exprs
    --     LogMachineM $ lift $ modifyIORef testMap $ \m ->
    --       Map.insertWith (zipWith bvOr) (Some opcode) exprVals m

-- | Create an immutable copy of the register file.
freezeRegisters :: LogMachine rv
                -> IO (Array (BitVector 5) (BitVector (RVWidth rv)))
freezeRegisters = freeze . lmRegisters

-- | Create an immutable copy of the floating point register file.
freezeFRegisters :: LogMachine rv
                 -> IO (Array (BitVector 5) (BitVector (RVFloatWidth rv)))
freezeFRegisters = freeze . lmFRegisters

-- | Run the simulator for a given number of steps.
runLogMachine :: Int -> LogMachine rv -> IO Int
runLogMachine steps m = flip runReaderT m $ runLogMachineM $ runRV steps
