{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : RISCV.Simulation.LogMachine
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
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
  , freezeMemory
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
import           Data.List (union)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Parameterized
import           Data.Traversable (for)

import RISCV.InstructionSet
import RISCV.Types
import RISCV.Simulation
import RISCV.Semantics.Exceptions

-- | IO-based machine state.
data LogMachine (arch :: BaseArch) (exts :: Extensions) = LogMachine
  { ioPC        :: IORef (BitVector (ArchWidth arch))
  , ioRegisters :: IOArray (BitVector 5) (BitVector (ArchWidth arch))
  , ioMemory    :: IOArray (BitVector (ArchWidth arch)) (BitVector 8)
  , ioCSRs      :: IORef (Map (BitVector 12) (BitVector (ArchWidth arch)))
  , ioPriv      :: IORef (BitVector 2)
  , ioMaxAddr   :: BitVector (ArchWidth arch)
  , ioException :: IORef (Maybe Exception)
  , ioSteps     :: IORef Int
  , ioTestMap   :: IORef (Map (Some (Opcode arch exts)) [[BitVector 1]])
  }

writeBS :: (Enum i, Num i, Ix i) => i -> BS.ByteString -> IOArray i (BitVector 8) -> IO ()
writeBS ix bs arr = do
  case BS.null bs of
    True -> return ()
    _    -> do
      writeArray arr ix (fromIntegral (BS.head bs))
      writeBS (ix+1) (BS.tail bs) arr

-- | Construct an LogMachine with a given maximum address, entry point, and list of
-- (addr, bytestring) pairs to load into the memory.
mkLogMachine :: forall arch exts .(KnownArch arch, KnownExtensions exts)
             => BitVector (ArchWidth arch)
             -> BitVector (ArchWidth arch)
             -> [(BitVector (ArchWidth arch), BS.ByteString)]
             -> IO (LogMachine arch exts)
mkLogMachine maxAddr entryPoint byteStrings = do
  pc        <- newIORef entryPoint
  registers <- newArray (1, 31) 0
  memory    <- newArray (0, maxAddr) 0
  csrs      <- newIORef $ Map.fromList $
    [ ]
  priv      <- newIORef 0b00
  e         <- newIORef Nothing
  steps     <- newIORef 0
  testMap   <- newIORef Map.empty -- Map.fromList (zip opcodes (repeat []))

  forM_ byteStrings $ \(addr, bs) -> do
    writeBS addr bs memory
  return (LogMachine pc registers memory csrs priv maxAddr e steps testMap)

-- | The 'LogMachineM' monad instantiates the 'RVState' monad type class, tying the
-- 'RVState' interface functions to actual transformations on the underlying mutable
-- state.
newtype LogMachineM (arch :: BaseArch) (exts :: Extensions) a =
  LogMachineM { runLogMachineM :: ReaderT (LogMachine arch exts) IO a }
  deriving (Functor, Applicative, Monad, R.MonadReader (LogMachine arch exts))

instance KnownArch arch => RVStateM (LogMachineM arch exts) arch exts where
  getPC = LogMachineM $ do
    pcRef <- ioPC <$> ask
    pcVal <- lift $ readIORef pcRef
    return pcVal
  getReg rid = LogMachineM $ do
    regArray <- ioRegisters <$> ask
    regVal   <- lift $ readArray regArray rid
    return regVal
  getMem bytes addr = LogMachineM $ do
    memArray <- ioMemory <$> ask
    maxAddr  <- ioMaxAddr <$> ask
    case addr + fromIntegral (natValue bytes) < maxAddr of
      True -> do
        val <- lift $
          for [addr..addr+(fromIntegral (natValue bytes-1))] $ \a -> readArray memArray a
        return (bvConcatManyWithRepr ((knownNat @8) `natMultiply` bytes) val)
      False -> do
        -- TODO: We need to change this code to actually execute the semantics
        -- we've pre-defined in RISCV.Semantics.Exceptions, and we can get rid
        -- of the ioException field here.
        eRef <- ioException <$> ask
        lift $ writeIORef eRef (Just LoadAccessFault)
        return (BV ((knownNat @8) `natMultiply` bytes) 0)
  getCSR csr = LogMachineM $ do
    csrsRef <- ioCSRs <$> ask
    csrMap  <- lift $ readIORef csrsRef
    let csrVal = case Map.lookup csr csrMap of
          Just val -> val
          Nothing  -> 0 -- TODO: throw exception in this case
    return csrVal
  getPriv = LogMachineM $ do
    privRef <- ioPriv <$> ask
    privVal <- lift $ readIORef privRef
    return privVal

  setPC pcVal = LogMachineM $ do
    -- We assume that every time we are setting the PC, we have just finished
    -- executing an instruction, so increment steps.
    pcRef <- ioPC <$> ask
    stepsRef <- ioSteps <$> ask
    stepsVal <- lift $ readIORef stepsRef
    lift $ writeIORef pcRef pcVal
    lift $ writeIORef stepsRef (stepsVal+1)
  setReg rid regVal = LogMachineM $ do
    regArray <- ioRegisters <$> ask
    lift $ writeArray regArray rid regVal
  setMem bytes addr val = LogMachineM $ do
    memArray <- ioMemory <$> ask
    maxAddr <- ioMaxAddr <$> ask
    case addr < maxAddr of
      True -> lift $
        for_ addrValPairs $ \(a, byte) -> writeArray memArray a byte
        where addrValPairs = zip
                [addr..addr+(fromIntegral (natValue bytes-1))]
                (bvGetBytesU (fromIntegral (natValue bytes)) val)
      False -> do
        eRef <- ioException <$> ask
        lift $ writeIORef eRef (Just StoreAccessFault)
  setCSR csr csrVal = LogMachineM $ do
    csrsRef <- ioCSRs <$> ask
    csrMap  <- lift $ readIORef csrsRef
    case Map.member csr csrMap of
      True -> lift $ writeIORef csrsRef (Map.insert csr csrVal csrMap)
      False -> lift $ writeIORef csrsRef (Map.insert csr csrVal csrMap) -- TODO:
               -- throw exception in this case
  setPriv privVal = LogMachineM $ do
    privRef <- ioPriv <$> ask
    lift $ writeIORef privRef privVal

  logInstruction (Some (Inst opcode operands)) iset = do
    return ()
    testMap <- LogMachineM (ioTestMap <$> ask)
    let formula = semanticsFromOpcode iset opcode
        tests = getTests formula
    testVals <- traverse (evalExpr operands 4) tests
    LogMachineM $ lift $ modifyIORef testMap $ \m ->
      Map.insertWith union (Some opcode) [testVals] m

-- | Create an immutable copy of the register file.
freezeRegisters :: LogMachine arch exts
                -> IO (Array (BitVector 5) (BitVector (ArchWidth arch)))
freezeRegisters = freeze . ioRegisters

-- TODO: Why does this need KnownNat (ArchWidth arch) but freezeRegisters does not?
-- | Create an immutable copy of the memory.
freezeMemory :: KnownArch arch
             => LogMachine arch exts
             -> IO (Array (BitVector (ArchWidth arch)) (BitVector 8))
freezeMemory = freeze . ioMemory

-- | Run the simulator for a given number of steps.
runLogMachine :: (KnownArch arch, KnownExtensions exts)
             => Int
             -> LogMachine arch exts
             -> IO Int
runLogMachine steps m =
  flip runReaderT m $ runLogMachineM $ runRV steps
