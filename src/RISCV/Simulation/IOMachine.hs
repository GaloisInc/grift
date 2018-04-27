{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : RISCV.Simulation.IOMachine
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

An IO-based simulation backend for RISC-V machines. We use the 'BitVector' type
directly for the underlying values, which allows us to keep the architecture width
unspecified.
-}

module RISCV.Simulation.IOMachine
  ( IOMachine(..)
  , mkIOMachine
  , IOMachineM
  , freezeRegisters
  , freezeMemory
  , runIOMachine
  ) where

import           Control.Monad (forM_)
import qualified Control.Monad.Reader.Class as R
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.Array.IArray
import           Data.Array.IO
import           Data.BitVector.Sized
import qualified Data.ByteString as BS
import           Data.IORef

import RISCV.Types
import RISCV.Simulation
import RISCV.Semantics (Exception(..))

-- | IO-based machine state.
data IOMachine (arch :: BaseArch) (exts :: Extensions) = IOMachine
  { ioPC        :: IORef (BitVector (ArchWidth arch))
  , ioRegisters :: IOArray (BitVector 5) (BitVector (ArchWidth arch))
  , ioMemory    :: IOArray (BitVector (ArchWidth arch)) (BitVector 8)
  , ioPriv      :: IORef (BitVector 2)
  , ioMaxAddr   :: BitVector (ArchWidth arch)
  , ioException :: IORef (Maybe Exception)
  , ioSteps     :: IORef Int
  }

writeBS :: (Enum i, Num i, Ix i) => i -> BS.ByteString -> IOArray i (BitVector 8) -> IO ()
writeBS ix bs arr = do
  case BS.null bs of
    True -> return ()
    _    -> do
      writeArray arr ix (fromIntegral (BS.head bs))
      writeBS (ix+1) (BS.tail bs) arr

-- | Construct an IOMachine with a given maximum address, entry point, and list of
-- (addr, bytestring) pairs to load into the memory.
mkIOMachine :: (KnownArch arch, KnownExtensions exts)
            => BitVector (ArchWidth arch)
            -> BitVector (ArchWidth arch)
            -> [(BitVector (ArchWidth arch), BS.ByteString)]
            -> IO (IOMachine arch exts)
mkIOMachine maxAddr entryPoint byteStrings = do
  pc        <- newIORef entryPoint
  registers <- newArray (1, 31) 0
  memory    <- newArray (0, maxAddr) 0
  priv      <- newIORef 0b00
  e         <- newIORef Nothing
  steps     <- newIORef 0

  forM_ byteStrings $ \(addr, bs) -> do
    writeBS addr bs memory
  return (IOMachine pc registers memory priv maxAddr e steps)

-- | The 'IOMachineM' monad instantiates the 'RVState' monad type class, tying the
-- 'RVState' interface functions to actual transformations on the underlying mutable
-- state.
newtype IOMachineM (arch :: BaseArch) (exts :: Extensions) a =
  IOMachineM { runIOMachineM :: ReaderT (IOMachine arch exts) IO a }
  deriving (Functor, Applicative, Monad, R.MonadReader (IOMachine arch exts))

instance KnownArch arch => RVStateM (IOMachineM arch exts) arch exts where
  getPC = IOMachineM $ do
    pcRef <- ioPC <$> ask
    pcVal <- lift $ readIORef pcRef
    return pcVal
  getReg rid = IOMachineM $ do
    regArray <- ioRegisters <$> ask
    regVal   <- lift $ readArray regArray rid
    return regVal
  getMem addr = IOMachineM $ do
    memArray <- ioMemory <$> ask
    maxAddr <- ioMaxAddr <$> ask
    case addr < maxAddr of
      True -> do byte <- lift $ readArray memArray addr
                 return byte
      False -> do
        eRef <- ioException <$> ask
        lift $ writeIORef eRef (Just MemoryAccessError)
        return 0
  getPriv = IOMachineM $ do
    privRef <- ioPriv <$> ask
    privVal <- lift $ readIORef privRef
    return privVal

  setPC pcVal = IOMachineM $ do
    -- We assume that every time we are setting the PC, we have just finished
    -- executing an instruction, so increment steps.
    pcRef <- ioPC <$> ask
    stepsRef <- ioSteps <$> ask
    stepsVal <- lift $ readIORef stepsRef
    lift $ writeIORef pcRef pcVal
    lift $ writeIORef stepsRef (stepsVal+1)
  setReg 0 _ = return ()
  setReg rid regVal = IOMachineM $ do
    regArray <- ioRegisters <$> ask
    lift $ writeArray regArray rid regVal
  setMem addr byte = IOMachineM $ do
    memArray <- ioMemory <$> ask
    maxAddr <- ioMaxAddr <$> ask
    case addr < maxAddr of
      True -> lift $ writeArray memArray addr byte
      False -> do
        eRef <- ioException <$> ask
        lift $ writeIORef eRef (Just MemoryAccessError)
  setPriv privVal = IOMachineM $ do
    privRef <- ioPriv <$> ask
    lift $ writeIORef privRef privVal

  throwException e = IOMachineM $ do
    eRef <- ioException <$> ask
    lift $ writeIORef eRef (Just e)

  exceptionStatus = IOMachineM $ do
    eRef <- ioException <$> ask
    lift $ readIORef eRef

-- | Create an immutable copy of the register file.
freezeRegisters :: IOMachine arch exts
                -> IO (Array (BitVector 5) (BitVector (ArchWidth arch)))
freezeRegisters = freeze . ioRegisters

-- TODO: Why does this need KnownNat (ArchWidth arch) but freezeRegisters does not?
-- | Create an immutable copy of the memory.
freezeMemory :: KnownArch arch
             => IOMachine arch exts
             -> IO (Array (BitVector (ArchWidth arch)) (BitVector 8))
freezeMemory = freeze . ioMemory

-- | Run the simulator for a given number of steps.
runIOMachine :: (KnownArch arch, KnownExtensions exts)
             => Int
             -> IOMachine arch exts
             -> IO Int
runIOMachine steps m =
  flip runReaderT m $ runIOMachineM $ runRV steps
