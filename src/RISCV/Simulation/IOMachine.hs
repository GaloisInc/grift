{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

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

module RISCV.Simulation.IOMachine where

import           Control.Monad (forM_)
import qualified Control.Monad.Reader.Class as R
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.Array.IArray
import           Data.Array.IO
import           Data.BitVector.Sized
import qualified Data.ByteString as BS
import           Data.IORef
import           GHC.TypeLits

import RISCV.Types
import RISCV.Simulation
import RISCV.Semantics (Exception)

data IOMachine (arch :: BaseArch) (exts :: Extensions) = IOMachine
  { ioPC        :: IORef (BitVector (ArchWidth arch))
  , ioRegisters :: IOArray (BitVector 5) (BitVector (ArchWidth arch))
  , ioMemory    :: IOArray (BitVector (ArchWidth arch)) (BitVector 8)
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
  e         <- newIORef Nothing
  steps     <- newIORef 0

  forM_ byteStrings $ \(addr, bs) -> do
    writeBS addr bs memory
  return (IOMachine pc registers memory maxAddr e steps)

-- | The 'IOMachineM' monad instantiates the 'RVState' monad type class, tying the
-- 'RVState' interface functions to actual transformations on the underlying mutable
-- state.
newtype IOMachineM (arch :: BaseArch) (exts :: Extensions) a =
  IOMachineM { runIOMachineM :: ReaderT (IOMachine arch exts) IO a }
  deriving (Functor, Applicative, Monad, R.MonadReader (IOMachine arch exts))

-- TODO: add dynamic checking for memory out of bounds; just throw an error for now
instance KnownNat (ArchWidth arch) => RVState (IOMachineM arch exts) arch exts where
  getPC = IOMachineM $ do
    pcRef <- ioPC <$> ask
    pcVal <- lift $ readIORef pcRef
    return pcVal
  getReg 0 = return 0 -- rid 0 is hardwired to the constant 0.
  getReg rid = IOMachineM $ do
    regArray <- ioRegisters <$> ask
    regVal   <- lift $ readArray regArray rid
    return regVal
  getMem addr = IOMachineM $ do
    memArray <- ioMemory <$> ask
    byte     <- lift $ readArray memArray addr
    return byte

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
  setMem 0 _ = return ()
  setMem addr byte = IOMachineM $ do
    memArray <- ioMemory <$> ask
    lift $ writeArray memArray addr byte

  throwException e = IOMachineM $ do
    eRef <- ioException <$> ask
    lift $ writeIORef eRef (Just e)

  exceptionStatus = IOMachineM $ do
    eRef <- ioException <$> ask
    lift $ readIORef eRef

-- | Run a IOMachineM transformation on an initial state and return the result
execIOMachine :: (KnownArch arch, KnownExtensions exts)
              => IOMachineM arch exts (Maybe Exception)
              -> IOMachine arch exts
              -> IO ( BitVector (ArchWidth arch)
                    , Array (BitVector 5) (BitVector (ArchWidth arch))
                    , Array (BitVector (ArchWidth arch)) (BitVector 8)
                    , Int
                    , Maybe Exception
                    )
execIOMachine action m = do
  (ioPC', ioRegisters', ioMemory', ioSteps', e') <- flip runReaderT m $ runIOMachineM $ do
    e <- action
    m' <- R.ask
    return (ioPC m', ioRegisters m', ioMemory m', ioSteps m', e)
  pc <- readIORef ioPC'
  registers <- freeze ioRegisters'
  memory <- freeze ioMemory'
  steps <- readIORef ioSteps'
  return (pc, registers, memory, steps, e')
