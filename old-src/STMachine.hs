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

{-|
Module      : RISCV.Simulation.STMachine
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

An ST-based simulation backend for RISC-V machines. We use the 'BitVector' type
directly for the underlying values, which allows us to keep the architecture width
unspecified.
-}

module RISCV.Simulation.STMachine
  ( STMachine(..)
  , mkSTMachine
  , execSTMachine
  , STMachineM(..)
  ) where

import           Control.Monad (forM_)
import qualified Control.Monad.Reader.Class as R
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Control.Monad.ST
import           Data.Array.IArray
import           Data.Array.ST
import           Data.BitVector.Sized
import qualified Data.ByteString as BS
import           Data.STRef

import RISCV.Semantics (Exception)
import RISCV.Simulation
import RISCV.Types

-- | An ST-based backend for a RISC-V simulator.
data STMachine s (arch :: BaseArch) (exts :: Extensions) = STMachine
  { stPC        :: STRef s (BitVector (ArchWidth arch))
  , stRegisters :: STArray s (BitVector 5) (BitVector (ArchWidth arch))
  , stMemory    :: STArray s (BitVector (ArchWidth arch)) (BitVector 8)
  , stPriv      :: STRef s (BitVector 2)
  , stMaxAddr   :: BitVector (ArchWidth arch)
  , stException :: STRef s (Maybe Exception)
  , stSteps     :: STRef s Int
  }

writeBS :: (Enum i, Num i, Ix i) => i -> BS.ByteString -> STArray s i (BitVector 8) -> ST s ()
writeBS ix bs arr = do
  case BS.null bs of
    True -> return ()
    _    -> do
      writeArray arr ix (fromIntegral (BS.head bs))
      writeBS (ix+1) (BS.tail bs) arr

-- | Construct an STMachine with a given maximum address, entry point, and list of
-- (addr, bytestring) pairs to load into the memory.
mkSTMachine :: KnownArch arch
            => BaseArchRepr arch
            -> ExtensionsRepr exts
            -> BitVector (ArchWidth arch)
            -> BitVector (ArchWidth arch)
            -> [(BitVector (ArchWidth arch), BS.ByteString)]
            -> ST s (STMachine s arch exts)
mkSTMachine _ _ maxAddr entryPoint byteStrings = do
  pc        <- newSTRef entryPoint
  registers <- newArray (1, 31) 0
  memory    <- newArray (0, maxAddr) 0
  priv      <- newSTRef 0b00
  e         <- newSTRef Nothing
  steps     <- newSTRef 0

  forM_ byteStrings $ \(addr, bs) -> do
    writeBS addr bs memory
  return (STMachine pc registers memory priv maxAddr e steps)

-- | Run a STMachineM transformation on an initial state and return the result
execSTMachine :: (KnownArch arch, KnownExtensions exts)
              => STMachineM s arch exts ()
              -> STMachine s arch exts
              -> ST s ( BitVector (ArchWidth arch)
                      , Array (BitVector 5) (BitVector (ArchWidth arch))
                      , Array (BitVector (ArchWidth arch)) (BitVector 8)
                      , BitVector 2
                      , Int
                      )
execSTMachine action m = do
  (stPC', stRegisters', stMemory', stPriv', stSteps') <- flip runReaderT m $ runSTMachineM $ do
    action
    m' <- R.ask
    return (stPC m', stRegisters m', stMemory m', stPriv m', stSteps m')
  pc <- readSTRef stPC'
  registers <- freeze stRegisters'
  memory <- freeze stMemory'
  priv <- readSTRef stPriv'
  steps <- readSTRef stSteps'
  return (pc, registers, memory, priv, steps)

-- | The 'STMachineM' monad instantiates the 'RVState' monad type class, tying the
-- 'RVState' interface functions to actual transformations on the underlying mutable
-- state.
newtype STMachineM s (arch :: BaseArch) (exts :: Extensions) a =
  STMachineM { runSTMachineM :: ReaderT (STMachine s arch exts) (ST s) a }
  deriving (Functor, Applicative, Monad, R.MonadReader (STMachine s arch exts))

-- TODO: add dynamic checking for memory out of bounds; just throw an error for now
instance KnownArch arch => RVState (STMachineM s arch exts) arch exts where
  getPC = STMachineM $ do
    pcRef <- stPC <$> ask
    pcVal <- lift $ readSTRef pcRef
    return pcVal
  getReg 0 = return 0 -- rid 0 is hardwired to the constant 0.
  getReg rid = STMachineM $ do
    regArray <- stRegisters <$> ask
    regVal   <- lift $ readArray regArray rid
    return regVal
  getMem addr = STMachineM $ do
    memArray <- stMemory <$> ask
    byte     <- lift $ readArray memArray addr
    return byte
  getPriv = STMachineM $ do
    privRef <- stPriv <$> ask
    privVal <- lift $ readSTRef privRef
    return privVal

  setPC pcVal = STMachineM $ do
    pcRef <- stPC <$> ask
    stepsRef <- stSteps <$> ask
    stepsVal <- lift $ readSTRef stepsRef
    lift $ writeSTRef pcRef pcVal
    lift $ writeSTRef stepsRef (stepsVal+1)
  setReg 0 _ = return ()
  setReg rid regVal = STMachineM $ do
    regArray <- stRegisters <$> ask
    lift $ writeArray regArray rid regVal
  setMem 0 _ = return ()
  setMem addr byte = STMachineM $ do
    memArray <- stMemory <$> ask
    lift $ writeArray memArray addr byte
  setPriv privVal = STMachineM $ do
    privRef <- stPriv <$> ask
    lift $ writeSTRef privRef privVal

  throwException e = STMachineM $ do
    eRef <- stException <$> ask
    lift $ writeSTRef eRef (Just e)

  exceptionStatus = STMachineM $ do
    eRef <- stException <$> ask
    lift $ readSTRef eRef
