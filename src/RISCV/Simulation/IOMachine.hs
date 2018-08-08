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
License     : AGPLv3
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
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Parameterized

import RISCV.Types
import RISCV.Simulation
import RISCV.Semantics.Exceptions

-- | IO-based machine state.
data IOMachine (arch :: BaseArch) (exts :: Extensions) = IOMachine
  { ioPC        :: IORef (BitVector (ArchWidth arch))
  , ioRegisters :: IOArray (BitVector 5) (BitVector (ArchWidth arch))
  , ioMemory    :: IOArray (BitVector (ArchWidth arch)) (BitVector 8)
  , ioCSRs      :: IORef (Map (BitVector 12) (BitVector (ArchWidth arch)))
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
  csrs      <- newIORef $ Map.fromList $
    [ ]
  priv      <- newIORef 0b00
  e         <- newIORef Nothing
  steps     <- newIORef 0

  forM_ byteStrings $ \(addr, bs) -> do
    writeBS addr bs memory
  return (IOMachine pc registers memory csrs priv maxAddr e steps)

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
        lift $ writeIORef eRef (Just LoadAccessFault)
        return 0
  getCSR csr = IOMachineM $ do
    csrsRef <- ioCSRs <$> ask
    csrMap  <- lift $ readIORef csrsRef
    let csrVal = case Map.lookup csr csrMap of
          Just val -> val
          Nothing  -> 0 -- TODO: throw exception in this case
    return csrVal
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
        lift $ writeIORef eRef (Just StoreAccessFault)
  setCSR csr csrVal = IOMachineM $ do
    csrsRef <- ioCSRs <$> ask
    csrMap  <- lift $ readIORef csrsRef
    case Map.member csr csrMap of
      True -> lift $ writeIORef csrsRef (Map.insert csr csrVal csrMap)
      False -> lift $ writeIORef csrsRef (Map.insert csr csrVal csrMap) -- TODO:
               -- throw exception in this case
  setPriv privVal = IOMachineM $ do
    privRef <- ioPriv <$> ask
    lift $ writeIORef privRef privVal

  logInstruction _ _ = return ()

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
