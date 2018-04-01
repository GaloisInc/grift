{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : RISCV.Simulation.STMachine
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

An ST-based simulation backend for RISC-V machines.
-}

module RISCV.Simulation.STMachine
  ( STMachine(..)
  , STMachineM(..)
  ) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Control.Monad.ST
import           Data.Array.ST
import           Data.BitVector.Sized
import           Data.Parameterized
import           Data.STRef

import RISCV.Instruction
import RISCV.Simulation

byteSize :: NatRepr 8
byteSize = knownRepr

data STMachine s (arch :: BaseArch) (exts :: Extensions) = STMachine
  { stPC        :: STRef s (BitVector (ArchWidth arch))
  , stRegisters :: STArray s (BitVector 5) (BitVector (ArchWidth arch))
  , stMemory    :: STArray s (BitVector (ArchWidth arch)) (BitVector 8)
  , stMaxAddr   :: BitVector (ArchWidth arch)
  }

newtype STMachineM s (arch :: BaseArch) (exts :: Extensions) a =
  STMachineM { runSTMachineM :: ReaderT (STMachine s arch exts) (ST s) a }
  deriving (Functor, Applicative, Monad)

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
  getMem bRepr addr = STMachineM $ do
    let numBytes = bitVector (natValue bRepr)
    memArray <- stMemory <$> ask
    bytes <- forM [addr .. addr + numBytes - 1] (lift . readArray memArray)
    return (bvConcatManyWithRepr (byteSize `natMultiply` bRepr) bytes)

  setPC pcVal = STMachineM $ do
    pcRef <- stPC <$> ask
    lift $ writeSTRef pcRef pcVal
  setReg rid regVal = STMachineM $ do
    regArray <- stRegisters <$> ask
    lift $ writeArray regArray rid regVal
  setMem _ 0 _ = return ()
  setMem bRepr addr bv = STMachineM $ do
    let numBytes = natValue bRepr
        writes   = zip [ addr .. addr + (bitVector numBytes) - 1] (bvGetBytesU (fromIntegral numBytes) bv)
    memArray <- stMemory <$> ask
    forM_ writes (lift . uncurry (writeArray memArray))
