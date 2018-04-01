{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : MainSimulator
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Tool for simulating RISC-V programs in the ELF executable format.
-}

module Main where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Bits
import           Data.BitVector.Sized
import           Data.Bool
import           Data.Monoid
import           Data.Parameterized
import           Data.STRef
import           System.Environment
import           System.Exit

import           RISCV

type W = ArchWidth 'RV32I

byteSize :: NatRepr 8
byteSize = knownRepr

data Machine s = Machine
  { mPC        :: STRef s (BitVector W)
  , mRegisters :: STArray s (BitVector 5) (BitVector W)
  , mMemory    :: STArray s (BitVector W) (BitVector 8)
  }

newtype MachineM s a = MachineM { runMachineM :: ReaderT (Machine s) (ST s) a }
  deriving (Functor, Applicative, Monad)

instance RVState (MachineM s) 'RV32I ('Extensions '(MYes, FDNo)) where
  getPC = MachineM $ do
    pcRef <- mPC <$> ask
    pcVal <- lift $ readSTRef pcRef
    return pcVal
  getReg 0 = return 0 -- rid 0 is hardwired to the constant 0.
  getReg rid = MachineM $ do
    regArray <- mRegisters <$> ask
    regVal   <- lift $ readArray regArray rid
    return regVal
  getMem bRepr addr = MachineM $ do
    let numBytes = bitVector (natValue bRepr)
    memArray <- mMemory <$> ask
    bytes <- forM [addr .. addr + numBytes - 1] (lift . readArray memArray)
    return (bvConcatManyWithRepr (byteSize `natMultiply` bRepr) bytes)

  setPC pcVal = MachineM $ do
    pcRef <- mPC <$> ask
    lift $ writeSTRef pcRef pcVal
  setReg rid regVal = MachineM $ do
    regArray <- mRegisters <$> ask
    lift $ writeArray regArray rid regVal
  setMem _ 0 _ = return ()
  setMem bRepr addr bv = MachineM $ do
    let numBytes = natValue bRepr
        writes   = zip [ addr .. addr + (bitVector numBytes) - 1] (bvGetBytesU (fromIntegral numBytes) bv)
    memArray <- mMemory <$> ask
    forM_ writes (lift . uncurry (writeArray memArray))

main :: IO ()
main = putStrLn "Simulator not yet implemented"

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

