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

import           Control.Monad
import qualified Control.Monad.Reader.Class as R
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Control.Monad.ST
import           Data.Array.IArray
import           Data.Array.ST
import           Data.BitVector.Sized
import qualified Data.ByteString as BS
import           Data.Parameterized
import           Data.STRef
import           GHC.TypeLits

import RISCV.Simulation
import RISCV.Types

byteSize :: NatRepr 8
byteSize = knownRepr

-- | An ST-based backend for a RISC-V simulator.
data STMachine s (arch :: BaseArch) (exts :: Extensions) = STMachine
  { stPC        :: STRef s (BitVector (ArchWidth arch))
  , stRegisters :: STArray s (BitVector 5) (BitVector (ArchWidth arch))
  , stMemory    :: STArray s (BitVector (ArchWidth arch)) (BitVector 8)
  , stMaxAddr   :: BitVector (ArchWidth arch)
  , stHalted    :: STRef s Bool
  }

writeBS :: (Enum i, Num i, Ix i) => i -> BS.ByteString -> STArray s i (BitVector 8) -> ST s ()
writeBS ix bs arr = do
  case BS.null bs of
    True -> return ()
    _    -> do
      writeArray arr ix (fromIntegral (BS.head bs))
      writeBS (ix+1) (BS.tail bs) arr

-- | Construct an STMachine with a given maximum address and program.
mkSTMachine :: KnownNat (ArchWidth arch)
            => BaseArchRepr arch
            -> ExtensionsRepr exts
            -> BitVector (ArchWidth arch)
            -> BS.ByteString
            -> ST s (STMachine s arch exts)
mkSTMachine _ _ maxAddr progBytes = do
  pc        <- newSTRef 0
  registers <- newArray (1, 31) 0
  memory    <- newArray (0, maxAddr) 0
  halted    <- newSTRef False

  writeBS 0 progBytes memory
  return (STMachine pc registers memory maxAddr halted)

-- | Run a STMachineM transformation on an initial state and return the result
execSTMachine :: (KnownArch arch, KnownExtensions exts)
              => STMachineM s arch exts ()
              -> STMachine s arch exts
              -> ST s ( BitVector (ArchWidth arch)
                      , Array (BitVector 5) (BitVector (ArchWidth arch))
                      , Array (BitVector (ArchWidth arch)) (BitVector 8)
                      )
execSTMachine action m = do
  (stPC', stRegisters', stMemory') <- flip runReaderT m $ runSTMachineM $ do
    action
    m' <- R.ask
    return (stPC m', stRegisters m', stMemory m')
  pc <- readSTRef stPC'
  registers <- freeze stRegisters'
  memory <- freeze stMemory'
  return (pc, registers, memory)

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
  getMem bRepr addr = STMachineM $ do
    let numBytes = bitVector (natValue bRepr)
    memArray <- stMemory <$> ask
    bytes <- forM [addr .. addr + numBytes - 1] (lift . readArray memArray)
    return (bvConcatManyWithRepr (byteSize `natMultiply` bRepr) bytes)

  setPC pcVal = STMachineM $ do
    pcRef <- stPC <$> ask
    lift $ writeSTRef pcRef pcVal
  setReg 0 _ = return ()
  setReg rid regVal = STMachineM $ do
    regArray <- stRegisters <$> ask
    lift $ writeArray regArray rid regVal
  setMem _ 0 _ = return ()
  setMem bRepr addr bv = STMachineM $ do
    let numBytes = natValue bRepr
        writes   = zip [ addr .. addr + (bitVector numBytes) - 1] (bvGetBytesU (fromIntegral numBytes) bv)
    memArray <- stMemory <$> ask
    forM_ writes (lift . uncurry (writeArray memArray))

  throwException _ = STMachineM $ do
    haltedRef <- stHalted <$> ask
    lift $ writeSTRef haltedRef True

  isHalted = STMachineM $ do
    haltedRef <- stHalted <$> ask
    lift $ readSTRef haltedRef
