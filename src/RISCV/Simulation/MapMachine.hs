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
  , mkMachine
  , MapMachineM
  , runMapMachine
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
data MapMachine (arch :: BaseArch) (exts :: Extensions) = MapMachine
  { pc        :: BitVector (ArchWidth arch)
  , registers :: Map (BitVector 5) (BitVector (ArchWidth arch))
  , memory    :: Map (BitVector (ArchWidth arch)) (BitVector 8)
  , csrs      :: Map (BitVector 12) (BitVector (ArchWidth arch))
  , priv      :: BitVector 2
  , maxAddr   :: BitVector (ArchWidth arch)
  , exception :: Maybe Exception
  , steps     :: Int
  }

-- | Construct an MapMachine with a given maximum address, entry point, and list of
-- (addr, bytestring) pairs to load into the memory.
mkMachine :: (KnownArch arch, KnownExtensions exts)
          => BitVector (ArchWidth arch)
          -> BitVector (ArchWidth arch)
          -> [(BitVector (ArchWidth arch), BS.ByteString)]
          -> MapMachine arch exts
mkMachine maxAddr' entryPoint byteStrings =
  MapMachine { pc = entryPoint
             , registers = Map.fromList (zip [1..31] (repeat 0))
             , memory = Map.fromList memoryAssocs
             , csrs = Map.empty
             , priv = 0b00
             , maxAddr = maxAddr'
             , exception = Nothing
             , steps = 0
             }
  where memoryAssocs = concat (map zipBS byteStrings)
        zipBS (start, bs) = zip [start..] (map (bitVector . fromIntegral) (BS.unpack bs))

-- | The 'MapMachineM' monad instantiates the 'RVState' monad type class, tying the
-- 'RVState' interface functions to actual transformations on the underlying mutable
-- state.
newtype MapMachineM (arch :: BaseArch) (exts :: Extensions) a =
  MapMachineM { runMapMachineM :: Reader (MapMachine arch exts) a }
  deriving (Functor, Applicative, Monad, R.MonadReader (MapMachine arch exts))

instance KnownArch arch => RVStateM (MapMachineM arch exts) arch exts where
  getPC = undefined
  getReg rid = undefined
  getMem addr = undefined
  getCSR csr = undefined
  getPriv = undefined

  setPC pcVal = undefined
  setReg rid regVal = undefined
  setMem addr byte = undefined
  setCSR csr csrVal = undefined
  setPriv privVal = undefined

  logInstruction _ _ = return ()

-- | Run the simulator for a given number of steps.
runMapMachine :: (KnownArch arch, KnownExtensions exts)
             => Int
             -> MapMachine arch exts
             -> Int
runMapMachine steps m = undefined
