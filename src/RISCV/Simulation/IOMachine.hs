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


import qualified Control.Monad.Reader.Class as R
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.Array.IArray
import           Data.Array.IO
import           Data.BitVector.Sized
import qualified Data.ByteString as BS
import           Data.IORef
import           GHC.TypeLits

-- data IOMachine (arch :: BaseArch) (exts :: Extensions) = IOMachine
--   { ioPC        :: IORef (BitVector (ArchWidth arch))
--   , ioRegisters :: IOArray (BitVector 5) (BitVector (ArchWidth arch))
--   , ioMemory    :: IOArray (BitVector (ArchWidth arch)) (BitVector 8)
--   , ioMaxAddr   :: BitVector (ArchWidth arch)
--   , ioException :: IORef (Maybe Exception)
--   , ioSteps     :: IORef Int
--   }
