{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-|
Module      : RISCV.Latency
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Experimental -- we are attaching cycle counts to each instruction, which will be
computed during simulation.
-}

module RISCV.Latency where

import Data.Parameterized.Map (MapF)

import RISCV.Semantics
import RISCV.Types

newtype Flip t a b = Flip (t b a)

type LatencyMap arch = MapF (Opcode arch) (Flip (Expr arch) (ArchWidth arch))
