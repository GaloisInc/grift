{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-|
Module      : RISCV.Extensions.Priv
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

RISC-V Privileged architecture extensions
-}

module RISCV.Extensions.Priv
  ( privm
  ) where

import Data.BitVector.Sized
import Data.BitVector.Sized.App
import Data.Monoid
import qualified Data.Parameterized.Map as Map
import Data.Parameterized
import Data.Parameterized.List

import RISCV.Extensions.Helpers
import RISCV.InstructionSet
import RISCV.Semantics
import RISCV.Semantics.Exceptions
import RISCV.Types

privm :: KnownArch arch => InstructionSet arch exts
privm = instructionSet privmEncode privmSemantics

privmEncode :: EncodeMap arch exts
privmEncode = Map.fromList
  [ Pair Mret (OpBits PRepr (0b00110000001000000000000001110011 :< Nil))
  , Pair Wfi  (OpBits PRepr (0b00010000010100000000000001110011 :< Nil))
  ]

privmSemantics :: KnownArch arch => SemanticsMap arch exts
privmSemantics = Map.fromList
  [ Pair Mret $ InstFormula $ getFormula $ do
      comment "Returns from a machine-mode exception handler."

      -- TODO: Need to add the rest of the behavior here.
      let mepc = readCSR (litBV $ encodeCSR MEPC)

      assignPC mepc
  , Pair Wfi $ InstFormula $ getFormula $ do
      comment ""

      -- Doesn't do anything yet.
      incrPC
  ]
