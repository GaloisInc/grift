{-# LANGUAGE BinaryLiterals        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module      : RISCV.Format
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module exports a data kind, Format, along with a FormatRepr datatype.
-}

module RISCV.Format
  ( -- * Instruction format
    Format(..)
  , FormatRepr(..)
  ) where

import Data.BitVector.Sized
import Data.Parameterized
import qualified Data.Parameterized.Map as Map
import Data.Parameterized.Map (MapF)
import Data.Parameterized.TH.GADT
import GHC.TypeLits

----------------------------------------
-- Formats

-- | The RISC-V instruction formats. Each RISC-V instruction has one of several
-- encoding formats, corresponding to its operands and the way those operands are
-- laid out as bits in the instruction word. We include one additional format, X,
-- inhabited only by an illegal instruction.
--
-- NOTE: Our formats differ somewhat from the RISC-V ISA manual. The manual
-- classifies instructions into formats based on <TODO: what?>. Our formats, while
-- very close to those in the manual, more exactly specify bits that are fixed by the
-- instruction (i.e. the ones that never vary, no matter what the operands of the
-- instruction are). In the manual, some instructions which have the same format
-- actually have different numbers of operands! For example: add (from RV32I) has
-- three operands, and fadd.s (from RV32F) has four operands (the additional one
-- being the rounding mode). Here, "operand" means "bit vector(s) tied to a
-- particular input for the instruction's semantics."

data Format = R | I | S | B | U | J | E | X

-- | A type-level representative of the format. Particularly useful when decoding
-- instructions since we won't know ahead of time what format to classify them as.
data FormatRepr :: Format -> * where
  RRepr :: FormatRepr 'R
  IRepr :: FormatRepr 'I
  SRepr :: FormatRepr 'S
  BRepr :: FormatRepr 'B
  URepr :: FormatRepr 'U
  JRepr :: FormatRepr 'J
  ERepr :: FormatRepr 'E
  XRepr :: FormatRepr 'X

-- Instances
$(return [])
deriving instance Show (FormatRepr k)
instance ShowF FormatRepr
deriving instance Eq (FormatRepr k)
instance EqF FormatRepr where
  eqF = (==)
instance TestEquality FormatRepr where
  testEquality = $(structuralTypeEquality [t|FormatRepr|] [])
instance OrdF FormatRepr where
  compareF = $(structuralTypeOrd [t|FormatRepr|] [])
instance KnownRepr FormatRepr 'R where knownRepr = RRepr
instance KnownRepr FormatRepr 'I where knownRepr = IRepr
instance KnownRepr FormatRepr 'S where knownRepr = SRepr
instance KnownRepr FormatRepr 'B where knownRepr = BRepr
instance KnownRepr FormatRepr 'U where knownRepr = URepr
instance KnownRepr FormatRepr 'J where knownRepr = JRepr
instance KnownRepr FormatRepr 'E where knownRepr = ERepr
instance KnownRepr FormatRepr 'X where knownRepr = XRepr
