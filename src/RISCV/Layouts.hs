{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

{-|
Module      : RISCV.Layouts
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module defines a variety of BitLayouts, which specify how smaller BitVectors
should get mapped into larger ones. These are used to specify both 1) positions of
functional bits (opcode, funct3, etc.) and 2) operand bits (rs1, rd, immediates,
rounding modes, etc.).
-}

module RISCV.Layouts
  ( -- * OpBits layouts & lenses
    opcodeLayout, opcodeLens
  , funct3Layout, funct3Lens
  , funct7Layout, funct7Lens
  , eLayout, eLens

    -- * Operand layouts & lenses
  , rdLayout, rdLens
  , rs1Layout, rs1Lens
  , rs2Layout, rs2Lens
  , imm12ILayout, imm12ILens
  , imm12SLayout, imm12SLens
  , imm12BLayout, imm12BLens
  , imm20ULayout, imm20ULens
  , imm20JLayout, imm20JLens
  , illegalLayout, illegalLens
  ) where

import Control.Lens hiding ((:<))
import Data.BitVector.Sized
import Data.BitVector.Sized.BitLayout
import Data.Parameterized.List

import RISCV.Types

-- TODO: Provide an abstraction for these. I'd like to export two lenses, opBitsLens
-- and operandsLens:

opBitsLens :: FormatRepr fmt
           -> Simple Lens (BitVector 32) (Operands fmt)
opBitsLens = undefined

operandsLens :: FormatRepr fmt
             -> Simple Lens (BitVector 32) (OpBits fmt)
operandsLens = undefined

-- OpBits layouts & lenses
opcodeLayout :: BitLayout 32 7
opcodeLayout = chunk 0 <: empty

opcodeLens :: Simple Lens (BitVector 32) (BitVector 7)
opcodeLens = layoutLens opcodeLayout

funct3Layout :: BitLayout 32 3
funct3Layout = chunk 12 <: empty

funct3Lens :: Simple Lens (BitVector 32) (BitVector 3)
funct3Lens = layoutLens funct3Layout

funct7Layout :: BitLayout 32 7
funct7Layout = chunk 25 <: empty

funct7Lens :: Simple Lens (BitVector 32) (BitVector 7)
funct7Lens = layoutLens funct7Layout

eLayout :: BitLayout 32 25
eLayout = chunk 7 <: empty

eLens :: Simple Lens (BitVector 32) (BitVector 25)
eLens = layoutLens eLayout

-- Operand layouts & lenses
rdLayout :: BitLayout 32 5
rdLayout = chunk 7 <: empty

rdLens :: Simple Lens (BitVector 32) (BitVector 5)
rdLens = layoutLens rdLayout

rs1Layout :: BitLayout 32 5
rs1Layout = chunk 15 <: empty

rs1Lens :: Simple Lens (BitVector 32) (BitVector 5)
rs1Lens = layoutLens rs1Layout

rs2Layout :: BitLayout 32 5
rs2Layout = chunk 20 <: empty

rs2Lens :: Simple Lens (BitVector 32) (BitVector 5)
rs2Lens = layoutLens rs2Layout

imm12ILayout :: BitLayout 32 12
imm12ILayout = chunk 20 <: empty

imm12ILens :: Simple Lens (BitVector 32) (BitVector 12)
imm12ILens = layoutLens imm12ILayout

imm12SLayout :: BitLayout 32 12
imm12SLayout = (chunk 25 :: Chunk 7) <: (chunk 7  :: Chunk 5) <: empty

imm12SLens :: Simple Lens (BitVector 32) (BitVector 12)
imm12SLens = layoutLens imm12SLayout

imm12BLayout :: BitLayout 32 12
imm12BLayout =
  (chunk 31 :: Chunk 1) <: (chunk 7  :: Chunk 1) <:
  (chunk 25 :: Chunk 6) <: (chunk 8  :: Chunk 4) <:
  empty

imm12BLens :: Simple Lens (BitVector 32) (BitVector 12)
imm12BLens = layoutLens imm12BLayout

imm20ULayout :: BitLayout 32 20
imm20ULayout = chunk 12 <: empty

imm20ULens :: Simple Lens (BitVector 32) (BitVector 20)
imm20ULens = layoutLens imm20ULayout

imm20JLayout :: BitLayout 32 20
imm20JLayout =
  (chunk 31 :: Chunk 1)  <: (chunk 12 :: Chunk 8)  <:
  (chunk 20 :: Chunk 1)  <: (chunk 21 :: Chunk 10) <:
  empty

imm20JLens :: Simple Lens (BitVector 32) (BitVector 20)
imm20JLens = layoutLens imm20JLayout

illegalLayout :: BitLayout 32 32
illegalLayout = chunk 0 <: empty

illegalLens :: Simple Lens (BitVector 32) (BitVector 32)
illegalLens = layoutLens illegalLayout
