{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module RISCV.Instruction.Lens
  ( -- * OpBits lenses
    opcodeLens
  , funct3Lens
  , funct7Lens
  , eLens

    -- * Operands lenses
  , rdLens
  , rs1Lens
  , rs2Lens
  , imm12ILens
  , imm12SLens
  , imm12BLens
  , imm20ULens
  , imm20JLens
  , illegalLens
  ) where

import Control.Lens
import Data.BitVector.Sized
import Data.BitVector.Sized.BitLayout

-- lenses
opcodeLens :: Simple Lens (BitVector 32) (BitVector 7)
opcodeLens = layoutLens $ (chunk 0 <: empty)

funct3Lens :: Simple Lens (BitVector 32) (BitVector 3)
funct3Lens = layoutLens $ (chunk 12 <: empty)

funct7Lens :: Simple Lens (BitVector 32) (BitVector 7)
funct7Lens = layoutLens $ (chunk 25 <: empty)

eLens :: Simple Lens (BitVector 32) (BitVector 25)
eLens = layoutLens $ (chunk 7 <: empty)

rdLens :: Simple Lens (BitVector 32) (BitVector 5)
rdLens = layoutLens $ (chunk 7 <: empty)

rs1Lens :: Simple Lens (BitVector 32) (BitVector 5)
rs1Lens = layoutLens $ (chunk 15 <: empty)

rs2Lens :: Simple Lens (BitVector 32) (BitVector 5)
rs2Lens = layoutLens $ (chunk 20 <: empty)

imm12ILens :: Simple Lens (BitVector 32) (BitVector 12)
imm12ILens = layoutLens $ (chunk 20 <: empty)

imm12SLens :: Simple Lens (BitVector 32) (BitVector 12)
imm12SLens = layoutLens $ (chunk 25 :: Chunk 7) <: (chunk 7  :: Chunk 5) <: empty

imm12BLens :: Simple Lens (BitVector 32) (BitVector 12)
imm12BLens = layoutLens $
  (chunk 31 :: Chunk 1) <: (chunk 7  :: Chunk 1) <:
  (chunk 25 :: Chunk 6) <: (chunk 8  :: Chunk 4) <:
  empty

imm20ULens :: Simple Lens (BitVector 32) (BitVector 20)
imm20ULens = layoutLens $ chunk 12 <: empty

imm20JLens :: Simple Lens (BitVector 32) (BitVector 20)
imm20JLens = layoutLens $
  (chunk 31 :: Chunk 1)  <: (chunk 12 :: Chunk 8)  <:
  (chunk 20 :: Chunk 1)  <: (chunk 21 :: Chunk 10) <:
  empty

illegalLens :: Simple Lens (BitVector 32) (BitVector 32)
illegalLens = layoutLens $ chunk 0 <: empty
