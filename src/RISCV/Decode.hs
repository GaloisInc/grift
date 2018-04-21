{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : RISCV.Decode
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module defines a function 'decode' that converts a 'BitVector' @32@ an internal
'Instruction', as well as the corresponding reverse 'encode' function.
-}

module RISCV.Decode
  ( -- * Functions
    decode
  , encode
  ) where

import Control.Lens hiding ( (:<), Index, imap, op, iset )
import Data.BitVector.Sized
import Data.BitVector.Sized.BitLayout
import Data.Parameterized
import Data.Parameterized.List
import GHC.TypeLits

import RISCV.InstructionSet
import RISCV.Types

izipWith :: forall a b c sh . (forall tp. Index sh tp -> a tp -> b tp -> c tp)
         -> List a sh
         -> List b sh
         -> List c sh
izipWith f = go id
  where
    go :: forall sh' .
          (forall tp . Index sh' tp -> Index sh tp)
       -> List a sh'
       -> List b sh'
       -> List c sh'
    go g as bs =
      case (as, bs) of
        (Nil, Nil) -> Nil
        (a :< as', b :< bs') ->
          f (g IndexHere) a b :< go (g . IndexThere) as' bs'

-- This would be easier if we could combine it with izipWith and Pair, but Pair hides
-- its type parameter!
ifoldr2 :: forall a b c sh .
           (forall tp. Index sh tp -> a tp -> b tp -> c -> c)
        -> c
        -> List a sh
        -> List b sh
        -> c
ifoldr2 f seed0 = go id seed0
  where
    go :: forall sh' .
          (forall tp . Index sh' tp -> Index sh tp)
       -> c
       -> List a sh'
       -> List b sh'
       -> c
    go g c as bs =
      case (as, bs) of
        (Nil, Nil) -> c
        (a :< as', b :< bs') -> f (g IndexHere) a b (go (g . IndexThere) c as' bs')

-- | Convert a 'List' of 'BitLayout's to a lens of a 'List' of 'BitVector's.
layoutsLens :: forall ws . List (BitLayout 32) ws -> Simple Lens (BitVector 32) (List BitVector ws)
layoutsLens layouts = lens
  (\bv -> imap (const $ flip extract bv) layouts)
  (\bv bvFlds -> ifoldr2 (\_ fld layout bv' -> inject layout bv' fld) bv bvFlds layouts)

simpleLayout :: (KnownNat w', KnownNat w) => Int -> BitLayout w w'
simpleLayout x = chunk x <: empty

-- | Given a format, get the 'BitLayout's for the 'OpBits' of that format.
opBitsLayouts :: FormatRepr fmt -> List (BitLayout 32) (OpBitsTypes fmt)
opBitsLayouts repr = case repr of
  RRepr -> opcode :< funct3 :< funct7 :< Nil
  IRepr -> opcode :< funct3 :< Nil
  SRepr -> opcode :< funct3 :< Nil
  BRepr -> opcode :< funct3 :< Nil
  URepr -> opcode :< Nil
  JRepr -> opcode :< Nil
  XRepr -> Nil
  where funct3 :: BitLayout 32 3
        funct3 = simpleLayout 12
        funct7 :: BitLayout 32 7
        funct7 = simpleLayout 25

-- | Given a format, get the 'BitLayout's for the 'Operands' of that format.
operandsLayouts :: FormatRepr fmt -> List (BitLayout 32) (OperandTypes fmt)
operandsLayouts repr = case repr of
  RRepr -> rdLayout  :< rs1Layout :< rs2Layout :< Nil
  IRepr -> rdLayout  :< rs1Layout :< imm12ILayout :< Nil
  SRepr -> rs1Layout :< rs2Layout :< imm12SLayout :< Nil
  BRepr -> rs1Layout :< rs2Layout :< imm12BLayout :< Nil
  URepr -> rdLayout  :< imm20ULayout :< Nil
  JRepr -> rdLayout  :< imm20JLayout :< Nil
  XRepr -> illegalLayout :< Nil

  where rdLayout     :: BitLayout 32 5  = simpleLayout 7
        rs1Layout    :: BitLayout 32 5 = simpleLayout 15
        rs2Layout    :: BitLayout 32 5 = simpleLayout 20
        imm12ILayout :: BitLayout 32 12 = simpleLayout 20
        imm12SLayout :: BitLayout 32 12 = (chunk 25 :: Chunk 7) <: (chunk 7  :: Chunk 5) <: empty
        imm12BLayout :: BitLayout 32 12 =
          (chunk 31 :: Chunk 1) <: (chunk 7  :: Chunk 1) <:
          (chunk 25 :: Chunk 6) <: (chunk 8  :: Chunk 4) <:
          empty
        imm20ULayout :: BitLayout 32 20 = simpleLayout 12
        imm20JLayout :: BitLayout 32 20 =
          (chunk 31 :: Chunk 1)  <: (chunk 12 :: Chunk 8)  <:
          (chunk 20 :: Chunk 1)  <: (chunk 21 :: Chunk 10) <:
          empty
        illegalLayout :: BitLayout 32 32 = simpleLayout 0

opcode :: BitLayout 32 7
opcode = chunk 0 <: empty

-- | Get the format of an instruction word.
getFormat :: BitVector 32 -> Some FormatRepr
getFormat bv = case (bv ^. layoutLens opcode) of
  0b0110011 -> Some RRepr
  0b0111011 -> Some RRepr

  0b1100111 -> Some IRepr
  0b0000011 -> Some IRepr
  0b0010011 -> Some IRepr
  0b0001111 -> Some IRepr
  0b1110011 -> Some IRepr
  0b0011011 -> Some IRepr

  0b0100011 -> Some SRepr

  0b1100011 -> Some BRepr

  0b0110111 -> Some URepr
  0b0010111 -> Some URepr

  0b1101111 -> Some JRepr

  _ ->         Some XRepr

-- | Decode an instruction word. Since we won't know the format ahead of time, we
-- have to hide the format parameter of the return type with 'Some'.
decode :: InstructionSet arch exts
       -> BitVector 32
       -> Some (Instruction arch)
decode iset bv = case getFormat bv of
  Some repr -> case decodeOpcode iset repr bv of
    Right op     -> Some $ Inst op (decodeOperands repr bv)
    Left Illegal -> Some $ Inst Illegal (decodeOperands XRepr bv)

-- | From the format, get the operands
decodeOperands :: FormatRepr fmt -> BitVector 32 -> Operands fmt
decodeOperands repr bv = Operands repr (bv ^. layoutsLens (operandsLayouts repr))

-- | From the format, get the opbits
decodeOpBits :: FormatRepr fmt -> BitVector 32 -> OpBits fmt
decodeOpBits repr bv = OpBits repr (bv ^. layoutsLens (opBitsLayouts repr))

decodeOpcode :: InstructionSet arch exts
             -> FormatRepr fmt
             -> BitVector 32
             -> Either (Opcode arch X) (Opcode arch fmt)
decodeOpcode iset repr bv = opcodeFromOpBits iset (decodeOpBits repr bv)

-- | Encode an 'Instruction' as a 32-bit instruction word.
encode :: InstructionSet arch exts -> Instruction arch fmt -> BitVector 32
encode iset (Inst opc (Operands repr operands)) =
  0 & (opBitsLens .~ opBits) & (operandsLens .~ operands)
  where opBitsLens   = layoutsLens (opBitsLayouts repr)
        operandsLens = layoutsLens (operandsLayouts repr)
        (OpBits _ opBits) = opBitsFromOpcode iset opc
