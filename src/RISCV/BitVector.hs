{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : RISCV.BitVector
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module defines a width-parameterized 'BitVector' type and various associated
operations that assume a 2's complement representation.

Along with a 'Bits' instance for every @BitVector w@ type where @w@ is known at
compile time, we also provide a few additional operations for conversion between
bitvectors of different lengths.
-}

module RISCV.BitVector
  ( -- * BitVector type
    BitVector
  , bv
    -- * Bitwise ops (width-preserving)
    -- | These are alternative versions of some of the 'Bits' functions where we do
    -- not need to know the width at compile time. They are all width-preserving.
  , bvAnd
  , bvOr
  , bvXor
  , bvComplement
  , bvShift
  , bvRotate
  , bvWidth
  , bvTestBit
  , bvPopCount
    -- * Bit arithmetic ops (width-preserving)
  , bvAdd, bvMul
  , bvAbs, bvNegate
  , bvSignum
    -- * Bitwise ops (variable width)
    -- | These are functions that involve bit vectors of different lengths.
  , bvConcat
  , bvExtract
  , bvTrunc
  , bvZext
  , bvSext
    -- * Conversions to Integer
  , bvIntegerU
  , bvIntegerS
  ) where

import Data.Bits
import Data.Parameterized.Classes
import Data.Parameterized.NatRepr
import GHC.TypeLits
import Text.Printf

----------------------------------------
-- BitVector data type definitions

-- | BitVector datatype, parameterized by width.
data BitVector (w :: Nat) :: * where
  BV :: NatRepr w -> Integer -> BitVector w

-- | Construct a bit vector in a context where the width is inferrable from the type
-- context. If the width is not large enough to hold the integer in 2's complement
-- representation, we silently truncate it to fit.
--
-- >>> bv 0xA :: BitVector 4
-- 0xa<4>
-- >>> bv 0xA :: BitVector 3
-- 0x2<3>
-- >>> bv (-1) :: BitVector 8
-- 0xff<8>
-- >>> bv (-1) :: BitVector 32
-- 0xffffffff<32>

bv :: KnownNat w => Integer -> BitVector w
bv x = BV wRepr (truncBits width (fromIntegral x))
  where wRepr = knownNat
        width = natValue wRepr

----------------------------------------
-- BitVector -> Integer functions

-- | Unsigned interpretation of a bit vector as a (positive) Integer.
bvIntegerU :: BitVector w -> Integer
bvIntegerU (BV _ x) = x

-- | Signed interpretation of a bit vector as an Integer.
bvIntegerS :: BitVector w -> Integer
bvIntegerS bvec = case bvTestBit bvec (width - 1) of
  True  -> bvIntegerU bvec - (1 `shiftL` width)
  False -> bvIntegerU bvec
  where width = bvWidth bvec

----------------------------------------
-- BitVector w operations (fixed width)

-- | Bitwise and
bvAnd :: BitVector w -> BitVector w -> BitVector w
bvAnd (BV wRepr x) (BV _ y) = BV wRepr (x .&. y)

-- | Bitwise or
bvOr :: BitVector w -> BitVector w -> BitVector w
bvOr (BV wRepr x) (BV _ y) = BV wRepr (x .|. y)

-- | Bitwise xor
bvXor :: BitVector w -> BitVector w -> BitVector w
bvXor (BV wRepr x) (BV _ y) = BV wRepr (x `xor` y)

-- | Bitwise complement (flip every bit)
bvComplement :: BitVector w -> BitVector w
bvComplement (BV wRepr x) = BV wRepr (truncBits width (complement x))
  where width = natValue wRepr

-- | Bitwise shift
bvShift :: BitVector w -> Int -> BitVector w
bvShift bvec@(BV wRepr _) shf = BV wRepr (truncBits width (x `shift` shf))
  where width = natValue wRepr
        -- use signed representation so right shifts are arithmetic
        x     = bvIntegerS bvec

-- | Bitwise rotate
bvRotate :: BitVector w -> Int -> BitVector w
bvRotate bvec rot' = leftChunk `bvOr` rightChunk
  where rot = rot' `mod` (bvWidth bvec)
        leftChunk = bvShift bvec rot
        rightChunk = bvShift bvec (rot - bvWidth bvec)

-- | Get the width of a 'BitVector'
bvWidth :: BitVector w -> Int
bvWidth (BV wRepr _) = fromIntegral (natValue wRepr)

-- | Test if a particular bit is set
bvTestBit :: BitVector w -> Int -> Bool
bvTestBit (BV _ x) b = testBit x b

-- | Get the number of 1 bits in a 'BitVector'
bvPopCount :: BitVector w -> Int
bvPopCount (BV _ x) = popCount x

----------------------------------------
-- BitVector w arithmetic operations (fixed width)

bvAdd :: BitVector w -> BitVector w -> BitVector w
bvAdd (BV wRepr x) (BV _ y) = BV wRepr (truncBits width (x + y))
  where width = natValue wRepr

-- TODO: Is this correct for signed as well? (think it is)
bvMul :: BitVector w -> BitVector w -> BitVector w
bvMul (BV wRepr x) (BV _ y) = BV wRepr (truncBits width (x * y))
  where width = natValue wRepr

bvAbs :: BitVector w -> BitVector w
bvAbs bvec@(BV wRepr _) = BV wRepr abs_x
  where width = natValue wRepr
        x     = bvIntegerS bvec
        abs_x = truncBits width (abs x) -- this is necessary

bvNegate :: BitVector w -> BitVector w
bvNegate (BV wRepr x) = BV wRepr (truncBits width (-x))
  where width = fromIntegral (natValue wRepr) :: Integer

bvSignum :: BitVector w -> BitVector w
bvSignum bvec@(BV wRepr _) = (bvShift bvec (1 - width)) `bvAnd` (BV wRepr 0x1)
  where width = fromIntegral (natValue wRepr)

----------------------------------------
-- Width-changing operations

-- TODO work out associativity with bvConcat.
-- | Concatenate two bit vectors.
--
-- >>> (bv 0xAA :: BitVector 8) `bvConcat` (bv 0xBCDEF0 :: BitVector 24)
-- 0xaabcdef0<32>
bvConcat :: BitVector v -> BitVector w -> BitVector (v+w)
bvConcat (BV hiWRepr hi) (BV loWRepr lo) =
  BV (hiWRepr `addNat` loWRepr) ((hi `shiftL` loWidth) .|. lo)
  where loWidth = fromIntegral (natValue loWRepr)

-- | Slice out a smaller bit vector from a larger one. The lowest significant bit is
-- given explicitly as an argument of type 'Int', and the length of the slice is
-- inferred from a type-level context.
--
-- >>> bvExtract 12 (bv 0xAABCDEF0 :: BitVector 32) :: BitVector 8
-- 0xcd<8>
--
-- Note that 'bvExtract' does not do any bounds checking whatsoever; if you try and
-- extract bits that aren't present in the input, you will get 0's.
bvExtract :: forall w w' . (KnownNat w')
          => Int
          -> BitVector w
          -> BitVector w'
bvExtract pos bvec = bv xShf
  where (BV _ xShf) = bvShift bvec (- pos)

-- | Truncate a bit vector to one of smaller length.
bvTrunc :: forall w w' . (KnownNat w', w' <= w)
        => BitVector w
        -> BitVector w'
bvTrunc (BV _ x) = bv x -- bv function handles the truncation.

-- | Zero-extend a vector to one of greater length.
bvZext :: forall w w' . (KnownNat w', w <= w')
       => BitVector w
       -> BitVector w'
bvZext (BV _ x) = bv x

-- | Sign-extend a vector to one of greater length.
bvSext :: forall w w' . (KnownNat w', w <= w')
       => BitVector w
       -> BitVector w'
bvSext bvec = bv (bvIntegerS bvec)

----------------------------------------
-- Class instances

instance Show (BitVector w) where
  show (BV wRepr val) = prettyHex width val
    where width = natValue wRepr

instance ShowF BitVector

instance Eq (BitVector w) where
  (BV _ x) == (BV _ y) = x == y

instance EqF BitVector where
  (BV wRepr x) `eqF` (BV wRepr' y) =
    natValue wRepr == natValue wRepr' &&
    x == y

instance KnownNat w => Bits (BitVector w) where
  (.&.)        = bvAnd
  (.|.)        = bvOr
  xor          = bvXor
  complement   = bvComplement
  shift        = bvShift
  rotate       = bvRotate
  bitSize      = bvWidth
  bitSizeMaybe = Just . bvWidth
  isSigned     = const False
  testBit      = bvTestBit
  bit          = bv . bit
  popCount     = bvPopCount

instance KnownNat w => Num (BitVector w) where
  (+) = bvAdd
  (*) = bvMul
  abs = bvAbs
  signum = bvSignum
  fromInteger = bv
  negate = bvNegate

----------------------------------------
-- Pretty Printing

-- | Print an integral value in hex with a leading "0x"
prettyHex :: (Integral a, PrintfArg a, Show a) => a -> Integer -> String
prettyHex width val = printf format val width
  where numDigits = (width+3) `div` 4
        format = "0x%." ++ show numDigits ++ "x<%d>"

----------------------------------------
-- Bits

-- | Mask for a specified number of lower bits.
lowMask :: (Integral a, Bits b) => a -> b
lowMask numBits = complement (complement zeroBits `shiftL` fromIntegral numBits)

-- | Truncate to a specified number of lower bits.
truncBits :: (Integral a, Bits b) => a -> b -> b
truncBits width b = b .&. lowMask width
