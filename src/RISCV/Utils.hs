{-|
Module      : RISCV.Utils
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

A few utility functions.
-}

module RISCV.Utils
  ( -- * Bits
    extract
  , lowMask
  , truncBits
  , fitsBitsSigned
  , fitsBitsUnsigned
  , placeBitsSigned
  , placeBitsUnsigned
    -- * Pretty printing
  , prettyHex
  ) where

import Data.Bits
import Data.Word
import Text.Printf

----------------------------------------
-- Pretty Printing
-- | Print an integral value in hex with a leading "0x"
prettyHex :: (Integral a, PrintfArg a, Show a) => a -> Integer -> String
prettyHex width val = printf format val width
  where numDigits = (width+3) `div` 4
        format = "0x%." ++ show numDigits ++ "x<%d>"

----------------------------------------
-- Bits

-- | Extract a slice from a 32-bit word.
extract :: Integral a => Int -> Int -> Word32 -> a
extract low hgh x = fromIntegral $ x `shiftR` low .&. complement (0xffffffff `shiftL` (hgh-low+1))

lowMask :: (Integral a, Bits b) => a -> b
lowMask numBits = complement (complement zeroBits `shiftL` fromIntegral numBits)

truncBits :: (Integral a, Bits b) => a -> b -> b
truncBits width b = b .&. lowMask width

-- | Determine if a signed integer fits in the specified number of bits.
fitsBitsSigned :: Integer -> Int -> Bool
fitsBitsSigned word width = fitsBitsUnsigned adjWord width
  where adjWord = word + (1 `shiftL` (width - 1))

-- | Determine if an unsigned integer fits in the specified number of bits. Return
-- False if the input word is negative.
fitsBitsUnsigned :: Integer -> Int -> Bool
fitsBitsUnsigned word width =
  if 0 <= word && word < (1 `shiftL` width)
  then True
  else False

placeBitsSigned :: Int -> Int -> Integer -> Maybe Word32
placeBitsSigned lo hi w = do
  let bits = hi - lo + 1
  if w `fitsBitsSigned` bits
    then return $ (fromIntegral w .&. (bit bits - 1)) `shiftL` lo
    else Nothing

placeBitsUnsigned :: Int -> Int -> Integer -> Maybe Word32
placeBitsUnsigned lo hi w = do
  let bits = hi - lo + 1
  if w `fitsBitsUnsigned` bits
    then return $ (fromIntegral w .&. (bit bits - 1)) `shiftL` lo
    else Nothing
