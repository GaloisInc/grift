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
    fitsBitsSigned
  , fitsBitsUnsigned
  , placeBitsSigned
  , placeBitsUnsigned
    -- * Pretty printing
  , prettyHex
  ) where

import Data.Bits
import Data.Word
import Numeric (showHex)

----------------------------------------
-- Pretty Printing
-- | Print an integral value in hex with a leading "0x"
prettyHex :: (Show a, Integral a) => a -> String
prettyHex x = "0x" ++ showHex x ""

----------------------------------------
-- Bits

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
