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
    lowMask
  , truncBits
    -- * Pretty printing
  , prettyHex
  ) where

import Data.Bits
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

-- | Mask for a specified number of lower bits.
lowMask :: (Integral a, Bits b) => a -> b
lowMask numBits = complement (complement zeroBits `shiftL` fromIntegral numBits)

-- | Truncate to a specified number of lower bits.
truncBits :: (Integral a, Bits b) => a -> b -> b
truncBits width b = b .&. lowMask width
