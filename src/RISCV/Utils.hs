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

module RISCV.Utils where

import Numeric (showHex)

-- | Print an integral value in hex with a leading "0x"
prettyHex :: (Show a, Integral a) => a -> String
prettyHex x = "0x" ++ showHex x ""
