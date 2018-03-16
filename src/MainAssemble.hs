
{-|
Module      : MainAssemble
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Tool for assembling RISC-V programs in the ELF executable format.
-}

module Main where

import           Control.Monad
import qualified Data.ByteString as B
import           System.Environment
import           System.Exit
-- import qualified Data.ElfEdit ( Elf )
-- import           Data.ElfEdit as E

main :: IO ()
main = do
  args <- getArgs
  when (length args==0) $ do
    putStrLn "Please supply a RISC-V ELF binary."
    exitFailure
  putStrLn "Good job!"

