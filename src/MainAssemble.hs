
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

import           Control.Lens ( (^.) )
import           Control.Monad
import qualified Data.ByteString as BS
import           System.Environment
import           System.Exit
import           Data.ElfEdit as E

main :: IO ()
main = do
  args <- getArgs
  when (length args==0) $ do
    putStrLn "Please supply a RISC-V ELF binary."
    exitFailure

  -- load in each file as a bytestring
  forM_ args $ \fileName -> do
    fileBS <- BS.readFile fileName
    case E.parseElf fileBS of
      E.Elf32Res _err e -> do
        let efd = e^.elfFileData
        return ()
      E.Elf64Res _err e -> do
        putStrLn "64-bit"
      E.ElfHeaderError _byteOffset _msg -> do
        putStrLn "error"
