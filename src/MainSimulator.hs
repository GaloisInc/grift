{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : MainSimulator
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Tool for simulating RISC-V programs in the ELF executable format.
-}

module Main where

import           Control.Lens ( (^.)
                              , (^..)
                              )
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString as BS
import           System.Environment
import           System.Exit
import           Data.ElfEdit

main :: IO ()
main = do
  args <- getArgs
  when (length args==0) $ do
    putStrLn "Please supply a RISC-V ELF binary."
    exitFailure

  -- load in each file as a bytestring
  forM_ args $ \fileName -> do
    fileBS <- BS.readFile fileName
    case parseElf fileBS of
      Elf32Res _err e -> runElf e
      Elf64Res _err e -> runElf e
      ElfHeaderError _byteOffset _msg -> do
        putStrLn "error"

runElf :: ElfWidthConstraints w => Elf w -> IO ()
runElf e = do
  let codeSections = filter isCodeSection $  e ^.. elfSections
      isCodeSection sec =
        (elfSectionFlags sec .&. shf_execinstr) /= zeroBits
  forM_ codeSections $ \sec -> do
    return ()
