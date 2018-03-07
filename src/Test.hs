{-# LANGUAGE DataKinds #-}

module Test where

import qualified Data.ByteString as BS
import qualified Data.ElfEdit as E

import RISCV

elf :: IO (E.Elf 32)
elf = do
  bs <- BS.readFile "/Users/benselfridge/c-programs/test"
  case E.parseElf bs of
    E.Elf32Res err e -> do
      return e
    _ -> error "couldn't parse"
