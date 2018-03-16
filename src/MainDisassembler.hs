{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : MainDisassembler
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Tool for disassembling RISC-V programs in the ELF executable format.
-}

module Main where

import           Control.Lens ( (^.)
                              , (^..)
                              )
import           Control.Monad
import           Data.Bits
import           Data.BitVector.Sized
import           Data.Bool
import qualified Data.ByteString as BS
import           Data.Parameterized
import           System.Environment
import           System.Exit
import           Data.ElfEdit

import           RISCV

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
      Elf32Res _err e -> disElf e
      Elf64Res _err e -> disElf e
      ElfHeaderError _byteOffset _msg -> do
        putStrLn "error"

-- | Decode a single instruction from a bytestring and returns the instruction along
-- with the remaining bytes and the number of bytes consumed. Since we currently only
-- support RV32I, this function only decodes 4-byte words.
disInstruction :: BS.ByteString -> Maybe (Some Instruction, BS.ByteString, Int)
disInstruction bs =
  bool Nothing (Just (inst, rstBytes, numBytes)) (BS.length bs >= 4)
  where (instBytes, rstBytes) = BS.splitAt 4 bs
        (b0:b1:b2:b3:[]) = fromIntegral <$> BS.unpack instBytes :: [Integer]
        instWordI = b3 `shiftL` 24 .|.
                    b2 `shiftL` 16 .|.
                    b1 `shiftL` 8  .|.
                    b0
        inst = decode $ bitVector instWordI
        numBytes = 4

-- | Decode a bytestring as a RISC-V program
disInstructions :: BS.ByteString -> [Some Instruction]
disInstructions bs = case disInstruction bs of
  -- not enough bytes to disassemble an instruction. We should probably detect if
  -- length bs < 4 and give some kind of warning, but I'm not worried about that for now.
  Nothing -> []
  Just (inst, bsRst, _) -> inst : disInstructions bsRst

disElf :: ElfWidthConstraints w => Elf w -> IO ()
disElf e = do
  let codeSections = filter isCodeSection $  e ^.. elfSections
      isCodeSection sec =
        (elfSectionFlags sec .&. shf_execinstr) /= zeroBits
  forM_ codeSections $ \sec -> do
    let bs = elfSectionData sec
    mapM_ print $ disInstructions bs
    return ()
