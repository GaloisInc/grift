{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

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

import           Control.Lens ( (^..) )
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.IArray
import           Data.Bits
import           Data.BitVector.Sized
import           Data.Bool
import qualified Data.ByteString as BS
import           Data.Monoid
import           Data.Parameterized
import           System.Environment
import           System.Exit
import           Data.ElfEdit

import           RISCV.Types
import           RISCV.Instruction
import           RISCV.InstructionSet
import           RISCV.Decode
import           RISCV.Extensions
import           RISCV.Simulation
import           RISCV.Simulation.STMachine

type SimArch = RV64I
type SimExts = (Exts '(MYes, FDNo))

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ do
    putStrLn "Use: riscv-sim steps elfFile"
    exitFailure

  let [stepStr, fileName] = args
  stepsToRun <- return $ (read stepStr :: Int)

  fileBS <- BS.readFile fileName
  case parseElf fileBS of
    Elf64Res _err e -> do
      let byteStrings = elfBytes e
      let (pc, registers, _, steps, err) = runST $ do
            m <- mkSTMachine
              (knownRepr :: BaseArchRepr SimArch)
              (knownRepr :: ExtensionsRepr SimExts)
              0x80000
              (fromIntegral $ elfEntry e)
              byteStrings
            execSTMachine (runRV stepsToRun) m
      case err of
        Nothing -> return ()
        Just err' -> putStrLn $ "Encountered exception: " ++ show err'
      putStrLn $ "Executed " ++ show steps ++ " instructions."
      putStrLn $ "Final PC: " ++ show pc
      putStrLn $ "Final register state:"
      forM_ (assocs registers) $ \(r, v) -> do
        putStrLn $ "  R[" ++ show r ++ "] = " ++ show v


-- TODO: modify this function to read in the Elf file, traverse all the sections, and
-- map them into the simulation memory as an ST s () action.
parseElfFile :: String -> IO [(Some (Instruction SimArch), BitVector 32)]
parseElfFile fileName = do
  fileBS <- BS.readFile fileName
  case parseElf fileBS of
    Elf32Res _err e -> disElf e
    Elf64Res _err e -> disElf e
    ElfHeaderError _byteOffset _msg -> return []

-- TODO: Modify this function to traverse all the sections and map them into the
-- simulation memory as an ST s () action.
disElf :: ElfWidthConstraints w => Elf w -> IO [(Some (Instruction SimArch), BitVector 32)]
disElf e = do
  let [textSection] = findSectionByName ".text" e
      bs  = elfSectionData textSection
      dis = disInstructions bs
  return dis

-- | From an Elf file, get a list of the byte strings to load into memory along with
-- their starting addresses.
elfBytes :: ElfWidthConstraints w => Elf w -> [(ElfWordType w, BS.ByteString)]
elfBytes e = pairWithAddr <$> filter memoryMapped sections
  where sections = e ^.. elfSections
        memoryMapped section = elfSectionAddr section > 0
        pairWithAddr section = (elfSectionAddr section, elfSectionData section)

-- | Decode a single instruction from a bytestring and returns the instruction along
-- with the remaining bytes and the number of bytes consumed. Since we currently only
-- support RV32I, this function only decodes 4-byte words.
disInstruction :: BS.ByteString -> Maybe (Some (Instruction SimArch), BitVector 32, BS.ByteString, Int)
disInstruction bs =
  bool Nothing (Just (inst, instBV, rb, numBytes)) (BS.length bs >= 4)
  where (ib, rb) = BS.splitAt 4 bs
        (b0:b1:b2:b3:[]) = fromIntegral <$> BS.unpack ib :: [Integer]
        instWordI = b3 `shiftL` 24 .|.
                    b2 `shiftL` 16 .|.
                    b1 `shiftL` 8  .|.
                    b0
        instBV = bitVector instWordI
        inst = decode (knownISet :: InstructionSet SimArch SimExts) instBV
        numBytes = 4

-- | Decode a bytestring as a RISC-V program
disInstructions :: BS.ByteString -> [(Some (Instruction SimArch), BitVector 32)]
disInstructions bs = case disInstruction bs of
  -- not enough bytes to disassemble an instruction. We should probably detect if
  -- length bs < 4 and give some kind of warning, but I'm not worried about that for now.
  Nothing -> []
  Just (inst, instBV, bsRst, _) -> (inst, instBV) : disInstructions bsRst
