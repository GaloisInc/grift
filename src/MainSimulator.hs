{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

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
import           Data.Array.IArray
import           Data.BitVector.Sized
import qualified Data.ByteString as BS
import           Data.IORef
import qualified Data.Map as Map
import           Data.Parameterized
import qualified Data.Parameterized.Map as MapF
import           System.Environment
import           System.Exit
import           Data.ElfEdit
import           GHC.TypeLits
import           System.FilePath.Posix
import           Text.PrettyPrint.HughesPJClass

import           RISCV.Coverage
import           RISCV.Extensions
import           RISCV.InstructionSet
import           RISCV.Types
import           RISCV.Semantics
import           RISCV.Semantics.Exceptions
import           RISCV.Simulation
import           RISCV.Simulation.LogMachine
import           RISCV.Simulation.MapMachine

type SimExts = (Exts '(PrivM, MYes, AYes, FDNo))

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ do
    putStrLn "Use: riscv-sim steps elfFile"
    exitFailure

  let [stepStr, fileName] = args
      stepsToRun = read stepStr :: Int
      logFile = replaceExtensions fileName "log"

  fileBS <- BS.readFile fileName
  case parseElf fileBS of
    Elf32Res _ e -> runElf stepsToRun logFile (RV32Elf e)
    Elf64Res _ e -> runElf stepsToRun logFile (RV64Elf e)

data RISCVElf (arch :: BaseArch) where
  RV32Elf :: Elf 32 -> RISCVElf RV32
  RV64Elf :: Elf 64 -> RISCVElf RV64

rElf :: RISCVElf arch -> Elf (ArchWidth arch)
rElf (RV32Elf e) = e
rElf (RV64Elf e) = e

runElf :: forall arch . (ElfWidthConstraints (ArchWidth arch), KnownArch arch)
       => Int -> FilePath -> RISCVElf arch -> IO ()
runElf stepsToRun logFile re = do
  let e = rElf re
      byteStrings = elfBytes e
  m :: LogMachine (RVConfig '(arch, SimExts)) <-
    mkLogMachine 0x1000000 (fromIntegral $ elfEntry e) 0x10000 byteStrings
  runLogMachine stepsToRun m

  pc         <- readIORef (ioPC m)
  registers  <- freezeRegisters m
  csrs       <- readIORef (ioCSRs m)
  testMap    <- readIORef (ioTestMap m)

  putStrLn $ "MInstRet = " ++
    show (bvIntegerU (Map.findWithDefault 0 (encodeCSR MInstRet) csrs))
  putStrLn $ "MEPC = " ++ show (Map.findWithDefault 0 (encodeCSR MEPC) csrs)
  putStrLn $ "MTVal = " ++ show (Map.findWithDefault 0 (encodeCSR MTVal) csrs)
  putStrLn $ "MCause = " ++ show (Map.findWithDefault 0 (encodeCSR MCause) csrs)
  putStrLn $ "Final PC: " ++ show pc
  putStrLn "Final register state:"
  forM_ (assocs registers) $ \(r, v) ->
    putStrLn $ "  R[" ++ show (bvIntegerU r) ++ "] = " ++ show v

  putStrLn "\n--------Coverage report--------\n"
  forM_ (Map.toList testMap) $ \(Some opcode, vals) -> do
    case MapF.lookup opcode knownCoverageMap of
      Just (InstExprList exprs) -> do
        let ones = length (filter (==1) vals)
        putStrLn $ show opcode ++ " (" ++ show ones ++ "/" ++ show (length vals) ++ ") :"
        forM_ (zip exprs vals) $ \(expr, val) ->
          putStrLn $ "  " ++ prettyShow expr ++ " ---> " ++ show val
      _ -> return ()

-- | From an Elf file, get a list of the byte strings to load into memory along with
-- their starting addresses.
elfBytes :: (KnownNat w, ElfWidthConstraints w) => Elf w -> [(BitVector w, BS.ByteString)]
elfBytes e = pairWithAddr <$> filter memoryMapped sections
  where sections = e ^.. elfSections
        memoryMapped section = elfSectionAddr section > 0
        pairWithAddr section = (fromIntegral $ elfSectionAddr section, elfSectionData section)
