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
import           System.Environment
import           System.Exit
import           Data.ElfEdit
import           GHC.TypeLits
import           System.FilePath.Posix

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
       => Int
       -> FilePath
       -> RISCVElf arch
       -> IO ()
runElf stepsToRun logFile re = do
  let e = rElf re
      byteStrings = elfBytes e
  m :: LogMachine arch SimExts <-
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

  writeFile logFile "Opcode, Coverage\n"

  let iset = knownISet :: InstructionSet arch SimExts
  forM_ (Map.assocs testMap) $ \(Some opcode, variants) ->
    let numTests = length (getTests (getInstFormula $ semanticsFromOpcode iset opcode))
    in
      appendFile logFile $ show opcode ++ ", " ++ show (length variants) ++
      "/" ++ show (2^numTests) ++ "\n"

runElfMap :: forall arch . (ElfWidthConstraints (ArchWidth arch), KnownArch arch)
          => Int
          -> FilePath
          -> RISCVElf arch
          -> IO ()
runElfMap stepsToRun logFile re = do
  let e = rElf re
      byteStrings = elfBytes e
  m :: MapMachine arch SimExts <-
    return $ mkMapMachine 0x1000000 (fromIntegral $ elfEntry e) byteStrings
  let (_, m') = runMapMachine stepsToRun m

  let err        = exception m'
      stepsRan   = steps m'
      pc'        = pc m'
      registers' = registers m'
      testMap'   = testMap m'

  case err of
    Nothing -> return ()
    Just err' -> putStrLn $ "Encountered exception: " ++ show err'
  putStrLn $ "Executed " ++ show stepsRan ++ " instructions."
  putStrLn $ "Final PC: " ++ show pc'
  putStrLn "Final register state:"
  forM_ (Map.assocs registers') $ \(r, v) ->
    putStrLn $ "  R[" ++ show r ++ "] = " ++ show v

  writeFile logFile "Opcode, Coverage\n"

  let iset = knownISet :: InstructionSet arch SimExts
  forM_ (Map.assocs testMap') $ \(Some opcode, variants) ->
    let numTests = length (getTests (getInstFormula $ semanticsFromOpcode iset opcode))
    in
      appendFile logFile $ show opcode ++ ", " ++ show (length variants) ++
      "/" ++ show (2^numTests) ++ "\n"

-- | From an Elf file, get a list of the byte strings to load into memory along with
-- their starting addresses.
elfBytes :: (KnownNat w, ElfWidthConstraints w) => Elf w -> [(BitVector w, BS.ByteString)]
elfBytes e = pairWithAddr <$> filter memoryMapped sections
  where sections = e ^.. elfSections
        memoryMapped section = elfSectionAddr section > 0
        pairWithAddr section = (fromIntegral $ elfSectionAddr section, elfSectionData section)
