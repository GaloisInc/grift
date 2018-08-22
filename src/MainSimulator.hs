{-
This file is part of GRIFT (Galois RISC-V ISA Formal Tools).

GRIFT is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GRIFT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero Public License for more details.

You should have received a copy of the GNU Affero Public License
along with GRIFT.  If not, see <https://www.gnu.org/licenses/>.
-}

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
License     : AGPLv3
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
import           RISCV.InstructionSet
import           RISCV.InstructionSet.Known
import           RISCV.InstructionSet.Utils
import           RISCV.Types
import           RISCV.Semantics
import           RISCV.Simulation
import           RISCV.Simulation.LogMachine

-- | Get an 'RV' type representative from an input 'String'.
rvReprFromString :: String -> Maybe (Some RVRepr)
rvReprFromString s = case s of
  "RV32I"     -> Just $ Some (knownRepr :: RVRepr RV32I)
  "RV32IM"    -> Just $ Some (knownRepr :: RVRepr RV32IM)
  "RV32IMA"   -> Just $ Some (knownRepr :: RVRepr RV32IMA)
  "RV32IMAF"  -> Just $ Some (knownRepr :: RVRepr RV32IMAF)
  "RV32IMAFD" -> Just $ Some (knownRepr :: RVRepr RV32IMAFD)
  "RV64I"     -> Just $ Some (knownRepr :: RVRepr RV64I)
  "RV64IM"    -> Just $ Some (knownRepr :: RVRepr RV64IM)
  "RV64IMA"   -> Just $ Some (knownRepr :: RVRepr RV64IMA)
  "RV64IMAF"  -> Just $ Some (knownRepr :: RVRepr RV64IMAF)
  "RV64IMAFD" -> Just $ Some (knownRepr :: RVRepr RV64IMAFD)
  _ -> Nothing

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ do
    putStrLn "Use: riscv-sim <config> <steps> elfFile"
    putStrLn "  <config> = RV{32|64}I, RV{32|64}IM, ..., RV{32|64}IMAFD"
    putStrLn "  <steps> = positive integer, # of steps to run"
    exitFailure

  let [rvStr, stepStr, fileName] = args
      stepsToRun = read stepStr :: Int
      logFile = replaceExtensions fileName "log"
  Some rvRepr <- case rvReprFromString rvStr of
    Nothing -> do
      putStrLn $ "Unknown configuration " ++ rvStr ++", defaulting to RV32I"
      return (Some (knownRepr :: RVRepr RV32I))
    Just repr -> return repr

  fileBS <- BS.readFile fileName
  case parseElf fileBS of
    Elf32Res _ e -> runElf rvRepr stepsToRun logFile e
    Elf64Res _ e -> runElf rvRepr stepsToRun logFile e

-- | Run a RISC-V ELF executable in simulation for a pre-specified number of steps.
runElf :: forall rv w . ElfWidthConstraints w
       => RVRepr rv -> Int -> FilePath -> Elf w -> IO ()
runElf rvRepr stepsToRun logFile e = do
  let byteStrings = elfBytes e
  m  <- mkLogMachine rvRepr 0x1000000 (fromIntegral $ elfEntry e) 0x10000 byteStrings
  runLogMachine stepsToRun m

  pc         <- readIORef (ioPC m)
  registers  <- freezeRegisters m
  fregisters <- freezeFRegisters m
  csrs       <- readIORef (ioCSRs m)
  testMap    <- readIORef (ioTestMap m)

  putStrLn $ "MInstRet = " ++
    show (bvIntegerU (Map.findWithDefault 0 (encodeCSR MInstRet) csrs))
  putStrLn $ "MEPC = " ++ show (Map.findWithDefault 0 (encodeCSR MEPC) csrs)
  putStrLn $ "MTVal = " ++ show (Map.findWithDefault 0 (encodeCSR MTVal) csrs)
  putStrLn $ "MCause = " ++ show (Map.findWithDefault 0 (encodeCSR MCause) csrs)
  putStrLn $ "FCSR = " ++ show (Map.findWithDefault 0 (encodeCSR FCSR) csrs)
  putStrLn $ "Final PC: " ++ show pc
  putStrLn "Final register state:"
  forM_ (assocs registers) $ \(r, v) ->
    putStrLn $ "  x[" ++ show (bvIntegerU r) ++ "] = " ++ show v
  putStrLn "Final FP register state:"
  forM_ (assocs fregisters) $ \(r, v) ->
    putStrLn $ "  f[" ++ show (bvIntegerU r) ++ "] = " ++ show v

  -- putStrLn "\n--------Coverage report--------\n"
  -- forM_ (Map.toList testMap) $ \(Some opcode, vals) -> do
  --   case MapF.lookup opcode knownCoverageMap of
  --     Just (InstExprList exprs) -> do
  --       let ones = length (filter (==1) vals)
  --       putStrLn $ show opcode ++ " (" ++ show ones ++ "/" ++ show (length vals) ++ ") :"
  --       forM_ (zip exprs vals) $ \(expr, val) ->
  --         putStrLn $ "  " ++ prettyShow expr ++ " ---> " ++ show val
  --     _ -> return ()

-- | From an Elf file, get a list of the byte strings to load into memory along with
-- their starting addresses.
elfBytes :: (KnownNat w, ElfWidthConstraints w) => Elf w -> [(BitVector w, BS.ByteString)]
elfBytes e = pairWithAddr <$> filter memoryMapped sections
  where sections = e ^.. elfSections
        memoryMapped section = elfSectionAddr section > 0
        pairWithAddr section = (fromIntegral $ elfSectionAddr section, elfSectionData section)
