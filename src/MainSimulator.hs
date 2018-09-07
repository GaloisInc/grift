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
{-# LANGUAGE Rank2Types          #-}
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
import           System.Console.GetOpt
import           System.FilePath.Posix
import           System.IO
import           System.IO.Error
import           Text.PrettyPrint.HughesPJClass
import           Text.Read

import           RISCV.Coverage
import           RISCV.InstructionSet
import           RISCV.InstructionSet.Known
import           RISCV.InstructionSet.Utils
import           RISCV.Types
import           RISCV.Semantics
import           RISCV.Simulation
import           RISCV.Simulation.LogMachine

import Data.Coerce

rvReprFromString :: String -> Maybe (Some RVRepr)
rvReprFromString s = case s of
  "RV32I" -> Just $ Some (knownRepr :: RVRepr RV32I)
  "RV32IM" -> Just $ Some (knownRepr :: RVRepr RV32IM)
  "RV32IMA" -> Just $ Some (knownRepr :: RVRepr RV32IMA)
  "RV32IMAF" -> Just $ Some (knownRepr :: RVRepr RV32IMAF)
  "RV32IMAFD" -> Just $ Some (knownRepr :: RVRepr RV32IMAFD)
  "RV64I" -> Just $ Some (knownRepr :: RVRepr RV64I)
  "RV64IM" -> Just $ Some (knownRepr :: RVRepr RV64IM)
  "RV64IMA" -> Just $ Some (knownRepr :: RVRepr RV64IMA)
  "RV64IMAF" -> Just $ Some (knownRepr :: RVRepr RV64IMAF)
  "RV64IMAFD" -> Just $ Some (knownRepr :: RVRepr RV64IMAFD)
  _ -> Nothing

data SimOpts = SimOpts
  { simSteps :: Int
  , simRV :: Some RVRepr
  , simCov :: Maybe String
  }

defaultSimOpts = SimOpts
  { simSteps = 10000
  , simRV = Some (knownRepr :: RVRepr RV32I)
  , simCov = Nothing
  }

options :: [OptDescr (SimOpts -> IO SimOpts)]
options =
  [ Option ['s'] ["steps"]
    (ReqArg (\stepStr opts -> case readMaybe stepStr of
                Nothing    -> exitWithUsage $ "Illegal value for --steps: " ++ stepStr ++ "\n"
                Just steps -> return $ opts { simSteps = steps } )
     "NUM")
    ("max # of simulation steps (default = " ++ show (simSteps defaultSimOpts) ++ ")")
  , Option ['a'] ["arch"]
    (ReqArg (\rvStr opts -> case rvReprFromString rvStr of
                Nothing    -> exitWithUsage $ "Unrecognized --arch value: " ++ rvStr ++ "\n"
                Just someRV -> return $ opts { simRV = someRV } )
     "ARCH")
    ("RISC-V arch configuration (default = RV32I)")
  , Option ['c'] ["coverage"]
    (ReqArg (\covStr opts -> return $ opts { simCov = Just covStr })
     "FILE")
    ("Print coverage analysis to file")
  , Option ['h'] ["help"]
    (NoArg (\_ -> exitWithUsage ""))
    ("display help message")
  ]

header :: String
header = "Usage: grift-sim [-s NUM] [-a ARCH] [-c FILE] elffile"

exitWithUsage :: String -> IO a
exitWithUsage "" = do
  putStrLn $ usageInfo header options
  exitWith (ExitFailure 1)
exitWithUsage msg = do
  putStrLn msg
  putStrLn $ usageInfo header options
  exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  let (actions, nonOptions, errors) = getOpt Permute options args

  -- First check that there were no option errors
  when (not (null errors)) $ exitWithUsage (concat errors)

  -- Next build up the options, potentially exiting early
  opts <- foldl (>>=) (return defaultSimOpts) actions

  -- Next check that there is exactly one argument, a path to an elf file
  fileName <- case nonOptions of
    [s] -> return s
    _ -> exitWithUsage $ "error: provide a path to an elf file to simulate\n"

  -- Next check that the file actually exists
  fileBS <- catchIOError (BS.readFile fileName) $ \_ ->
    exitWithUsage $ "error: file \"" ++ fileName ++ "\" does not exist\n"

  case (simRV opts, parseElf fileBS) of
    (Some rvRepr@(RVRepr RV32Repr _), Elf32Res _ e) ->
      runElf rvRepr (simSteps opts) (simCov opts) e
    (Some rvRepr@(RVRepr RV64Repr _), Elf64Res _ e) ->
      runElf rvRepr (simSteps opts) (simCov opts) e
    _ -> exitWithUsage $ "Error: bad object file\n"

runElf :: ElfWidthConstraints (RVWidth rv) => RVRepr rv -> Int -> Maybe String -> Elf (RVWidth rv) -> IO ()
runElf rvRepr stepsToRun mLogFile e = withRVWidth rvRepr $ do
  let byteStrings = elfBytes e
  m <- mkLogMachine
    rvRepr
    0x1000000
    (fromIntegral $ elfEntry e)
    0x10000
    byteStrings

  case mLogFile of
    Nothing -> runLogMachine stepsToRun m
    Just _  -> runLogMachineLog stepsToRun m

  pc         <- readIORef (lmPC m)
  registers  <- freezeRegisters m
  fregisters <- freezeFRegisters m
  csrs       <- readIORef (lmCSRs m)
  testMap    <- readIORef (lmTestMap m)

  putStrLn $ "MInstRet = " ++
    show (bvIntegerU (Map.findWithDefault 0 (encodeCSR MInstRet) csrs))
  putStrLn $ "MEPC = " ++ show (Map.findWithDefault 0 (encodeCSR MEPC) csrs)
  putStrLn $ "MTVal = " ++ show (Map.findWithDefault  0 (encodeCSR MTVal) csrs)
  putStrLn $ "MCause = " ++ show (Map.findWithDefault 0 (encodeCSR MCause) csrs)
  putStrLn $ "FCSR = " ++ show (Map.findWithDefault 0 (encodeCSR FCSR) csrs)
  putStrLn $ "Final PC: " ++ show pc
  putStrLn "Final register state:"
  forM_ (assocs registers) $ \(r, v) ->
    putStrLn $ "  x[" ++ show (bvIntegerU r) ++ "] = " ++ show v
  putStrLn "Final FP register state:"
  forM_ (assocs fregisters) $ \(r, v) ->
    putStrLn $ "  f[" ++ show (bvIntegerU r) ++ "] = " ++ show v

  case mLogFile of
    Nothing -> return ()
    Just logFile -> withFile logFile WriteMode $ \h -> do
      hPutStrLn h "\n--------Coverage report--------\n"
      forM_ (Map.toList testMap) $ \(Some opcode, vals) -> do
        case MapF.lookup opcode (knownCoverageWithRepr rvRepr) of
          Just (InstExprList exprs) -> do
            let ones = length (filter (==1) vals)
            hPutStrLn h $ show opcode ++ " (" ++ show ones ++ "/" ++ show (length vals) ++ ") :"
            forM_ (zip exprs vals) $ \(expr, val) ->
              hPutStrLn h $ "  " ++ prettyShow expr ++ " ---> " ++ show val
          _ -> return ()

-- | From an Elf file, get a list of the byte strings to load into memory along with
-- their starting addresses.
elfBytes :: (KnownNat w, ElfWidthConstraints w) => Elf w -> [(BitVector w, BS.ByteString)]
elfBytes e = pairWithAddr <$> filter memoryMapped sections
  where sections = e ^.. elfSections
        memoryMapped section = elfSectionAddr section > 0
        pairWithAddr section = (fromIntegral $ elfSectionAddr section, elfSectionData section)
