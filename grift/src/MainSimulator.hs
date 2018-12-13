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

import           Control.Lens ( (^..), (^.) )
import           Control.Monad
import           Data.Array.IArray
import           Data.Bits
import           Data.BitVector.Sized
import qualified Data.ByteString as BS
import           Data.Foldable
import           Data.IORef
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.Parameterized
import           Data.Parameterized.List
import qualified Data.Parameterized.Map as MapF
import           Data.Word
import           System.Environment
import           System.Exit
import           Data.ElfEdit
import           GHC.TypeLits
import           Numeric (showHex)
import           System.Console.GetOpt
import           System.FilePath.Posix
import           System.IO
import           System.IO.Error
import           Text.PrettyPrint.HughesPJClass
import           Text.Read

import           GRIFT.Coverage
import           GRIFT.InstructionSet
import           GRIFT.InstructionSet.Known
import           GRIFT.InstructionSet.Utils
import           GRIFT.Types
import           GRIFT.Semantics
import           GRIFT.Simulation
import           GRIFT.Simulation.LogMachine

import Data.Coerce

rvReprFromString :: String -> Maybe (Some RVRepr)
rvReprFromString s = case s of
  "RV32I" -> Just $ Some (knownRepr :: RVRepr RV32I)
  "RV32IM" -> Just $ Some (knownRepr :: RVRepr RV32IM)
  "RV32IMA" -> Just $ Some (knownRepr :: RVRepr RV32IMA)
  "RV32IMAF" -> Just $ Some (knownRepr :: RVRepr RV32IMAF)
  "RV32G" -> Just $ Some (knownRepr :: RVRepr RV32G)
  "RV32GC" -> Just $ Some (knownRepr :: RVRepr RV32GC)
  "RV64I" -> Just $ Some (knownRepr :: RVRepr RV64I)
  "RV64IM" -> Just $ Some (knownRepr :: RVRepr RV64IM)
  "RV64IMA" -> Just $ Some (knownRepr :: RVRepr RV64IMA)
  "RV64IMAF" -> Just $ Some (knownRepr :: RVRepr RV64IMAF)
  "RV64G" -> Just $ Some (knownRepr :: RVRepr RV64G)
  "RV64GC" -> Just $ Some (knownRepr :: RVRepr RV64GC)
  _ -> Nothing

data SimOpts rv = SimOpts
  { simSteps :: Int
  , simRV :: RVRepr rv
--  , simCovFile :: Maybe String
  , simOpcodeCov :: Maybe (Some (Opcode rv))
  , simHaltPC :: Maybe Word64
  , simMemDumpStart :: Word64
  , simMemDumpEnd :: Word64
  }

defaultSimOpts :: SimOpts RV64GC
defaultSimOpts = SimOpts
  { simSteps = 10000
  , simRV = knownRepr :: RVRepr RV64GC
--  , simCovFile = Nothing
  , simOpcodeCov = Nothing
  , simHaltPC = Nothing
  , simMemDumpStart = 0x0
  , simMemDumpEnd = 0x0
  }

-- TODO: Idea -- if the opcode does not belong to the current instruction set,
-- augment the instruction set minimally to include it.
options :: [OptDescr (Some SimOpts -> IO (Some SimOpts))]
options =
  [ Option ['s'] ["steps"]
    (ReqArg (\stepStr (Some opts) -> case readMaybe stepStr of
                Nothing    -> exitWithUsage $ "Illegal value for --steps: " ++ stepStr ++ "\n"
                Just steps -> return $ Some $ opts { simSteps = steps } )
     "NUM")
    ("max # of simulation steps (default = " ++ show (simSteps defaultSimOpts) ++ ")")
  , Option ['a'] ["arch"]
    (ReqArg (\rvStr (Some opts) -> case rvReprFromString rvStr of
                Nothing    -> exitWithUsage $ "Unrecognized --arch value: " ++ rvStr ++ "\n"
                Just (Some rv) -> case simOpcodeCov opts of
                  Nothing -> return $ Some $ opts { simRV = rv
                                                  , simOpcodeCov = Nothing
                                                  }
                  Just (Some oc) -> case opcodeCast rv oc of
                    Just oc' -> return $ Some $ opts { simRV = rv, simOpcodeCov = Just (Some oc') }
                    Nothing  -> return $ Some $ opts { simRV = rv, simOpcodeCov = Nothing } )
     "ARCH")
    ("RISC-V arch configuration (default = RV64GC)")
  -- , Option ['c'] ["coverage"]
  --   (ReqArg (\covStr (Some opts) -> return $ Some $ opts { simCovFile = Just covStr })
  --    "FILE")
  --   ("Print coverage analysis to file")
  , Option ['h'] ["help"]
    (NoArg (\_ -> exitWithUsage ""))
    ("display help message")
  , Option [] ["inst-coverage"]
    (ReqArg (\ocStr (Some opts) -> case readOpcode ocStr of
                Nothing -> exitWithUsage $ "Unrecognized --inst-coverage value: " ++ ocStr ++ "\n"
                Just (Some oc) ->
                  case opcodeCast (simRV opts) oc of
                    Nothing -> exitWithUsage $ "Opcode " ++ ocStr ++ " is not in specified instruction set"
                    Just oc' -> return $ Some $ opts { simOpcodeCov = Just (Some oc') } )
      "OPCODE")
    "display semantic coverage of a particular instruction"
  , Option [] ["halt-pc"]
    (ReqArg (\addrStr (Some opts) -> case readMaybe addrStr of
                Nothing -> exitWithUsage $ "Invalid --halt-pc value: " ++ addrStr ++ "\n"
                Just addr ->
                  return $ Some $ opts { simHaltPC = Just addr }  )
     "ADDR")
    ("address of PC to trigger a halt (default = none)")
  , Option [] ["mem-dump-start"]
    (ReqArg (\addrStr (Some opts) -> case readMaybe addrStr of
                Nothing -> exitWithUsage $ "Invalid --mem-dump-start value: " ++ addrStr ++ "\n"
                Just addr ->
                  return $ Some $ opts { simMemDumpStart = addr } )
      "ADDR")
    ("start address of post-simulation memory dump (default = " ++ show (simMemDumpStart defaultSimOpts) ++ ")")
  , Option [] ["mem-dump-end"]
    (ReqArg (\lenStr (Some opts) -> case readMaybe lenStr of
                Nothing -> exitWithUsage $ "Invalid --mem-dump-end value: " ++ lenStr ++ "\n"
                Just len ->
                  return $ Some $ opts { simMemDumpEnd = len } )
      "ADDR")
    ("end address of post-simulation memory dump (default = " ++ show (simMemDumpEnd defaultSimOpts) ++ ")")
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
  Some opts <- foldl (>>=) (return (Some defaultSimOpts)) actions

  -- Next check that there is exactly one argument, a path to an elf file
  fileName <- case nonOptions of
    [s] -> return s
    _ -> exitWithUsage $ "error: provide a path to an elf file to simulate\n"

  -- Next check that the file actually exists
  fileBS <- catchIOError (BS.readFile fileName) $ \_ ->
    exitWithUsage $ "error: file \"" ++ fileName ++ "\" does not exist\n"

  case (simRV opts, parseElf fileBS) of
    (rvRepr@(RVRepr RV32Repr _), Elf32Res _ e) ->
      runElf opts e
    (rvRepr@(RVRepr RV64Repr _), Elf64Res _ e) ->
      runElf opts e
    _ -> exitWithUsage $ "Error: bad object file\n"

runElf :: ElfWidthConstraints (RVWidth rv)
       => SimOpts rv
       -> Elf (RVWidth rv)
       -> IO ()
runElf (SimOpts stepsToRun rvRepr covOpcode haltPC memDumpStart memDumpEnd) e = withRVWidth rvRepr $ do
  let byteStrings = elfBytes e
  m <- mkLogMachine
       rvRepr
       (fromIntegral $ elfEntry e)
       0x10000
       byteStrings
       (bitVector <$> fromIntegral <$> haltPC)
       covOpcode

  case covOpcode of
    Nothing -> runLogMachine stepsToRun m
    Just _  -> runLogMachineLog stepsToRun m

  pc         <- readIORef (lmPC m)
  mem        <- readIORef (lmMemory m)
  registers  <- freezeRegisters m
  fregisters <- freezeFRegisters m
  csrs       <- readIORef (lmCSRs m)
  cov        <- readIORef (lmCov m)

  case (cov, memDumpEnd > memDumpStart) of
    (_, True) -> do
      forM_ (enumFromThenTo memDumpStart (memDumpStart+4) memDumpEnd) $ \addr -> do
        let [byte0, byte1, byte2, byte3] = fmap
              (\a -> (fromIntegral $ bvIntegerU (Map.findWithDefault 0 (bitVector $ fromIntegral a) mem) :: Word32))
              [addr, addr+1, addr+2, addr+3]
            val = (byte3 `shiftL` 24 .|. byte2 `shiftL` 16 .|. byte1 `shiftL` 8 .|. byte0)
        putStrLn $ pad8 (showHex val "")
    (Nothing, _) -> do
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
    (Just (Pair opcode covTrees), _) -> do
      -- the pattern match below should never fail
      let Just sem = MapF.lookup opcode (isSemanticsMap (knownISetWithRepr rvRepr))
      putStrLn "Instruction semantics"
      putStrLn "====================="
      print $ pPrintInstSemantics sem
      putStrLn ""
      putStrLn "Instruction coverage"
      putStrLn "===================="
      traverse_ print (pPrintInstCTList rvRepr opcode covTrees)

      putStrLn "\n(green = fully covered, red = not covered, cyan = covered true, yellow = covered false)"

  where pad8 :: String -> String
        pad8 s = replicate (8 - length s) '0' ++ s

-- | From an Elf file, get a list of the byte strings to load into memory along with
-- their starting addresses.
elfBytes :: (KnownNat w, ElfWidthConstraints w) => Elf w -> [(BitVector w, BS.ByteString)]
elfBytes e = pairWithAddr <$> filter memoryMapped sections
  where sections = e ^.. elfSections
        memoryMapped section = elfSectionAddr section > 0
        pairWithAddr section = (fromIntegral $ elfSectionAddr section, elfSectionData section)
