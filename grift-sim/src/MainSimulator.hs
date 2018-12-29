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
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Array.IArray
import           Data.Bits
import           Data.BitVector.Sized
import qualified Data.ByteString as BS
import           Data.Char (toUpper)
import           Data.Foldable
import           Data.IORef
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (fromJust)
import           Data.Parameterized
import           Data.Parameterized.List
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Map (MapF)
import           Data.Traversable (for)
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

-- TODO: Generalize simTrackedOpcode to simAction, which is a single "action" that we
-- are doing after simulation -- generating a report of some kind (coverage, mem
-- dump, register dump, or some combination thereof
data SimOpts rv = SimOpts
  { simSteps :: Int
  , simRV :: RVRepr rv
  , simTrackedOpcode :: TrackedOpcode rv -- Maybe (Some (Opcode rv))
  , simHaltPC :: Maybe Word64
  , simMemDumpStart :: Word64
  , simMemDumpEnd :: Word64
  }

defaultSimOpts :: SimOpts RV64GC
defaultSimOpts = SimOpts
  { simSteps = 10000
  , simRV = knownRepr :: RVRepr RV64GC
  , simTrackedOpcode = NoOpcode
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
                Just (Some rv) -> case simTrackedOpcode opts of
                  NoOpcode -> return $ Some $ opts { simRV = rv
                                                   , simTrackedOpcode = NoOpcode
                                                   }
                  SomeOpcode (Some oc) -> case opcodeCast rv oc of
                    Just oc' -> return $ Some $ opts { simRV = rv
                                                     , simTrackedOpcode = SomeOpcode (Some oc') }
                    Nothing  -> return $ Some $ opts { simRV = rv, simTrackedOpcode = NoOpcode } )
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
    (ReqArg (\ocStr (Some opts) ->
               case (toUpper <$> ocStr) of
                 "ALL" -> return $ Some $ opts { simTrackedOpcode = AllOpcodes }
                 _ -> case readOpcode ocStr of
                   Nothing -> exitWithUsage $ "Unrecognized --inst-coverage value: " ++ ocStr ++ "\n"
                   Just (Some oc) ->
                     case opcodeCast (simRV opts) oc of
                       Nothing ->
                         exitWithUsage $ "Opcode " ++ ocStr ++ " is not in specified instruction set"
                       Just oc' ->
                         return $ Some $ opts { simTrackedOpcode = SomeOpcode (Some oc') } )
         "OPCODE")
    ("display semantic coverage of a particular instruction\n" ++
     "(\"all\" to print total coverage over all instructions)")
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

  fileNames <- case nonOptions of
    [] -> exitWithUsage $ "error: provide a path to an elf file to simulate\n"
    fns -> return fns

  case simRV opts of
    RVRepr RV32Repr _ -> do
      covMapRef <- newIORef (buildCTMap (simRV opts))
      ms <- for fileNames $ \fileName -> do
        fileBS <- catchIOError (BS.readFile fileName) $ \_ ->
          exitWithUsage $ "error: file \"" ++ fileName ++ "\" does not exist\n"
        case parseElf fileBS of
          Elf32Res _ e -> runElf opts e covMapRef
          _ -> exitWithUsage $
             "Error: expected 32-bit elf file, but " ++ fileName ++ " is not"
      covMap <- readIORef covMapRef
      report opts covMap (last ms)
    RVRepr RV64Repr _ -> do
      covMapRef <- newIORef (buildCTMap (simRV opts))
      ms <- for fileNames $ \fileName -> do
        fileBS <- catchIOError (BS.readFile fileName) $ \_ ->
          exitWithUsage $ "error: file \"" ++ fileName ++ "\" does not exist\n"
        case parseElf fileBS of
          Elf64Res _ e -> runElf opts e covMapRef
          _ -> exitWithUsage $
             "Error: expected 64-bit elf file, but " ++ fileName ++ " is not"
      covMap <- readIORef covMapRef
      report opts covMap (last ms)
    _ -> return ()

report :: KnownRVWidth rv
       => SimOpts rv
       -> MapF (Opcode rv) (InstCTList rv)
       -> LogMachine rv
       -> IO ()
report (SimOpts _ rvRepr trackedOpcode _ memDumpStart memDumpEnd) covMap m = do

  pc         <- readIORef (lmPC m)
  mem        <- readIORef (lmMemory m)
  registers  <- freezeRegisters m
  fregisters <- freezeFRegisters m
  csrs       <- readIORef (lmCSRs m)

  case (trackedOpcode, memDumpEnd > memDumpStart) of
    (_, True) -> reportMemDump rvRepr memDumpStart memDumpEnd mem
    (NoOpcode, _) -> do
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
    (SomeOpcode (Some opcode), _) -> reportInstCoverage rvRepr covMap opcode
    (AllOpcodes, _) -> reportCovMap covMap

-- | Run a single Elf file in simulation, using a coverage map given to us in simulation.
runElf :: ElfWidthConstraints (RVWidth rv)
       => SimOpts rv
       -> Elf (RVWidth rv)
       -> IORef (MapF (Opcode rv) (InstCTList rv))
       -> IO (LogMachine rv)
runElf (SimOpts stepsToRun rvRepr trackedOpcode haltPC _ _) e covMapRef =
  withRVWidth rvRepr $ do
    let byteStrings = elfBytes e
    m <- mkLogMachineWithCovMap
         rvRepr
         (fromIntegral $ elfEntry e)
         0x10000
         byteStrings
         (bitVector <$> fromIntegral <$> haltPC)
         trackedOpcode
         covMapRef

    case trackedOpcode of
      NoOpcode -> runLogMachine stepsToRun m
      _ -> runLogMachineLog stepsToRun m

    return m

reportMemDump :: (KnownRVWidth rv)
              => RVRepr rv
              -> Word64
              -> Word64
              -> Map (BitVector (RVWidth rv)) (BitVector 8)
              -> IO ()
reportMemDump rvRepr memDumpStart memDumpEnd mem =
  forM_ (enumFromThenTo memDumpStart (memDumpStart+4) memDumpEnd) $ \addr -> do
    let [byte0, byte1, byte2, byte3] = fmap
          (\a -> (fromIntegral $ bvIntegerU (Map.findWithDefault 0 (bitVector $ fromIntegral a) mem) :: Word32))
          [addr, addr+1, addr+2, addr+3]
        val = (byte3 `shiftL` 24 .|. byte2 `shiftL` 16 .|. byte1 `shiftL` 8 .|. byte0)
    putStrLn $ pad8 (showHex val "")

  where pad8 :: String -> String
        pad8 s = replicate (8 - length s) '0' ++ s

reportInstCoverage :: RVRepr rv -> MapF (Opcode rv) (InstCTList rv) -> Opcode rv fmt -> IO ()
reportInstCoverage rvRepr covMap opcode = do
  -- the pattern match below should never fail
  let Just sem = MapF.lookup opcode (isSemanticsMap (knownISetWithRepr rvRepr))
  putStrLn "Instruction semantics"
  putStrLn "====================="
  print $ pPrintInstSemantics sem
  putStrLn ""
  putStrLn "Instruction coverage"
  putStrLn "===================="
  case MapF.lookup opcode covMap of
    Just covTrees -> do
      traverse_ print (pPrintInstCTList rvRepr opcode covTrees)
      let (hitBranches, totalBranches) = countInstCTList covTrees
      putStrLn $ "Covered " ++ show hitBranches ++ " of " ++ show totalBranches ++
        " total branches."
    Nothing -> putStrLn $ "Instruction never encountered."

  putStrLn "\n(green = fully covered, red = not covered, cyan = covered true, yellow = covered false)"


reportCovMap :: MapF (Opcode rv) (InstCTList rv) -> IO ()
reportCovMap covMap = do
  let covTrees = MapF.elems covMap
      treeCounts = viewSome countInstCTList <$> covTrees
      (hitBranches, totalBranches) = foldl (\(a,b) (c,d) -> (a+c,b+d)) (0,0) treeCounts
  for_ (MapF.toList covMap) $ \(Pair opcode ct) -> do
    let (opcodeHitBranches, opcodeTotalBranches) = countInstCTList ct
    putStrLn $ show (pPrint opcode) ++ ": " ++ show opcodeHitBranches ++ "/" ++
      show opcodeTotalBranches ++ " branches covered"
  putStrLn $ "Covered " ++ show hitBranches ++ " of " ++
    show totalBranches ++ " total branches in the instruction set."

-- | From an Elf file, get a list of the byte strings to load into memory along with
-- their starting addresses.
elfBytes :: (KnownNat w, ElfWidthConstraints w) => Elf w -> [(BitVector w, BS.ByteString)]
elfBytes e = pairWithAddr <$> filter memoryMapped sections
  where sections = e ^.. elfSections
        memoryMapped section = elfSectionAddr section > 0
        pairWithAddr section = (fromIntegral $ elfSectionAddr section, elfSectionData section)
