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
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

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
import           Data.BitVector.Sized.Unsigned (UnsignedBV(..))
import qualified Data.ByteString as BS
import           Data.Char (toUpper, ord)
import           Data.ElfEdit hiding (header)
import           Data.Foldable
import           Data.IORef
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Parameterized
import           Data.Parameterized.List
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Map (MapF)
import           Data.Traversable (for)
import qualified Data.Vector as V
import           Data.Word
import           GHC.TypeLits
import           Options.Applicative
import           Numeric (showHex)
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.IO.Error
import           Text.PrettyPrint.HughesPJClass hiding ((<>))
import           Text.Read

import           GRIFT.InstructionSet
import           GRIFT.InstructionSet.Known
import           GRIFT.Types
import           GRIFT.Semantics
import           GRIFT.Semantics.Pretty
import           GRIFT.Semantics.Utils
import           GRIFT.Simulation
import           GRIFT.Simulation.LogMachine

import Data.Coerce

rvReprFromString :: String -> Either String (Some RVRepr)
rvReprFromString s = case s of
  "RV32I" -> Right $ Some (knownRepr :: RVRepr RV32I)
  "RV32IM" -> Right $ Some (knownRepr :: RVRepr RV32IM)
  "RV32IMA" -> Right $ Some (knownRepr :: RVRepr RV32IMA)
  "RV32IMAF" -> Right $ Some (knownRepr :: RVRepr RV32IMAF)
  "RV32G" -> Right $ Some (knownRepr :: RVRepr RV32G)
  "RV32GC" -> Right $ Some (knownRepr :: RVRepr RV32GC)
  "RV64I" -> Right $ Some (knownRepr :: RVRepr RV64I)
  "RV64IM" -> Right $ Some (knownRepr :: RVRepr RV64IM)
  "RV64IMA" -> Right $ Some (knownRepr :: RVRepr RV64IMA)
  "RV64IMAF" -> Right $ Some (knownRepr :: RVRepr RV64IMAF)
  "RV64G" -> Right $ Some (knownRepr :: RVRepr RV64G)
  "RV64GC" -> Right $ Some (knownRepr :: RVRepr RV64GC)
  str -> Left $ "Unknown configuration " ++ str

trackedOpcodeFromString :: String -> Either String (TrackedOpcode RV64GC)
trackedOpcodeFromString str = case (toUpper <$> str) of
  "ALL" -> Right AllOpcodes
  str -> case readOpcode str of
    Nothing -> Left $ "Unknown opcode " ++ str
    Just (Some opcode) -> Right $ SomeOpcode (Some opcode)

data SimCfg rv = SimCfg
  { simFilenames :: [FilePath]
  , simRepr :: RVRepr rv
  , simSteps :: Int
  , simHaltPC :: Maybe Addr
  , simReportType :: ReportType rv
  }

data Opts = Opts { optsFilenames :: [FilePath]
                 , optsRepr :: Some RVRepr
                 , optsSteps :: Int
                 , optsHaltPC :: Maybe Addr
                 , optsReportType :: ReportType RV64GC
                 }

validateOpts :: Opts -> Either String (Some SimCfg)
validateOpts (Opts filenames (Some rvRepr) steps haltPC reportType) =
  case reportType of
    NoReport -> return $ Some $ SimCfg filenames rvRepr steps haltPC NoReport
    MemDump memRange -> return $ Some $ SimCfg filenames rvRepr steps haltPC (MemDump memRange)
    RegDump -> return $ Some $ SimCfg filenames rvRepr steps haltPC RegDump
    CoverageReport NoOpcode ->
      return $ Some $ SimCfg filenames rvRepr steps haltPC (CoverageReport NoOpcode)
    CoverageReport AllOpcodes ->
      return $ Some $ SimCfg filenames rvRepr steps haltPC (CoverageReport AllOpcodes)
    CoverageReport (SomeOpcode (Some opcode)) -> do
      (opcode, _) <- maybe
        (Left $ "Opcode " ++ show (pPrint opcode) ++ " not in " ++ show (pPrint rvRepr))
        return
        (opcodeCast rvRepr opcode)
      return $ Some $ SimCfg filenames rvRepr steps haltPC (CoverageReport (SomeOpcode (Some opcode)))

optsParser :: Parser Opts
optsParser = Opts
  <$> some ( strArgument
             ( help "name(s) of ELF file(s) to run in simulation"
               <> metavar "FILES") )
  <*> option (eitherReader rvReprFromString)
      ( help "RISC-V architecture variant"
        <> long "arch"
        <> short 'a'
        <> metavar "ARCH"
        <> value (Some rv64GCRepr)
        <> showDefaultWith (const "RV64GC") )
  <*> option auto
      ( help "max # of steps to take in simulation"
        <> long "steps"
        <> short 's'
        <> metavar "STEPS"
        <> value 10000
        <> showDefault )
  <*> optional (addrParser "halt-pc" "halt PC at address/symbol")
  <*> reportTypeParser

data ReportType rv = MemDump MemDumpRange
                   | RegDump
                   | CoverageReport (TrackedOpcode rv)
                   | NoReport
  deriving Show

reportTypeParser :: Parser (ReportType RV64GC)
reportTypeParser =
  pure NoReport <|>
  (MemDump <$> memDumpRangeParser) <|>
  (flag' RegDump
   ( help "post-simulation register file dump"
     <> long "reg-dump" ) ) <|>
  (CoverageReport <$>
    option (eitherReader trackedOpcodeFromString)
    ( help "display semantic coverage of instruction. Input is either an opcode or \"all\", for a full coverage report."
      <> long "inst-coverage"
      <> metavar "OPCODE" ) )

data MemDumpRange = MemDumpRange { dumpLow :: Addr
                                 , dumpHigh :: Addr
                                 }
  deriving (Eq, Show)

data Addr = ConcreteAddr Word64
          | SymbolAddr String
  deriving (Eq, Show)

memDumpRangeParser :: Parser MemDumpRange
memDumpRangeParser = MemDumpRange
  <$> addrParser "mem-dump-begin" "beginning address/symbol of memory dump"
  <*> addrParser "mem-dump-end" "end address/symbol of memory dump (non-inclusive)"

addrParser :: String -> String -> Parser Addr
addrParser longText helpText =
  option addrReader
  ( help helpText
    <> long longText
    <> metavar "ADDR" )
  where addrReader = do
          loc <- str
          let locAsAddr :: Maybe Word64 = readMaybe loc
          case locAsAddr of
            Just addr -> return (ConcreteAddr addr)
            Nothing -> return (SymbolAddr loc)

main :: IO ()
main = do
  opts <- execParser optsParserInfo
  case validateOpts opts of
    Left e -> do putStrLn e
                 exitFailure
    Right (Some cfg) -> griftSim cfg
  where optsParserInfo = info (optsParser <**> helper)
               ( fullDesc
                 <> progDesc "Run RISC-V ELF binary(ies)"
                 <> header "grift-sim -- RISC-V simulator" )

-- | Parser an ELF file, ensuring its width matches a RISC-V configuration. We also
-- make sure to satisfy `ElfWidthConstraints` while we're at it.
parseRVElf :: RVRepr rv
           -> BS.ByteString
           -> a
           -> (Elf (RVWidth rv) -> (ElfWidthConstraints (RVWidth rv) => a))
           -> a
parseRVElf (RVRepr RV32Repr _) bs d k = case parseElf bs of
  Elf32Res _ e -> k e
  _ -> d
parseRVElf (RVRepr RV64Repr _) bs d k = case parseElf bs of
  Elf64Res _ e -> k e
  _ -> d
parseRVElf _ _ d _ = d

griftSim :: SimCfg rv -> IO ()
griftSim cfg = do
  covMapRef <- newIORef (buildCTMap (simRepr cfg))
  results <- for (simFilenames cfg) $ \filename -> do
    fileBS <- catchIOError (BS.readFile filename) $ \_ -> do
      putStrLn $ "error: file \"" ++ filename ++ "\" does not exist\n"
      exitFailure
    (m, e) <- parseRVElf (simRepr cfg) fileBS ((putStrLn $ "error: incompatible elf file") >> exitFailure) $ \e -> do
      m <- runElf cfg e covMapRef
      return (m, e)
    return (m, e)
  covMap <- readIORef covMapRef
  let (m, e) = last results
  elfClassInstances (elfClass e) $
    report cfg covMap m e

resolveAddr :: ElfWidthConstraints w => Elf w -> Addr -> Maybe Word64
resolveAddr _ (ConcreteAddr addr) = return addr
resolveAddr e (SymbolAddr symbol) = do
  let symTabs = elfSymtab e
      entries = toList (V.concat (elfSymbolTableEntries <$> symTabs))
      match ste = steName ste == BS.pack (map (fromIntegral . ord) symbol)
  entry <- find match entries
  return $ fromIntegral (steValue entry)

report :: ElfWidthConstraints w
       => SimCfg rv
       -> MapF (Opcode rv) (InstCTList rv)
       -> LogMachine rv
       -> Elf w
       -> IO ()
report cfg covMap m e = withRV (simRepr cfg) $ do

  pc   <- readIORef (lmPC m)
  mem  <- readIORef (lmMemory m)
  gprs <- freezeGPRs m
  fprs <- freezeFPRs m
  csrs <- readIORef (lmCSRs m)

  case simReportType cfg of
    NoReport -> return ()
    RegDump -> reportRegDump (simRepr cfg) pc gprs fprs csrs
    MemDump (MemDumpRange lo hi) -> do
      loAddr <- return $
        fromMaybe (error $ "could not resolve symbol " ++ show lo) (resolveAddr e lo)
      hiAddr <- return $
        fromMaybe (error $ "could not resolve symbol " ++ show hi) (resolveAddr e hi)
      reportMemDump (simRepr cfg) loAddr hiAddr mem
    CoverageReport (SomeOpcode (Some opcode)) -> reportInstCoverage (simRepr cfg) covMap opcode
    CoverageReport AllOpcodes -> reportCovMap covMap
    rt -> error $ "Unrecognized report type " ++ show rt

-- | Run a single Elf file in simulation, using a coverage map given to us in simulation.
runElf :: ElfWidthConstraints (RVWidth rv)
       => SimCfg rv
       -> Elf (RVWidth rv)
       -> IORef (MapF (Opcode rv) (InstCTList rv))
       -> IO (LogMachine rv)
runElf cfg e covMapRef =
  withRV (simRepr cfg) $ do
    let byteStrings = elfBytes e
    let trackedOpcode = case simReportType cfg of
          CoverageReport toc -> toc
          _ -> NoOpcode
    let haltPC = resolveAddr e =<< simHaltPC cfg
    m <- mkLogMachineWithCovMap
         (simRepr cfg)
         (fromIntegral $ elfEntry e)
         0x10000
         byteStrings
         (fromIntegral <$> haltPC)
         trackedOpcode
         covMapRef

    case trackedOpcode of
      NoOpcode -> withRV (simRepr cfg) $ runLogMachine (simSteps cfg) m
      _ -> withRV (simRepr cfg) $ runLogMachineLog (simSteps cfg) m

    return m

reportRegDump :: (KnownRV rv)
              => RVRepr rv
              -> UnsignedBV (RVWidth rv)
              -> Array (UnsignedBV 5) (UnsignedBV (RVWidth rv))
              -> Array (UnsignedBV 5) (UnsignedBV (RVFloatWidth rv))
              -> Map (UnsignedBV 12) (UnsignedBV (RVWidth rv))
              -> IO ()
reportRegDump rvRepr pc gprs fprs csrs = do
  putStrLn $ "MInstRet = " ++
    show (asUnsigned (asBV (Map.findWithDefault 0 (UnsignedBV (encodeCSR MInstRet)) csrs)))
  putStrLn $ "MEPC = " ++ showHexUBV (Map.findWithDefault 0 (UnsignedBV (encodeCSR MEPC)) csrs)
  putStrLn $ "MTVal = " ++ showHexUBV (Map.findWithDefault  0 (UnsignedBV (encodeCSR MTVal)) csrs)
  putStrLn $ "MCause = " ++ showHexUBV (Map.findWithDefault 0 (UnsignedBV (encodeCSR MCause)) csrs)
  putStrLn $ "FCSR = " ++ showHexUBV (Map.findWithDefault 0 (UnsignedBV (encodeCSR FCSR)) csrs)
  putStrLn $ "Final PC: " ++ showHexUBV pc
  putStrLn "Final register state:"
  forM_ (assocs gprs) $ \(r, v) ->
    putStrLn $ "  x[" ++ show (asUnsigned (asBV r)) ++ "] = " ++ showHexUBV v
  putStrLn "Final FP register state:"
  forM_ (assocs fprs) $ \(r, v) ->
    putStrLn $ "  f[" ++ show (asUnsigned (asBV r)) ++ "] = " ++ showHexUBV v
  where
    showHexUBV (UnsignedBV bv) = "0x" ++ (showHex (asUnsigned bv) "")

reportMemDump :: (KnownRV rv)
              => RVRepr rv
              -> Word64
              -> Word64
              -> Map (UnsignedBV (RVWidth rv)) (UnsignedBV 8)
              -> IO ()
reportMemDump rvRepr memDumpStart memDumpEnd mem =
  forM_ (enumFromThenTo memDumpStart (memDumpStart+4) (memDumpEnd-4)) $ \addr -> do
    let [byte0, byte1, byte2, byte3] = fmap
          (\a -> (fromIntegral $ asUnsigned $ asBV $ (Map.findWithDefault 0 (fromIntegral a) mem) :: Word32))
          [addr, addr+1, addr+2, addr+3]
        val = (byte3 `shiftL` 24 .|. byte2 `shiftL` 16 .|. byte1 `shiftL` 8 .|. byte0)
    putStrLn $ pad8 (showHex val "")

  where pad8 :: String -> String
        pad8 s = replicate (8 - length s) '0' ++ s

reportInstCoverage :: RVRepr rv -> MapF (Opcode rv) (InstCTList rv) -> Opcode rv fmt -> IO ()
reportInstCoverage rvRepr covMap opcode = withRV rvRepr $ do
  -- the pattern match below should never fail
  let Just sem = MapF.lookup opcode (isSemanticsMap (knownISetWithRepr rvRepr))
  putStrLn "Instruction semantics"
  putStrLn "====================="
  print $ pPrintInstSemantics Abbrev sem
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
elfBytes :: (KnownNat w, ElfWidthConstraints w) => Elf w -> [(UnsignedBV w, BS.ByteString)]
elfBytes e = pairWithAddr <$> filter memoryMapped sections
  where sections = e ^.. elfSections
        memoryMapped section = elfSectionAddr section > 0
        pairWithAddr section = (fromIntegral $ elfSectionAddr section, elfSectionData section)
