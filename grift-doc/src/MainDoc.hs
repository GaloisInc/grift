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
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-|
Module      : MainDoc
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Tool for emitting documentation on RISC-V instructions, both their semantics and
their encodings.
-}

module Main where

import Control.Monad (when)
import Data.Bifunctor (bimap)
import Data.BitVector.Sized
import Data.BitVector.Sized.BitLayout
import Data.Char (toUpper, intToDigit)
import Data.Foldable (forM_)
import Data.List (sortBy)
import Data.Parameterized
import Data.Parameterized.List
import qualified Data.Parameterized.Map as MapF
import Numeric (showHex, showIntAtBase)
import System.Console.GetOpt
import System.Environment
import System.Exit
import Text.PrettyPrint.HughesPJClass

import GRIFT.Decode
import GRIFT.InstructionSet
import GRIFT.InstructionSet.Known
import GRIFT.Semantics
import GRIFT.Types

-- Utilities

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

data Action = ActionDocOpcode
            | ActionShowHelp

data DocOpts rv = DocOpts
  { optsRV :: RVRepr rv
  , optsAction :: Action
  }

defaultDocOpts :: DocOpts RV64GC
defaultDocOpts = DocOpts knownRepr ActionShowHelp

options :: [OptDescr (Some DocOpts -> IO (Some DocOpts))]
options =
  [ Option ['h'] ["help"]
    (NoArg (\_ -> exitWithUsage ""))
    ("display help message")
  , Option ['a'] ["arch"]
    (ReqArg (\rvStr (Some opts) -> case rvReprFromString rvStr of
                Nothing -> exitWithUsage $ "Unrecognized --arch value: " ++ rvStr ++ "\n"
                Just (Some rvRepr) -> return $ Some $ opts { optsRV = rvRepr } )
     "ARCH")
    ("RISC-V arch configuration (default = RV64GC)")
  ]

header :: String
header = "Usage: grift-doc [-a ARCH] opcode"

exitWithUsage :: String -> IO a
exitWithUsage "" = do
  putStrLn $ usageInfo header options
  exitWith (ExitFailure 1)
exitWithUsage msg = do
  putStrLn msg
  putStrLn $ usageInfo header options
  exitWith (ExitFailure 1)

main = do
  args <- getArgs
  let (actions, nonOptions, errors) = getOpt Permute options args

  -- First check that there were no option errors
  when (not (null errors)) $ exitWithUsage (concat errors)

  -- Next build up the options, potentially exiting early
  Some opts <- foldl (>>=) (return (Some defaultDocOpts)) actions

  ocStr <- case nonOptions of
    [oc] -> return oc
    _ -> exitWithUsage $ "error: provide exactly one opcode"

  case readOpcode (toUpper <$> ocStr) of
    Nothing -> exitWithUsage $ "Unrecognized opcode value: " ++ ocStr ++ "\n"
    Just (Some opcode') -> case opcodeCast (optsRV opts) opcode' of
      Nothing -> exitWithUsage $ "Opcode " ++ ocStr ++ " is not in specified instruction set"
      Just (opcode, fmtRepr) -> do
        let iset = knownISetWithRepr (optsRV opts)
        let Just sem@(InstSemantics _ opNames') = MapF.lookup opcode (isSemanticsMap iset)
        let opNames = listSome opNames'
        let operandsLayout = viewSome bitLayoutAssignmentList <$> (listSome $ operandsLayouts fmtRepr)
        let Just (OpBits _ opBits') = MapF.lookup opcode (isEncodeMap iset)
        let opBits = listSome opBits'
        let opBitsLayout = viewSome bitLayoutAssignmentList <$> (listSome $ opBitsLayouts fmtRepr)
        let opBitsPairs = zip opBitsLayout opBits
        let operandsPairs = zip operandsLayout opNames

        let cmpFst (a,_) (b,_) = compare a b
        let placeholders = sortBy cmpFst ((concat $ uncurry opbitsPlaceholderList <$> opBitsPairs) ++
                                          (concat $ uncurry operandsPlaceholderList <$> operandsPairs))

        putStrLn ""
        putStrLn $ show (pPrint opcode) ++ " encoding"
        putStrLn "====================="
        putStrLn ""
        putStrLn $ reverse $ pPrintPHList placeholders
        putStrLn "instruction bit : value"
        forM_ placeholders $ \(i, ph) -> do
          putStrLn $ padSpaceEnd 16 (show i) ++ ": " ++ pPrintPH ph
        putStrLn ""
        putStrLn $ show (pPrint opcode) ++ " semantics"
        putStrLn "====================="
        putStrLn ""
        print $ pPrintInstSemantics sem

listSome :: List l sh -> [Some l]
listSome = ifoldr (\_ layout rst -> Some layout : rst) []

data InstructionChunk w = ConcreteBV (BitVector w) | OperandBV (OperandName w)

pPrintBinary :: BitVector w -> String
pPrintBinary (BV wRepr x) = pad0 (natValue wRepr) $ showIntAtBase 2 intToDigit x ""

pad0 :: Integral a => a -> String -> String
pad0 n s = replicate (fromIntegral n - length s) '0' ++ s

padSpaceEnd :: Integral a => a -> String -> String
padSpaceEnd n s = s ++ replicate (fromIntegral n - length s) ' '

data BitPlaceHolder = BP0 | BP1 | BPOperand (Some OperandName) Int
  deriving Show

pPrintPH :: BitPlaceHolder -> String
pPrintPH BP0 = "0"
pPrintPH BP1 = "1"
pPrintPH (BPOperand (Some on) ix) = show (pPrint on) ++ "[" ++ show ix ++ "]"

opbitsPlaceholderList :: [Int] -> Some BitVector -> [(Int, BitPlaceHolder)]
opbitsPlaceholderList ixs sbv = zip ixs (bphFromChar <$> viewSome pPrintBinary sbv)
  where bphFromChar '1' = BP1
        bphFromChar _ = BP0

operandsPlaceholderList :: [Int] -> Some OperandName -> [(Int, BitPlaceHolder)]
operandsPlaceholderList ixs on = zip ixs (bphFromIx <$> [0..])
  where bphFromIx n = BPOperand on n

pPrintPHList :: [(Int, BitPlaceHolder)] -> String
pPrintPHList [] = ""
pPrintPHList ((_,BPOperand _ _):rst) = 'x' : pPrintPHList rst
pPrintPHList ((_,ph):rst) = pPrintPH ph ++ pPrintPHList rst
