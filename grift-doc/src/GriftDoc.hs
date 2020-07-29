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
import Options.Applicative
import System.Exit (exitFailure)
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import GRIFT.Decode
import GRIFT.InstructionSet
import GRIFT.InstructionSet.Known
import GRIFT.Semantics
import GRIFT.Semantics.Pretty
import GRIFT.Types

data Opts = Opts { optsRepr :: Some RVRepr
                 , optsAbbrev :: AbbrevLevel
                 , optsOpcode :: Some (Opcode RV64GC)
                 }

optsParser :: Parser Opts
optsParser = Opts
  <$> rvReprParser
  <*> abbrevParser
  <*> opcodeParser

rvReprParser :: Parser (Some RVRepr)
rvReprParser = option (eitherReader rvReprFromString)
  ( help "RISC-V architecture variant"
    <> long "arch"
    <> short 'a'
    <> metavar "ARCH"
    <> value (Some rv64GCRepr)
    <> showDefaultWith (const "RV64GC") )

opcodeParser :: Parser (Some (Opcode RV64GC))
opcodeParser = argument (eitherReader readOpcodeEither)
  ( help "name of opcode"
    <> metavar "OPCODE")
  where readOpcodeEither :: String -> Either String (Some (Opcode RV64GC))
        readOpcodeEither s = case readOpcode s of
          Nothing -> Left $ "Unknown opcode " ++ s
          Just oc -> Right oc

abbrevParser :: Parser AbbrevLevel
abbrevParser = flag Abbrev NoAbbrev
  ( help "don't abbreviate semantics"
    <> long "verbose"
    <> short 'v' )

main = griftDoc =<< execParser opts
  where
    opts = info (optsParser <**> helper)
           ( fullDesc
             <> progDesc "Display RISC-V instruction documentation"
             <> header "grift-doc -- RISC-V documentation tool" )

griftDoc :: Opts -> IO ()
griftDoc (Opts (Some rvRepr) abbrevLevel (Some opcode)) = case opcodeCast rvRepr opcode of
  Nothing -> exitIncompatibleOpcode rvRepr opcode
  Just (opcode, fmtRepr) -> do
    let iset = knownISetWithRepr rvRepr
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
    print (withRV rvRepr $ pPrintInstSemantics abbrevLevel sem :: Doc)

  where exitIncompatibleOpcode :: RVRepr rv -> Opcode rv' fmt -> IO ()
        exitIncompatibleOpcode rvRepr opcode = do
          putStrLn $ "Error: opcode " ++ show (pPrint opcode) ++ " not in " ++ show (pPrint rvRepr)
          exitFailure

listSome :: List l sh -> [Some l]
listSome = ifoldr (\_ layout rst -> Some layout : rst) []

data InstructionChunk w = ConcreteBV (BitVector w) | OperandBV (OperandName w)

pPrintBinary :: BitVector w -> String
pPrintBinary (BitVector wRepr x) = pad0 (natValue wRepr) $ showIntAtBase 2 intToDigit x ""

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
