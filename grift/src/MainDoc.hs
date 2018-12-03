{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import Control.Monad
import Data.List
import Data.Parameterized
import Data.Parameterized.Map
import Text.PrettyPrint.HughesPJClass
import System.Environment

import GRIFT.InstructionSet
import GRIFT.InstructionSet.Known
import GRIFT.Semantics
import GRIFT.Types

isetFromString :: String -> Maybe (Some InstructionSet)
isetFromString s = case s of
  "RV32I" -> Just $ Some (knownISet :: InstructionSet RV32I)
  "RV32IM" -> Just $ Some (knownISet :: InstructionSet RV32IM)
  "RV32IMA" -> Just $ Some (knownISet :: InstructionSet RV32IMA)
  "RV32IMAF" -> Just $ Some (knownISet :: InstructionSet RV32IMAF)
  "RV32G" -> Just $ Some (knownISet :: InstructionSet RV32G)
  "RV32GC" -> Just $ Some (knownISet :: InstructionSet RV32GC)
  "RV64I" -> Just $ Some (knownISet :: InstructionSet RV64I)
  "RV64IM" -> Just $ Some (knownISet :: InstructionSet RV64IM)
  "RV64IMA" -> Just $ Some (knownISet :: InstructionSet RV64IMA)
  "RV64IMAF" -> Just $ Some (knownISet :: InstructionSet RV64IMAF)
  "RV64G" -> Just $ Some (knownISet :: InstructionSet RV64G)
  "RV64GC" -> Just $ Some (knownISet :: InstructionSet RV64GC)
  _ -> Nothing

main :: IO ()
main = do
  args <- getArgs
  someISet <- case args of
    [s] | Just iset' <- isetFromString s -> return iset'
    _ -> do
      putStrLn $ "Defaulting to RV32I"
      return $ Some (knownISet :: InstructionSet RV32I)
  case someISet of
    Some iset -> do
      forM_ (sortBy pairSort $ toList (isSemanticsMap iset)) $ \(Pair opcode semantics) -> do
        putStrLn $ show opcode ++ ": "
        print (nest 4 $ pPrintInstSemantics semantics)
  where pairSort :: Pair (Opcode rv) (InstSemantics rv) -> Pair (Opcode rv) (InstSemantics rv) -> Ordering
        pairSort p1@(Pair oc1 _) p2@(Pair oc2 _) = compare (show oc1) (show oc2)
