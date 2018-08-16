{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import Control.Monad
import Data.List
import Data.Parameterized
import Data.Parameterized.Map
import Text.PrettyPrint.HughesPJClass
import System.Environment

import RISCV.InstructionSet
import RISCV.InstructionSet.Known
import RISCV.Semantics
import RISCV.Types

isetFromString :: String -> Maybe (Some InstructionSet)
isetFromString s = case s of
  "RV32I" -> Just $ Some (knownISet :: InstructionSet RV32I)
  "RV32IM" -> Just $ Some (knownISet :: InstructionSet RV32IM)
  "RV32IMA" -> Just $ Some (knownISet :: InstructionSet RV32IMA)
  "RV32IMAF" -> Just $ Some (knownISet :: InstructionSet RV32IMAF)
  "RV32IMAFD" -> Just $ Some (knownISet :: InstructionSet RV32IMAFD)
  "RV64I" -> Just $ Some (knownISet :: InstructionSet RV64I)
  "RV64IM" -> Just $ Some (knownISet :: InstructionSet RV64IM)
  "RV64IMA" -> Just $ Some (knownISet :: InstructionSet RV64IMA)
  "RV64IMAF" -> Just $ Some (knownISet :: InstructionSet RV64IMAF)
  "RV64IMAFD" -> Just $ Some (knownISet :: InstructionSet RV64IMAFD)
  _ -> Nothing

main :: IO ()
main = do
  args <- getArgs
  let someISet = case args of
        [s] | Just iset' <- isetFromString s -> iset'
        _ -> Some (knownISet :: InstructionSet RV32I)
  case someISet of
    Some iset -> do
      forM_ (sortBy pairSort $ toList (isSemanticsMap iset)) $ \(Pair opcode semantics) -> do
        putStrLn $ show opcode ++ ": "
        print (nest 4 $ pPrint semantics)
  where pairSort :: Pair (Opcode rv) (InstSemantics rv) -> Pair (Opcode rv) (InstSemantics rv) -> Ordering
        pairSort p1@(Pair oc1 _) p2@(Pair oc2 _) = compare (show oc1) (show oc2)
