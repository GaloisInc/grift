{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.BitVector.Sized
import Data.Parameterized
import Data.Parameterized.List
import Data.Foldable

import RISCV.Types

inst :: KnownRepr FormatRepr fmt
     => Opcode rv fmt
     -> List BitVector (OperandTypes fmt)
     -> Some (Instruction rv)
inst opcode operands = Some $ Inst opcode (Operands knownRepr operands)

liProg :: BitVector 5  -- rd
       -> BitVector 32 -- x[rd]
       -> [Some (Instruction RV32I)]
liProg reg xreg =
  [ inst Lui  (reg :< bvExtract 12 xreg :< Nil)
  , inst Addi (reg :< 0 :< bvExtract 0 xreg :< Nil)
  ]

main :: IO ()
main = traverse_ print (liProg 5 0x64)
