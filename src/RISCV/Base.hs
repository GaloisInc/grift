{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : RISCV.Base
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

RV32I base ISA, encoding and semantics.
-}

module RISCV.Base
  ( base
  )
  where

import qualified Data.Parameterized.Map as Map
import Data.Parameterized

import RISCV.Instruction
import RISCV.Semantics
import RISCV.Format

-- | RV32I Base ISA
base :: InstructionSet 'RV32
base = instructionSet baseEncode baseSemantics

baseEncode :: EncodeMap
baseEncode = Map.fromList
  [ -- RV32I
    -- R type
    Pair Add  (ROpBits 0b0110011 0b000 0b0000000)
  , Pair Sub  (ROpBits 0b0110011 0b000 0b0100000)
  , Pair Sll  (ROpBits 0b0110011 0b001 0b0000000)
  , Pair Slt  (ROpBits 0b0110011 0b010 0b0000000)
  , Pair Sltu (ROpBits 0b0110011 0b011 0b0000000)
  , Pair Xor  (ROpBits 0b0110011 0b100 0b0000000)
  , Pair Srl  (ROpBits 0b0110011 0b101 0b0000000)
  , Pair Sra  (ROpBits 0b0110011 0b101 0b0100000)
  , Pair Or   (ROpBits 0b0110011 0b110 0b0000000)
  , Pair And  (ROpBits 0b0110011 0b111 0b0000000)

  -- I type
  , Pair Jalr   (IOpBits 0b1100111 0b000)
  , Pair Lb     (IOpBits 0b0000011 0b000)
  , Pair Lh     (IOpBits 0b0000011 0b001)
  , Pair Lw     (IOpBits 0b0000011 0b010)
  , Pair Lbu    (IOpBits 0b0000011 0b100)
  , Pair Lhu    (IOpBits 0b0000011 0b101)
  , Pair Addi   (IOpBits 0b0010011 0b000)
  , Pair Slti   (IOpBits 0b0010011 0b010)
  , Pair Sltiu  (IOpBits 0b0010011 0b011)
  , Pair Xori   (IOpBits 0b0010011 0b100)
  , Pair Ori    (IOpBits 0b0010011 0b110)
  , Pair Andi   (IOpBits 0b0010011 0b111)
  , Pair Slli   (IOpBits 0b0010011 0b001)
  , Pair Srli   (IOpBits 0b0010011 0b101)
  , Pair Srai   (IOpBits 0b0010011 0b101)
  , Pair Fence  (IOpBits 0b0001111 0b000)
  , Pair Fence  (IOpBits 0b0001111 0b001)
  , Pair Csrrw  (IOpBits 0b1110011 0b001)
  , Pair Csrrs  (IOpBits 0b1110011 0b010)
  , Pair Csrrc  (IOpBits 0b1110011 0b011)
  , Pair Csrrwi (IOpBits 0b1110011 0b101)
  , Pair Csrrsi (IOpBits 0b1110011 0b110)
  , Pair Csrrci (IOpBits 0b1110011 0b111)

  -- S type
  , Pair Sb (SOpBits 0b0100011 0b000)
  , Pair Sh (SOpBits 0b0100011 0b001)
  , Pair Sw (SOpBits 0b0100011 0b010)

  -- B type
  , Pair Beq  (BOpBits 0b1100011 0b000)
  , Pair Bne  (BOpBits 0b1100011 0b001)
  , Pair Blt  (BOpBits 0b1100011 0b100)
  , Pair Bge  (BOpBits 0b1100011 0b101)
  , Pair Bltu (BOpBits 0b1100011 0b110)
  , Pair Bgeu (BOpBits 0b1100011 0b111)

  -- U type
  , Pair Lui   (UOpBits 0b0110111)
  , Pair Auipc (UOpBits 0b0010111)

  -- J type
  , Pair Jal (JOpBits 0b1101111)

  -- E type
  , Pair Ecall  (EOpBits 0b1110011 0b0000000000000000000000000)
  , Pair Ebreak (EOpBits 0b1110011 0b0000000000010000000000000)

  -- X type
  , Pair Illegal XOpBits
  ]

baseSemantics :: SemanticsMap 'RV32
baseSemantics = Map.fromList
  [ Pair Add $ getFormula $ do
      comment "Adds register x[rs2] to register x[rs1] and writes the result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp addE
  , Pair Sub $ getFormula $ do
      comment "Subtracts register x[rs2] from register x[rs1] and writes the result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp subE
  , Pair Sll $ getFormula $ do
      comment "Shifts register x[rs1] left by x[rs2] bit positions."
      comment "The vacated bits are filled with zeros, and the result is writtein to x[rd]."

      rOp sllE
  , Pair Slt $ getFormula $ do
      comment "Compares x[rs1] and x[rs2] as two's complement numbers."
      comment "Writes 1 to x[rd] if x[rs1] is smaller, or 0 if not."

      rOp (\e1 e2 -> ltsE e1 e2 >>= zextE)
  , Pair Sltu $ getFormula $ do
      comment "Compares x[rs1] and x[rs2] as unsigned numbers."
      comment "Writes 1 to x[rd] if x[rs1] is smaller, or 0 if not."

      rOp (\e1 e2 -> ltuE e1 e2 >>= zextE)
  , Pair Xor $ getFormula $ do
      comment "Computes the bitwise exclusive-OR of registers x[rs1] and x[rs2]."
      comment "Writes the result to x[rd]."

      rOp xorE
  , Pair Srl $ getFormula $ do
      comment "Shifts register x[rs1] right by x[rs2] bit positions."
      comment "The vacated bits are filled with zeros, and the result is written to x[rd]."

      rOp srlE
  , Pair Sra $ getFormula $ do
      comment "Shifts register x[rs1] right by x[rs2] bit positions."
      comment "The vacated bits are filled with copies of x[rs1]'s most significant bits."
      comment "The result is written to x[rd]."

      rOp srlE
  , Pair Or $ getFormula $ do
      comment "Computes the bitwise inclusive-OR of registers x[rs1] and x[rs2]."
      comment "Writes the result to x[rd]."

      rOp orE
  , Pair And $ getFormula $ do
      comment "Computes the bitwise AND of registers x[rs1] and x[rs2]."
      comment "Writes the result to x[rd]."

      rOp andE

  -- -- I type
  , Pair Jalr $ getFormula $ do
      comment "Sets the pc to x[rs1] + sext(offset)."
      comment "Masks off the least significant bit of the computed address."
      comment "Writes the previous pc+4 to x[rd]."

      (rd, rs1, offset) <- params

      pc <- pcRead
      t  <- pc `addE` (litBV 4)

      x_rs1       <- regRead rs1
      sext_offset <- sextE offset
      new_pc'     <- x_rs1 `addE` sext_offset
      mask        <- complementE (litBV 1)
      new_pc      <- new_pc' `andE` mask

      assignPC new_pc
      assignReg rd t
  -- , Pair Lb     undefined
  -- , Pair Lh     undefined
  -- , Pair Lw     undefined
  -- , Pair Lbu    undefined
  -- , Pair Lhu    undefined
  -- , Pair Addi   undefined
  -- , Pair Slti   undefined
  -- , Pair Sltiu  undefined
  -- , Pair Xori   undefined
  -- , Pair Ori    undefined
  -- , Pair Andi   undefined
  -- , Pair Slli   undefined
  -- , Pair Srli   undefined
  -- , Pair Srai   undefined
  -- , Pair Fence  undefined
  -- , Pair Fence  undefined
  -- , Pair Csrrw  undefined
  -- , Pair Csrrs  undefined
  -- , Pair Csrrc  undefined
  -- , Pair Csrrwi undefined
  -- , Pair Csrrsi undefined
  -- , Pair Csrrci undefined

  -- -- S type
  -- , Pair Sb undefined
  -- , Pair Sh undefined
  -- , Pair Sw undefined

  -- -- B type
  -- , Pair Beq  undefined
  -- , Pair Bne  undefined
  -- , Pair Blt  undefined
  -- , Pair Bge  undefined
  -- , Pair Bltu undefined
  -- , Pair Bgeu undefined

  -- -- U type
  -- , Pair Lui   undefined
  -- , Pair Auipc undefined

  -- -- J type
  -- , Pair Jal undefined

  -- -- E type
  -- , Pair Ecall  undefined
  -- , Pair Ebreak undefined

  -- -- X type
  -- , Pair Illegal undefined
  ]

type ArithOp arch fmt = BVExpr arch (ArchWidth arch)
                     -> BVExpr arch (ArchWidth arch)
                     -> FormulaBuilder arch fmt (BVExpr arch (ArchWidth arch))

rOp :: ArithOp arch 'R -> FormulaBuilder arch 'R ()
rOp op = do
      (rd, rs1, rs2)  <- params

      x_rs1 <- regRead rs1
      x_rs2 <- regRead rs2

      result <- op x_rs1 x_rs2
      assignReg rd result
