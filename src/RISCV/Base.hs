{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}

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
  , semAdd
  )
  where

import qualified Data.Parameterized.Map as Map
import Data.Parameterized

import RISCV.Instruction
import RISCV.Semantics

-- | RV32I Base ISA
base :: InstructionSet
base = instructionSet Width32 $ Map.fromList
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

-- TODO: There is some redundancy here with the InstructionSet type; we are
-- representing whether the instruction RV32/RV64 twice. Might make sense to remove
-- it from the InstructionSet type altogether, or to force every instruction to have
-- the same arch type as the InstructionSet. Think on it.

-- TODO: tie the parameters declared within a Semantics to the encoding/decoding
-- defined in Instruction.Layout
semAdd :: Semantics 'RV32 ()
semAdd = do
  comment "Adds register x[rs2] to register x[rs1] and writes the result to x[rd]."
  comment "Arithmetic overflow is ignored."

  rd  <- param "rd"
  rs1 <- param "rs1"
  rs2 <- param "rs2"

  x_rs1 <- regRead rs1
  x_rs2 <- regRead rs2

  result <- add x_rs1 x_rs2
  assignReg rd result
