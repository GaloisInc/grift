{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}

{-|
Module      : RISCV.Instruction
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Defines internal representations for instructions, opcodes, and the like. Currently
only implements RV32I.
-}

module RISCV.Instruction
  ( -- * Opcodes
    Opcode(..)
    -- * Operands
  , Operands(..)
  , Format(..)
    -- * Instructions
  , Instruction(..)
  ) where

import           Data.Parameterized

import RISCV.BitVector

----------------------------------------
-- Formats

-- | The seven RV32I instruction formats. Each RV32I opcode has one of seven encoding
-- formats, corresponding to its operands and the way those operands are laid out as
-- bits in the instruction word.
--
-- NOTE: Although the RISC-V spec only lists 6, in our formulation, the ecall and
-- ebreak instructions are slightly distinct from the other I-format instructions; in
-- particular, they don't have any operands, and they each actually use the same
-- opcode AND funct3 bits, and therefore have to have one extra bit to distinguish
-- between the two of them. Therefore, we invented an extra format, E, to represent
-- them.

data Format = R | I | S | B | U | J | E

----------------------------------------
-- Operands

-- | RV32I Operand lists, parameterized by format. There is exactly one constructor
-- per format.
data Operands :: Format -> * where
  ROperands :: BitVector 5 -> BitVector 5  -> BitVector 5  -> Operands 'R
  IOperands :: BitVector 5 -> BitVector 5  -> BitVector 12 -> Operands 'I
  SOperands :: BitVector 5 -> BitVector 5  -> BitVector 12 -> Operands 'S
  BOperands :: BitVector 5 -> BitVector 5  -> BitVector 12 -> Operands 'B
  UOperands :: BitVector 5 -> BitVector 20                 -> Operands 'U
  JOperands :: BitVector 5 -> BitVector 20                 -> Operands 'J
  EOperands ::                                                Operands 'E

instance Show (Operands k) where
  show (ROperands rd rs1 rs2) =
    "[ rd = "  ++ show rd ++
    ", rs1 = " ++ show rs1 ++
    ", rs2 = " ++ show rs2 ++ " ]"
  show (IOperands rd rs1 imm) =
    "[ rd = "  ++ show rd ++
    ", rs1 = " ++ show rs1 ++
    ", imm = " ++ show imm ++ " ]"
  show (SOperands rs1 rs2 imm) =
    "[ rs1 = " ++ show rs1 ++
    ", rs2 = " ++ show rs2 ++
    ", imm = " ++ show imm ++ " ]"
  show (BOperands rs1 rs2 imm) =
    "[ rs1 = " ++ show rs1 ++
    ", rs2 = " ++ show rs2 ++
    ", imm = " ++ show imm ++ " ]"
  show (UOperands rd imm) =
    "[ rd = "  ++ show rd ++
    ", imm = " ++ show imm ++ " ]"
  show (JOperands rd imm) =
    "[ rd = "  ++ show rd ++
    ", imm = " ++ show imm ++ " ]"
  show (EOperands) = "[]"
instance ShowF Operands


----------------------------------------
-- Opcodes

-- | RV32I Opcodes, parameterized by format.
data Opcode (f :: Format) :: * where

  -- R type
  Add  :: Opcode 'R
  Sub  :: Opcode 'R
  Sll  :: Opcode 'R
  Slt  :: Opcode 'R
  Sltu :: Opcode 'R
  Xor  :: Opcode 'R
  Srl  :: Opcode 'R
  Sra  :: Opcode 'R
  Or   :: Opcode 'R
  And  :: Opcode 'R

  -- I type
  Jalr    :: Opcode 'I
  Lb      :: Opcode 'I
  Lh      :: Opcode 'I
  Lw      :: Opcode 'I
  Lbu     :: Opcode 'I
  Lhu     :: Opcode 'I
  Addi    :: Opcode 'I
  Slti    :: Opcode 'I
  Sltiu   :: Opcode 'I
  Xori    :: Opcode 'I
  Ori     :: Opcode 'I
  Andi    :: Opcode 'I
  Slli    :: Opcode 'I
  Srli    :: Opcode 'I
  Srai    :: Opcode 'I
  -- TODO: Fence and Fence_i are both slightly wonky; we might need to separate them
  -- out into separate formats like we did with Ecall and Ebreak. Fence uses the
  -- immediate bits to encode additional operands and Fence_i requires them to be 0,
  -- so ideally we'd capture that in the type.
  Fence   :: Opcode 'I
  Fence_i :: Opcode 'I
  Csrrw   :: Opcode 'I
  Csrrs   :: Opcode 'I
  Csrrc   :: Opcode 'I
  Csrrwi  :: Opcode 'I
  Csrrsi  :: Opcode 'I
  Csrrci  :: Opcode 'I

  -- S type
  Sb :: Opcode 'S
  Sh :: Opcode 'S
  Sw :: Opcode 'S

  -- B type
  Beq  :: Opcode 'B
  Bne  :: Opcode 'B
  Blt  :: Opcode 'B
  Bge  :: Opcode 'B
  Bltu :: Opcode 'B
  Bgeu :: Opcode 'B

  -- U type
  Lui   :: Opcode 'U
  Auipc :: Opcode 'U

  -- J type
  Jal :: Opcode 'J

  -- E type
  Ecall   :: Opcode 'E
  Ebreak  :: Opcode 'E

instance Show (Opcode k) where
  -- R type
  show Add  = "Add"
  show Sub  = "Sub"
  show Sll  = "Sll"
  show Slt  = "Slt"
  show Sltu = "Sltu"
  show Xor  = "Xor"
  show Srl  = "Srl"
  show Sra  = "Sra"
  show Or   = "Or"
  show And  = "And"

  -- I type
  show Jalr    = "Jalr"
  show Lb      = "Lb"
  show Lh      = "Lh"
  show Lw      = "Lw"
  show Lbu     = "Lbu"
  show Lhu     = "Lhu"
  show Addi    = "Addi"
  show Slti    = "Slti"
  show Sltiu   = "Sltiu"
  show Xori    = "Xori"
  show Ori     = "Ori"
  show Andi    = "Andi"
  show Slli    = "Slli"
  show Srli    = "Srli"
  show Srai    = "Srai"
  show Fence   = "Fence"
  show Fence_i = "Fence.i"
  show Csrrw   = "Csrrw"
  show Csrrs   = "Csrrs"
  show Csrrc   = "Csrrc"
  show Csrrwi  = "Csrrwi"
  show Csrrsi  = "Csrrsi"
  show Csrrci  = "Csrrci"

  -- S type
  show Sb = "Sb"
  show Sh = "Sh"
  show Sw = "Sw"

  -- B type
  show Beq  = "Beq"
  show Bne  = "Bne"
  show Blt  = "Blt"
  show Bge  = "Bge"
  show Bltu = "Bltu"
  show Bgeu = "Bgeu"

  -- U type
  show Lui   = "Lui"
  show Auipc = "Auipc"

  -- J type
  show Jal = "Jal"

  -- E typ
  show Ecall   = "Ecall"
  show Ebreak  = "Ebreak"

instance ShowF Opcode

----------------------------------------
-- Instructions

-- | RV32I Instruction, parameterized by format.
data Instruction (k :: Format) = Inst { instOpcode   :: Opcode k
                                      , instOperands :: Operands k
                                      }

instance Show (Instruction k) where
  show (Inst opcode operands) = show opcode ++ " " ++ show operands

instance ShowF Instruction
