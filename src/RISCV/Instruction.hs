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

import Data.Parameterized

import RISCV.BitVector

-- -- | 5-bit immediate operand (usually a register identifier)
-- newtype RegId = RegId Integer
--   deriving (Show, Eq, Ord)
-- -- | 12-bit immediate operand
-- newtype Imm12 = Imm12 Integer
--   deriving (Show, Eq, Ord)
-- -- | 20-bit immediate operand
-- newtype BitVector 20 = BitVector 20 Integer
--   deriving (Show, Eq, Ord)

-- | RV32I Instruction formats
data Format = R | I | S | B | U | J

-- | RV32I Operand lists, parameterized by format. There is exactly one constructor
-- per format.
data Operands :: Format -> * where
  ROperands :: BitVector 5 -> BitVector 5  -> BitVector 5  -> Operands 'R
  IOperands :: BitVector 5 -> BitVector 5  -> BitVector 12 -> Operands 'I
  SOperands :: BitVector 5 -> BitVector 5  -> BitVector 12 -> Operands 'S
  BOperands :: BitVector 5 -> BitVector 5  -> BitVector 12 -> Operands 'B
  UOperands :: BitVector 5 -> BitVector 20                 -> Operands 'U
  JOperands :: BitVector 5 -> BitVector 20                 -> Operands 'J

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

instance ShowF Operands

-- FIXME: Instead of using a Nat to encode the entire Word32 representing the opcode,
-- we could instead check out KnownRepr; that might actually be able to use a data
-- structure with the opcode, funct3, and funct7 fields in it separately.

-- | RV32I Opcode, parameterized by format.
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
  Fence   :: Opcode 'I
  Fence_i :: Opcode 'I
  Ecall   :: Opcode 'I
  Ebreak  :: Opcode 'I
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
  Addui :: Opcode 'U

  -- J type
  Jal :: Opcode 'J


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
  show Ecall   = "Ecall"
  show Ebreak  = "Ebreak"
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
  show Addui = "Addui"

  -- J type
  show Jal = "Jal"

instance ShowF Opcode

-- | RV32I Instruction, parameterized by format.
data Instruction (k :: Format) = Inst { instOpcode   :: Opcode k
                                      , instOperands :: Operands k
                                      }

instance Show (Instruction k) where
  show (Inst opcode operands) = show opcode ++ " " ++ show operands

instance ShowF Instruction
