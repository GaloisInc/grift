{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : RISCV.Instruction
Description : Defines data types for instructions, opcodes, and the like.
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Defines data types for instructions, opcodes, and the like. Currently only implements
RV32I.
-}
module RISCV.Instruction where

newtype RegId = RegId Integer
newtype Imm12 = Imm12 Integer
newtype Imm20 = Imm20 Integer

data Format = R | I | S | B | U | J

data Operands :: Format -> * where
  ROperands :: RegId -> RegId -> RegId -> Operands R
  IOperands :: RegId -> RegId -> Imm12 -> Operands I
  SOperands :: RegId -> RegId -> Imm12 -> Operands S
  BOperands :: RegId -> RegId -> Imm12 -> Operands B
  UOperands :: RegId -> Imm20 ->          Operands U
  JOperands :: RegId -> Imm20 ->          Operands J

data Opcode :: Format -> * where

  -- R type
  Add  :: Opcode R
  Sub  :: Opcode R
  Sll  :: Opcode R
  Slt  :: Opcode R
  Sltu :: Opcode R
  Xor  :: Opcode R
  Srl  :: Opcode R
  Sra  :: Opcode R
  Or   :: Opcode R
  And  :: Opcode R

  -- I type
  Jalr    :: Opcode I
  Lb      :: Opcode I
  Lh      :: Opcode I
  Lw      :: Opcode I
  Lbu     :: Opcode I
  Lhu     :: Opcode I
  Addi    :: Opcode I
  Slti    :: Opcode I
  Sltiu   :: Opcode I
  Xori    :: Opcode I
  Ori     :: Opcode I
  Andi    :: Opcode I
  Slli    :: Opcode I
  Srli    :: Opcode I
  Srai    :: Opcode I
  Fence   :: Opcode I
  Fence_i :: Opcode I
  Ecall   :: Opcode I
  Ebreak  :: Opcode I
  Csrrw   :: Opcode I
  Csrrs   :: Opcode I
  Csrrc   :: Opcode I
  Csrrwi  :: Opcode I
  Csrrsi  :: Opcode I
  Csrrci  :: Opcode I

  -- S type
  Sb :: Opcode S
  Sh :: Opcode S
  Sw :: Opcode S

  -- B type
  Beq  :: Opcode B
  Bne  :: Opcode B
  Blt  :: Opcode B
  Bge  :: Opcode B
  Bltu :: Opcode B
  Bgeu :: Opcode B

  -- U type
  Lui   :: Opcode U
  Addui :: Opcode U

  -- J type
  Jal :: Opcode J

data Instruction (k :: Format) = Instruction { opcode   :: Opcode k,
                                               operands :: Operands k
                                             }
