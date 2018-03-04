{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

{-|
Module      : RISCV.Instruction
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Defines data types for instructions, opcodes, and the like. Currently only implements
RV32I.
-}

module RISCV.Instruction
  ( -- * Opcodes
    Opcode(..)
    -- * Operands
  , Operands(..)
  , Format(..)
  , RegId(..), Imm12(..), Imm20(..)
    -- * Instructions
  , Instruction(..)
  ) where

-- import Data.Parameterized.NatRepr
-- import Data.Parameterized.Some
-- import GHC.TypeLits
import           Numeric (showHex)

-- | Register identifier
newtype RegId = RegId Integer
  deriving (Show, Eq, Ord)
-- | 12-bit immediate operand
newtype Imm12 = Imm12 Integer
  deriving (Show, Eq, Ord)
-- | 20-bit immediate operand
newtype Imm20 = Imm20 Integer
  deriving (Show, Eq, Ord)

-- | RV32I Instruction formats
data Format = R | I | S | B | U | J

-- | RV32I Operand lists, parameterized by format. There is exactly one constructor
-- per format.
data Operands :: Format -> * where
  ROperands :: RegId -> RegId -> RegId -> Operands 'R
  IOperands :: RegId -> RegId -> Imm12 -> Operands 'I
  SOperands :: RegId -> RegId -> Imm12 -> Operands 'S
  BOperands :: RegId -> RegId -> Imm12 -> Operands 'B
  UOperands :: RegId -> Imm20          -> Operands 'U
  JOperands :: RegId -> Imm20          -> Operands 'J

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

-- | RV32I Instruction, parameterized by format.
data Instruction (k :: Format) = Inst { instOpcode   :: Opcode k
                                      , instOperands :: Operands k
                                      }

-- | Print an integral value in hex with a leading "0x"
prettyHex :: (Show a, Integral a) => a -> String
prettyHex x = "0x" ++ showHex x ""
