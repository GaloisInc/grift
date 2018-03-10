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
  , OpBits(..)
    -- * Operands
  , Operands(..)
  , Format(..)
    -- * Instructions
  , Instruction(..)
  , OperandLayout(..)
  , instOperandLayout
  ) where

import Control.Lens
import Data.BitVector.Sized
import Data.BitVector.Sized.BitLayout
import Data.Parameterized

----------------------------------------
-- Formats

-- | The seven RV32I instruction formats. Each RV32I opcode has one of seven encoding
-- formats, corresponding to its operands and the way those operands are laid out as
-- bits in the instruction word. We include an additional (eighth) format, X,
-- inhabited only by an illegal instruction.
--
-- NOTE: Although the RISC-V spec only lists 6, in our formulation, the ecall and
-- ebreak instructions are slightly distinct from the other I-format instructions; in
-- particular, they don't have any operands, and they each actually use the same
-- opcode AND funct3 bits, and therefore have to have one extra bit to distinguish
-- between the two of them. Therefore, we invented an extra format, E, to represent
-- them.

data Format = R | I | S | B | U | J | E | X

----------------------------------------
-- Operands

-- TODO: Consider encapsulating BitVector w with another type, Operand w, also
-- parameterized over width. This would contain the bitvector as well as an Int index
-- into the instruction word where this operand goes in the word. It should be
-- possible to use this information cleverly in both the encoding AND the decoding.
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
  XOperands :: BitVector 32                                -> Operands 'X

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
  show (XOperands ill) = "[ ill = " ++ show ill ++ "]"
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
  -- TODO: the shift instructions are also a slightly different format, we accept
  -- that for the time being.
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

  -- X type (illegal instruction)
  Illegal :: Opcode 'X

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

  -- E type
  show Ecall   = "Ecall"
  show Ebreak  = "Ebreak"

  -- X type (illegal instruction)
  show Illegal = "Illegal"

instance ShowF Opcode

----------------------------------------
-- OpBits

-- | Bits fixed by an opcode.
-- Holds all the bits that are fixed by a particular opcode. Each format maps to a
-- potentially different set of bits.
data OpBits :: Format -> * where
  ROpBits :: BitVector 7 -> BitVector 3 -> BitVector 7 -> OpBits 'R
  IOpBits :: BitVector 7 -> BitVector 3                -> OpBits 'I
  SOpBits :: BitVector 7 -> BitVector 3                -> OpBits 'S
  BOpBits :: BitVector 7 -> BitVector 3                -> OpBits 'B
  UOpBits :: BitVector 7                               -> OpBits 'U
  JOpBits :: BitVector 7                               -> OpBits 'J
  EOpBits :: BitVector 7 -> BitVector 25               -> OpBits 'E
  XOpBits ::                                              OpBits 'X

instance Show (OpBits k) where
  show (ROpBits opcode funct3 funct7) =
    "[ opcode = " ++ show opcode ++
    ", funct3 = " ++ show funct3 ++
    ", funct7 = " ++ show funct7 ++ "]"
  show (IOpBits opcode funct3) =
    "[ opcode = " ++ show opcode ++
    ", funct3 = " ++ show funct3 ++ "]"
  show (SOpBits opcode funct3) =
    "[ opcode = " ++ show opcode ++
    ", funct3 = " ++ show funct3 ++ "]"
  show (BOpBits opcode funct3) =
    "[ opcode = " ++ show opcode ++
    ", funct3 = " ++ show funct3 ++ "]"
  show (UOpBits opcode) =
    "[ opcode = " ++ show opcode ++ "]"
  show (JOpBits opcode) =
    "[ opcode = " ++ show opcode ++ "]"
  show (EOpBits opcode b) =
    "[ opcode = " ++ show opcode ++
    ", b = " ++ show b ++ "]"
  show (XOpBits) = "[]"

instance ShowF OpBits

----------------------------------------
-- Instructions

-- | RV32I Instruction, parameterized by format.
data Instruction (k :: Format) = Inst { instOpcode   :: Opcode k
                                      , instOperands :: Operands k
                                      }

instance Show (Instruction k) where
  show (Inst opcode operands) = show opcode ++ " " ++ show operands

instance ShowF Instruction

----------------------------------------
-- Encode/Decode

-- experimental stuff here, trying to use BitLayouts for encode/decode.
-- Might be useful: a Map (BitVector 7) (Some Opcode), and perhaps a Map (Opcode k)
-- (BitVector 7).

data OperandLayout (k :: Format) where
  ROperandLayout :: BitLayout 32 5
                 -> BitLayout 32 5
                 -> BitLayout 32 5
                 -> OperandLayout 'R

data OpBitsLayout (k :: Format) where
  ROpBitsLayout :: BitLayout 32 7
                -> BitLayout 32 3
                -> BitLayout 32 7
                -> OpBitsLayout 'R

data FormatRepr (k :: Format) where
  RRepr :: FormatRepr 'R
  IRepr :: FormatRepr 'I
  SRepr :: FormatRepr 'S
  BRepr :: FormatRepr 'B
  URepr :: FormatRepr 'U
  JRepr :: FormatRepr 'J
  ERepr :: FormatRepr 'E
  XRepr :: FormatRepr 'X

opcodeLens :: Simple Lens (BitVector 32) (BitVector 7)
opcodeLens = layoutLens (chunk 0 <: empty)

funct3 :: Simple Lens (BitVector 32) (BitVector 3)
funct3 = layoutLens (chunk 12 <: empty)

funct7 :: Simple Lens (BitVector 32) (BitVector 7)
funct7 = layoutLens (chunk 25 <: empty)

rd :: Simple Lens (BitVector 32) (BitVector 5)
rd = layoutLens (chunk 7 <: empty)

rs1 :: Simple Lens (BitVector 32) (BitVector 5)
rs1 = layoutLens (chunk 15 <: empty)

rs2 :: Simple Lens (BitVector 32) (BitVector 5)
rs2 = layoutLens (chunk 20 <: empty)

imm12I :: Simple Lens (BitVector 32) (BitVector 12)
imm12I = layoutLens (chunk 20 <: empty)

imm12S :: Simple Lens (BitVector 32) (BitVector 12)
imm12S = layoutLens $ (chunk 25 :: Chunk 7) <: (chunk 7  :: Chunk 5) <: empty

imm12BLens :: Simple Lens (BitVector 32) (BitVector 12)
imm12BLens = layoutLens $
  (chunk 31 :: Chunk 1) <: (chunk 7  :: Chunk 1) <:
  (chunk 25 :: Chunk 6) <: (chunk 8  :: Chunk 4) <:
  empty

imm20ULens :: Simple Lens (BitVector 32) (BitVector 20)
imm20ULens = layoutLens $ chunk 12 <: empty

imm20JLens :: Simple Lens (BitVector 32) (BitVector 20)
imm20JLens = layoutLens $
  (chunk 31 :: Chunk 1)  <: (chunk 12 :: Chunk 8)  <:
  (chunk 20 :: Chunk 1)  <: (chunk 21 :: Chunk 10) <:
  empty

illegalLens :: Simple Lens (BitVector 32) (BitVector 32)
illegalLens = layoutLens $ chunk 0 <: empty

decodeOpcode :: BitVector 7 -> Some Opcode
decodeOpcode = undefined

decodeFormat :: Some Opcode -> Some FormatRepr
decodeFormat = undefined

formatOperandLayout :: FormatRepr k -> OperandLayout k
formatOperandLayout = undefined

formatOpBitsLayout :: FormatRepr k -> OperandLayout k
formatOpBitsLayout = undefined

opcodeFormat :: Opcode k -> FormatRepr k
opcodeFormat = undefined

instOperandLayout :: Instruction k -> OperandLayout k
instOperandLayout (Inst _ (ROperands {})) = undefined
instOperandLayout (Inst _ (IOperands {})) = undefined
instOperandLayout (Inst _ (SOperands {})) = undefined
instOperandLayout (Inst _ (BOperands {})) = undefined
instOperandLayout (Inst _ (UOperands {})) = undefined
instOperandLayout (Inst _ (JOperands {})) = undefined
instOperandLayout (Inst _ (EOperands {})) = undefined
instOperandLayout (Inst _ (XOperands {})) = undefined
