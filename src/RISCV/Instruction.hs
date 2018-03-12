{-# LANGUAGE BinaryLiterals     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module RISCV.Instruction
  ( -- * Instructions
    Instruction(..)
  , Format(..)
  , FormatRepr(..)
    -- * Opcodes
  , Opcode(..)
  , OpBits(..)
    -- * Operands
  , Operands(..)
    -- * Opcode / OpBits conversion
  , opcodeFromOpBits
  , opBitsFromOpcode
  ) where

import Data.BitVector.Sized
import Data.Parameterized
import qualified Data.Parameterized.Map as Map
import Data.Parameterized.Map (MapF)
import Data.Parameterized.TH.GADT

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

-- | A type-level representative of the format. Particularly useful when decoding
-- instructions since we won't know ahead of time what format to classify them as.
data FormatRepr (k :: Format) where
  RRepr :: FormatRepr 'R
  IRepr :: FormatRepr 'I
  SRepr :: FormatRepr 'S
  BRepr :: FormatRepr 'B
  URepr :: FormatRepr 'U
  JRepr :: FormatRepr 'J
  ERepr :: FormatRepr 'E
  XRepr :: FormatRepr 'X

----------------------------------------
-- Operands

-- TODO: Consider making the operand arguments lists of bit vector types???
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

----------------------------------------
-- Instructions

-- | RV32I Instruction, parameterized by format.
data Instruction (k :: Format) = Inst { instOpcode   :: Opcode k
                                      , instOperands :: Operands k
                                      }

swap :: Pair (k :: Format -> *) (v :: Format -> *) -> Pair v k
swap (Pair k v) = Pair v k

transMap :: OrdF v => MapF (k :: Format -> *) (v :: Format -> *) -> MapF v k
transMap = Map.fromList . map swap . Map.toList

-- | Get the OpBits of an Opcode (the bits that are fixed by that opcode in all
-- instances)
opBitsFromOpcode :: Opcode k -> OpBits k
opBitsFromOpcode opcode = case Map.lookup opcode opcodeOpBitsMap of
  Just opBits -> opBits
  Nothing     -> error $ "Opcode " ++ show opcode ++
                 "does not have corresponding OpBits defined."

-- TODO: fix this; if we get Nothing we need to return an illegal instruction? Either
-- (Opcode 'X) Opcode k?
-- | Get the Opcode of an OpBits. Throws an error if given an invalid OpBits.
opcodeFromOpBits :: OpBits k -> Either (Opcode 'X) (Opcode k)
opcodeFromOpBits opBits = maybe (Left Illegal) Right $ Map.lookup opBits opBitsOpcodeMap

opcodeOpBitsMap :: MapF Opcode OpBits
opcodeOpBitsMap = Map.fromList $

  [ Pair Add  (ROpBits 0b0110011 0b000 0b0000000)
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
  , Pair Illegal (XOpBits)
  ]

opBitsOpcodeMap :: MapF OpBits Opcode
opBitsOpcodeMap = transMap opcodeOpBitsMap

----------------------------------------
-- Instances

-- Force types to be in context for TH stuff below.
$(return [])

instance Show (FormatRepr k) where
  show RRepr = "RRepr"
  show IRepr = "IRepr"
  show SRepr = "SRepr"
  show BRepr = "BRepr"
  show URepr = "URepr"
  show JRepr = "JRepr"
  show ERepr = "ERepr"
  show XRepr = "XRepr"

instance ShowF FormatRepr

-- TODO: KnownRepr instance for FormatRepr?


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

deriving instance Show (Opcode k)

instance ShowF Opcode

deriving instance Eq (Opcode k)

instance EqF Opcode where
  x `eqF` y = x == y

instance TestEquality Opcode where
  testEquality = $(structuralTypeEquality [t|Opcode|] [])

instance OrdF Opcode where
  compareF = $(structuralTypeOrd [t|Opcode|] [])

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

instance TestEquality OpBits where
  testEquality = $(structuralTypeEquality [t|OpBits|] [])

instance OrdF OpBits where
  compareF = $(structuralTypeOrd [t|OpBits|] [])

instance Show (Instruction k) where
  show (Inst opcode operands) = show opcode ++ " " ++ show operands

instance ShowF Instruction
