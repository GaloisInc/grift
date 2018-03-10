{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
import Data.BitVector.Sized.Internal ( BitVector(BV) )
import Data.BitVector.Sized.BitLayout
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
-- Force types to be in context.
$(return [])

----------------------------------------
-- Encode/Decode

-- experimental stuff here, trying to use BitLayouts for encode/decode.
-- Might be useful: a Map (BitVector 7) (Some Opcode), and perhaps a Map (Opcode k)
-- (BitVector 7).

type RegIdLayout = BitLayout 32 5
type Imm12Layout = BitLayout 32 12
type Imm20Layout = BitLayout 32 20
type XLayout     = BitLayout 32 32

-- TODO: not sure if this is at all useful, but it is kinda interesting
type family Layout (r :: Format -> *) :: * where
  Layout (r l) = BitLayout 32 l

type OpcodeLayout = BitLayout 32 7
type Funct3Layout = BitLayout 32 3
type Funct7Layout = BitLayout 32 7
type ELayout      = BitLayout 32 25

data OpBitsLayout (k :: Format) where
  ROpBitsLayout :: OpcodeLayout -> Funct3Layout -> Funct7Layout -> OpBitsLayout 'R
  IOpBitsLayout :: OpcodeLayout -> Funct3Layout                 -> OpBitsLayout 'I
  SOpBitsLayout :: OpcodeLayout -> Funct3Layout                 -> OpBitsLayout 'S
  BOpBitsLayout :: OpcodeLayout -> Funct3Layout                 -> OpBitsLayout 'B
  UOpBitsLayout :: OpcodeLayout                                 -> OpBitsLayout 'U
  JOpBitsLayout :: OpcodeLayout                                 -> OpBitsLayout 'J
  EOpBitsLayout :: OpcodeLayout -> ELayout                      -> OpBitsLayout 'E

data FormatRepr (k :: Format) where
  RRepr :: FormatRepr 'R
  IRepr :: FormatRepr 'I
  SRepr :: FormatRepr 'S
  BRepr :: FormatRepr 'B
  URepr :: FormatRepr 'U
  JRepr :: FormatRepr 'J
  ERepr :: FormatRepr 'E
  XRepr :: FormatRepr 'X

-- TODO: KnownRepr instance

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

opcodeLayout :: BitLayout 32 7
opcodeLayout = (chunk 0 <: empty)

funct3Layout :: BitLayout 32 3
funct3Layout = (chunk 12 <: empty)

funct7Layout :: BitLayout 32 7
funct7Layout = (chunk 25 <: empty)

rdLayout :: BitLayout 32 5
rdLayout = (chunk 7 <: empty)

rs1Layout :: BitLayout 32 5
rs1Layout = (chunk 15 <: empty)

rs2Layout :: BitLayout 32 5
rs2Layout = (chunk 20 <: empty)

imm12ILayout :: BitLayout 32 12
imm12ILayout = (chunk 20 <: empty)

imm12SLayout :: BitLayout 32 12
imm12SLayout = (chunk 25 :: Chunk 7) <: (chunk 7  :: Chunk 5) <: empty

imm12BLayout :: BitLayout 32 12
imm12BLayout =
  (chunk 31 :: Chunk 1) <: (chunk 7  :: Chunk 1) <:
  (chunk 25 :: Chunk 6) <: (chunk 8  :: Chunk 4) <:
  empty

imm20ULayout :: BitLayout 32 20
imm20ULayout = chunk 12 <: empty

imm20JLayout :: BitLayout 32 20
imm20JLayout =
  (chunk 31 :: Chunk 1)  <: (chunk 12 :: Chunk 8)  <:
  (chunk 20 :: Chunk 1)  <: (chunk 21 :: Chunk 10) <:
  empty

illegalLayout :: BitLayout 32 32
illegalLayout = chunk 0 <: empty

-- lenses

opcodeLens :: Simple Lens (BitVector 32) (BitVector 7)
opcodeLens = layoutLens $ (chunk 0 <: empty)

funct3Lens :: Simple Lens (BitVector 32) (BitVector 3)
funct3Lens = layoutLens $ (chunk 12 <: empty)

funct7Lens :: Simple Lens (BitVector 32) (BitVector 7)
funct7Lens = layoutLens $ (chunk 25 <: empty)

rdLens :: Simple Lens (BitVector 32) (BitVector 5)
rdLens = layoutLens $ (chunk 7 <: empty)

rs1Lens :: Simple Lens (BitVector 32) (BitVector 5)
rs1Lens = layoutLens $ (chunk 15 <: empty)

rs2Lens :: Simple Lens (BitVector 32) (BitVector 5)
rs2Lens = layoutLens $ (chunk 20 <: empty)

imm12ILens :: Simple Lens (BitVector 32) (BitVector 12)
imm12ILens = layoutLens $ (chunk 20 <: empty)

imm12SLens :: Simple Lens (BitVector 32) (BitVector 12)
imm12SLens = layoutLens $ (chunk 25 :: Chunk 7) <: (chunk 7  :: Chunk 5) <: empty

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

data OperandLayout (k :: Format) where
  ROperandLayout :: RegIdLayout -> RegIdLayout -> RegIdLayout -> OperandLayout 'R
  IOperandLayout :: RegIdLayout -> RegIdLayout -> Imm12Layout -> OperandLayout 'I
  SOperandLayout :: RegIdLayout -> RegIdLayout -> Imm12Layout -> OperandLayout 'S
  BOperandLayout :: RegIdLayout -> RegIdLayout -> Imm12Layout -> OperandLayout 'B
  UOperandLayout :: RegIdLayout -> Imm20Layout                -> OperandLayout 'U
  JOperandLayout :: RegIdLayout -> Imm20Layout                -> OperandLayout 'J
  EOperandLayout ::                                              OperandLayout 'E
  XOperandLayout :: XLayout                                   -> OperandLayout 'X

formatOperandLayout :: FormatRepr k -> OperandLayout k
formatOperandLayout RRepr = ROperandLayout rdLayout  rs1Layout rs2Layout
formatOperandLayout IRepr = IOperandLayout rdLayout  rs1Layout imm12ILayout
formatOperandLayout SRepr = SOperandLayout rs1Layout rs2Layout imm12SLayout
formatOperandLayout BRepr = BOperandLayout rs1Layout rs2Layout imm12BLayout
formatOperandLayout URepr = UOperandLayout rdLayout  imm20ULayout
formatOperandLayout JRepr = JOperandLayout rdLayout  imm20JLayout
formatOperandLayout ERepr = EOperandLayout
formatOperandLayout XRepr = XOperandLayout illegalLayout
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


-- DECODING

-- | First, get the format
decodeFormat :: BitVector 32 -> Some FormatRepr
decodeFormat bvec = case bvExtract 0 bvec :: BitVector 7 of
  BV _ 0b0110011 -> Some RRepr

  BV _ 0b1100111 -> Some IRepr
  BV _ 0b0000011 -> Some IRepr
  BV _ 0b0010011 -> Some IRepr
  BV _ 0b0001111 -> Some IRepr
  BV _ 0b1110011 -> Some IRepr

  BV _ 0b0100011 -> Some SRepr

  BV _ 0b1100011 -> Some BRepr

  BV _ 0b0110111 -> Some URepr
  BV _ 0b0010111 -> Some URepr

  BV _ 0b1101111 -> Some JRepr

  BV _ 0b1110011 -> Some ERepr

  _ ->              Some XRepr

decodeOperands :: FormatRepr k -> BitVector 32 -> Operands k
decodeOperands repr bvec = case repr of
  RRepr -> ROperands (bvec ^. rdLens)  (bvec ^. rs1Lens) (bvec ^. rs2Lens)
  IRepr -> IOperands (bvec ^. rdLens)  (bvec ^. rs1Lens) (bvec ^. imm12ILens)
  SRepr -> SOperands (bvec ^. rs1Lens) (bvec ^. rs2Lens) (bvec ^. imm12SLens)
  BRepr -> BOperands (bvec ^. rs1Lens) (bvec ^. rs2Lens) (bvec ^. imm12BLens)
  URepr -> UOperands (bvec ^. rdLens)  (bvec ^. imm20ULens)
  JRepr -> JOperands (bvec ^. rdLens)  (bvec ^. imm20JLens)
  ERepr -> EOperands
  XRepr -> XOperands (bvec ^. illegalLens)
  _ -> undefined

swap :: Pair k v -> Pair v k
swap (Pair k v) = Pair v k

transMap :: OrdF v => MapF k v -> MapF v k
transMap = Map.fromList . map swap . Map.toList

opcodeOpBitsMap :: MapF Opcode OpBits
opcodeOpBitsMap = Map.fromList $
  [ Pair Add (ROpBits (bv 0b0110011) (bv 0b000) (bv 0b0000000))
  , Pair Sub (ROpBits (bv 0b0110011) (bv 0b000) (bv 0b0100000))
  , Pair Sll (ROpBits (bv 0b0110011) (bv 0b001) (bv 0b0000000))
  , Pair Slt (ROpBits (bv 0b0110011) (bv 0b010) (bv 0b0000000))
  , Pair Sltu (ROpBits (bv 0b0110011) (bv 0b011) (bv 0b0000000))
  , Pair Xor (ROpBits (bv 0b0110011) (bv 0b100) (bv 0b0000000))
  , Pair Srl (ROpBits (bv 0b0110011) (bv 0b101) (bv 0b0000000))
  , Pair Sra (ROpBits (bv 0b0110011) (bv 0b101) (bv 0b0100000))
  , Pair Or ( ROpBits (bv 0b0110011) (bv 0b110) (bv 0b0000000))
  , Pair And (ROpBits (bv 0b0110011) (bv 0b111) (bv 0b0000000))

-- I type
  , Pair Jalr (IOpBits (bv 0b1100111) (bv 0b000))
  , Pair Lb (IOpBits (bv 0b0000011) (bv 0b000))
  , Pair Lh (IOpBits (bv 0b0000011) (bv 0b001))
  , Pair Lw (IOpBits (bv 0b0000011) (bv 0b010))
  , Pair Lbu (IOpBits (bv 0b0000011) (bv 0b100))
  , Pair Lhu (IOpBits (bv 0b0000011) (bv 0b101))
  , Pair Addi (IOpBits (bv 0b0010011) (bv 0b000))
  , Pair Slti (IOpBits (bv 0b0010011) (bv 0b010))
  , Pair Sltiu (IOpBits (bv 0b0010011) (bv 0b011))
  , Pair Xori (IOpBits (bv 0b0010011) (bv 0b100))
  , Pair Ori (IOpBits (bv 0b0010011) (bv 0b110))
  , Pair Andi (IOpBits (bv 0b0010011) (bv 0b111))
  , Pair Slli (IOpBits (bv 0b0010011) (bv 0b001))
  , Pair Srli (IOpBits (bv 0b0010011) (bv 0b101))
  , Pair Srai (IOpBits (bv 0b0010011) (bv 0b101))
  , Pair Fence ( IOpBits (bv 0b0001111) (bv 0b000))
  , Pair Fence ( IOpBits (bv 0b0001111) (bv 0b001))
  , Pair Csrrw ( IOpBits (bv 0b1110011) (bv 0b001))
  , Pair Csrrs ( IOpBits (bv 0b1110011) (bv 0b010))
  , Pair Csrrc ( IOpBits (bv 0b1110011) (bv 0b011))
  , Pair Csrrwi (IOpBits (bv 0b1110011) (bv 0b101))
  , Pair Csrrsi (IOpBits (bv 0b1110011) (bv 0b110))
  , Pair Csrrci (IOpBits (bv 0b1110011) (bv 0b111))

-- S type
  , Pair Sb (SOpBits (bv 0b0100011) (bv 0b000))
  , Pair Sh (SOpBits (bv 0b0100011) (bv 0b001))
  , Pair Sw (SOpBits (bv 0b0100011) (bv 0b010))

-- B type
  , Pair Beq (BOpBits (bv 0b1100011) (bv 0b000))
  , Pair Bne (BOpBits (bv 0b1100011) (bv 0b001))
  , Pair Blt (BOpBits (bv 0b1100011) (bv 0b100))
  , Pair Bge (BOpBits (bv 0b1100011) (bv 0b101))
  , Pair Bltu (BOpBits (bv 0b1100011) (bv 0b110))
  , Pair Bgeu (BOpBits (bv 0b1100011) (bv 0b111))

-- U type
  , Pair Lui ( UOpBits (bv 0b0110111))
  , Pair Auipc (UOpBits (bv 0b0010111))

-- J type
  , Pair Jal (JOpBits (bv 0b1101111))

-- E type
  , Pair Ecall (EOpBits (bv 0b1110011) (bv 0b0000000000000000000000000))
  , Pair Ebreak (EOpBits (bv 0b1110011) (bv 0b0000000000010000000000000))

-- X type
  , Pair Illegal (XOpBits)
  ]

-- Instances


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


instance EqF Opcode where
  -- R type
  Add `eqF` Add = True
  Sub `eqF` Sub = True
  Slt `eqF` Slt = True
  Sltu `eqF` Sltu = True
  Xor `eqF` Xor = True
  Srl `eqF` Srl = True
  Sra `eqF` Sra = True
  Or `eqF` Or = True
  And `eqF` And = True

  -- I type
  Jalr `eqF` Jalr = True
  Lb `eqF` Lb = True
  Lh `eqF` Lh = True
  Lw `eqF` Lw = True
  Lbu `eqF` Lbu = True
  Lhu `eqF` Lhu = True
  Addi `eqF` Addi = True
  Slti `eqF` Slti = True
  Sltiu `eqF` Sltiu = True
  Xori `eqF` Xori = True
  Ori `eqF` Ori = True
  Andi `eqF` Andi = True
  -- TODO: the shift instructions are also a slightly different format, we accept
  -- that for the time being.
  Slli `eqF` Slli = True
  Srli `eqF` Srli = True
  Srai `eqF` Srai = True
  -- TODO: Fence and Fence_i are both slightly wonky; we might need to separate them
  -- out into separate formats like we did with Ecall and Ebreak. Fence uses the
  -- immediate bits to encode additional operands and Fence_i requires them to be 0,
  -- so ideally we'd capture that in the type.
  Fence `eqF` Fence = True
  Fence_i `eqF` Fence_i = True
  Csrrw `eqF` Csrrw = True
  Csrrs `eqF` Csrrs = True
  Csrrc `eqF` Csrrc = True
  Csrrwi `eqF` Csrrwi = True
  Csrrsi `eqF` Csrrsi = True
  Csrrci `eqF` Csrrci = True

  -- S type
  Sb `eqF` Sb = True
  Sh `eqF` Sh = True
  Sw `eqF` Sw = True

  -- B type
  Beq `eqF` Beq = True
  Bne `eqF` Bne = True
  Blt `eqF` Blt = True
  Bge `eqF` Bge = True
  Bltu `eqF` Bltu = True
  Bgeu `eqF` Bgeu = True

  -- U type
  Lui `eqF` Lui = True
  Auipc `eqF` Auipc = True

  -- J type
  Jal `eqF` Jal = True

  -- E type
  Ecall `eqF` Ecall = True
  Ebreak `eqF` Ebreak = True

  -- X type (illegal instruction)
  Illegal `eqF` Illegal = True

  _ `eqF` _ = False


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

instance Show (Instruction k) where
  show (Inst opcode operands) = show opcode ++ " " ++ show operands

instance ShowF Instruction
