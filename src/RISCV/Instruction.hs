{-# LANGUAGE BinaryLiterals         #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

{-|
Module      : RISCV.Instruction
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

AST for Instruction data type, parameterized by instruction format (R, I, S, ...).
-}

-- TODO: Consider getting rid of the InstWord type and just doing everything by
-- number of bits. It's confusing to have two kinds of NatRepr's floating around.

module RISCV.Instruction
  ( -- * Instructions
    Instruction(..)
    -- * Opcodes
  , Opcode(..)
  , OpBits(..)
  ) where

import Data.BitVector.Sized
import Data.Parameterized
import Data.Parameterized.TH.GADT

import RISCV.Types

-- Instances
-- deriving instance Eq (Operands k)
-- instance EqF Operands where
--   eqF = (==)
-- instance TestEquality Operands where
--   testEquality = $(structuralTypeEquality [t|Operands|] [])
-- instance OrdF Operands where
--   compareF = $(structuralTypeOrd [t|Operands|] [])

----------------------------------------
-- Opcodes

-- | RISC-V Opcodes, parameterized by base architecture and format.
--
-- We note here that the 'Srai' and 'Srli' instructions are combined into the same
-- instruction. The reason for this is that they are actually encoded as format I
-- rather than R, which means that the bit that distinguishes them is classified as
-- an operand. Therefore, for our representation, we use the same constructor and
-- push the distinction into the semantics. It is a shame that these instruction
-- weren't encoded differently; if they used different OpBits, this could have been
-- avoided. Similarly, 'Ecall' and 'Ebreak' are combined into a single instruction.
data Opcode :: BaseArch -> Format -> * where

  -- RV32I
  Add    :: Opcode arch R
  Sub    :: Opcode arch R
  Sll    :: Opcode arch R
  Slt    :: Opcode arch R
  Sltu   :: Opcode arch R
  Xor    :: Opcode arch R
  Srl    :: Opcode arch R
  Sra    :: Opcode arch R
  Or     :: Opcode arch R
  And    :: Opcode arch R

  Jalr    :: Opcode arch I
  Lb      :: Opcode arch I
  Lh      :: Opcode arch I
  Lw      :: Opcode arch I
  Lbu     :: Opcode arch I
  Lhu     :: Opcode arch I
  Addi    :: Opcode arch I
  Slti    :: Opcode arch I
  Sltiu   :: Opcode arch I
  Xori    :: Opcode arch I
  Ori     :: Opcode arch I
  Andi    :: Opcode arch I
  Slli    :: Opcode arch I
  -- | @srai@ and @srli@ combined into a single instruction.
  Sri     :: Opcode arch I
  Fence   :: Opcode arch I
  FenceI  :: Opcode arch I
  Csrrw   :: Opcode arch I
  Csrrs   :: Opcode arch I
  Csrrc   :: Opcode arch I
  Csrrwi  :: Opcode arch I
  Csrrsi  :: Opcode arch I
  Csrrci  :: Opcode arch I
  Ecb     :: Opcode arch I

  -- S type
  Sb :: Opcode arch S
  Sh :: Opcode arch S
  Sw :: Opcode arch S

  -- B type
  Beq  :: Opcode arch B
  Bne  :: Opcode arch B
  Blt  :: Opcode arch B
  Bge  :: Opcode arch B
  Bltu :: Opcode arch B
  Bgeu :: Opcode arch B

  -- U type
  Lui   :: Opcode arch U
  Auipc :: Opcode arch U

  -- J type
  Jal :: Opcode arch J


  -- X type (illegal instruction)
  Illegal :: Opcode arch X

  -- RV64I
  Addw   :: 64 <= ArchWidth arch => Opcode arch R
  Subw   :: 64 <= ArchWidth arch => Opcode arch R
  Sllw   :: 64 <= ArchWidth arch => Opcode arch R
  Srlw   :: 64 <= ArchWidth arch => Opcode arch R
  Sraw   :: 64 <= ArchWidth arch => Opcode arch R
  Lwu    :: 64 <= ArchWidth arch => Opcode arch I
  Ld     :: 64 <= ArchWidth arch => Opcode arch I
  Addiw  :: 64 <= ArchWidth arch => Opcode arch I
  Slliw  :: 64 <= ArchWidth arch => Opcode arch I
  -- | @sraiw@ and @srliw@ combined into a single instruction.
  Sriw   :: 64 <= ArchWidth arch => Opcode arch I
  -- | @ecall@ and @ebreak@ combined into a single instruction.
  Sd     :: 64 <= ArchWidth arch => Opcode arch S

  -- RV32M
  Mul    :: Opcode arch R
  Mulh   :: Opcode arch R
  Mulhsu :: Opcode arch R
  Mulhu  :: Opcode arch R
  Div    :: Opcode arch R
  Divu   :: Opcode arch R
  Rem    :: Opcode arch R
  Remu   :: Opcode arch R

  -- RV64M
  Mulw   :: 64 <= ArchWidth arch => Opcode arch R
  Divw   :: 64 <= ArchWidth arch => Opcode arch R
  Divuw  :: 64 <= ArchWidth arch => Opcode arch R
  Remw   :: 64 <= ArchWidth arch => Opcode arch R
  Remuw  :: 64 <= ArchWidth arch => Opcode arch R

-- Instances
$(return [])
deriving instance Show (Opcode arch fmt)
instance ShowF (Opcode arch)
deriving instance Eq (Opcode arch fmt)
instance EqF (Opcode arch) where
  eqF = (==)
instance TestEquality (Opcode arch) where
  testEquality = $(structuralTypeEquality [t|Opcode|] [])
instance OrdF (Opcode arch) where
  compareF = $(structuralTypeOrd [t|Opcode|] [])

----------------------------------------
-- OpBits

-- | Bits fixed by an opcode.
-- Holds all the bits that are fixed by a particular opcode. Each format maps to a
-- potentially different set of bits.
data OpBits :: Format -> * where
  ROpBits :: BitVector 7 -> BitVector 3 -> BitVector 7 -> OpBits R
  IOpBits :: BitVector 7 -> BitVector 3                -> OpBits I
  SOpBits :: BitVector 7 -> BitVector 3                -> OpBits S
  BOpBits :: BitVector 7 -> BitVector 3                -> OpBits B
  UOpBits :: BitVector 7                               -> OpBits U
  JOpBits :: BitVector 7                               -> OpBits J
  XOpBits ::                                              OpBits X

-- Instances
$(return [])
deriving instance Show (OpBits k)
instance ShowF OpBits
deriving instance Eq (OpBits k)
instance EqF OpBits where
  eqF = (==)
instance TestEquality OpBits where
  testEquality = $(structuralTypeEquality [t|OpBits|] [])
instance OrdF OpBits where
  compareF = $(structuralTypeOrd [t|OpBits|] [])

----------------------------------------
-- Instructions

-- | RISC-V Instruction, parameterized by base architecture and format.
data Instruction (arch :: BaseArch) (fmt :: Format) =
  Inst { instOpcode   :: Opcode arch fmt
       , instOperands :: Operands fmt
       }

-- Instances
$(return [])
instance Show (Instruction arch fmt) where
  show (Inst opcode operands) = show opcode ++ " " ++ show operands
instance ShowF (Instruction arch)
-- instance Eq (Instruction arch fmt) where
--   Inst opcode operands == Inst opcode' operands' =
--     opcode == opcode' && operands == operands'
-- instance EqF (Instruction arch) where
--   eqF = (==)
-- instance TestEquality (Instruction arch) where
--   (Inst opcode operands) `testEquality` (Inst opcode' operands') =
--     case (opcode   `testEquality` opcode',
--           operands `testEquality` operands') of
--       (Just Refl, Just Refl) -> Just Refl
--       _ -> Nothing
-- instance OrdF (Instruction arch) where
--   Inst opcode operands `compareF` Inst opcode' operands' =
--     case opcode `compareF` opcode' of
--       EQF -> operands `compareF` operands'
--       cmp -> cmp
