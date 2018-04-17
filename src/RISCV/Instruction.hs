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
    -- * Operands
  , Operands(..)
  ) where

import Data.BitVector.Sized
import Data.Parameterized
import Data.Parameterized.TH.GADT

import RISCV.Types

----------------------------------------
-- Operands

-- TODO: Consider making the operand arguments lists of bit vector types???
-- | RISC-V Operand lists, parameterized by format. There is exactly one constructor
-- per format.
data Operands :: Format -> * where
  ROperands :: BitVector 5 -> BitVector 5  -> BitVector 5  -> Operands R
  IOperands :: BitVector 5 -> BitVector 5  -> BitVector 12 -> Operands I
  SOperands :: BitVector 5 -> BitVector 5  -> BitVector 12 -> Operands S
  BOperands :: BitVector 5 -> BitVector 5  -> BitVector 12 -> Operands B
  UOperands :: BitVector 5 -> BitVector 20                 -> Operands U
  JOperands :: BitVector 5 -> BitVector 20                 -> Operands J
  XOperands :: BitVector 32                                -> Operands X

-- Instances
$(return [])
deriving instance Show (Operands k)
instance ShowF Operands
deriving instance Eq (Operands k)
instance EqF Operands where
  eqF = (==)
instance TestEquality Operands where
  testEquality = $(structuralTypeEquality [t|Operands|] [])
instance OrdF Operands where
  compareF = $(structuralTypeOrd [t|Operands|] [])

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
instance Eq (Instruction arch fmt) where
  Inst opcode operands == Inst opcode' operands' =
    opcode == opcode' && operands == operands'
instance EqF (Instruction arch) where
  eqF = (==)
instance TestEquality (Instruction arch) where
  (Inst opcode operands) `testEquality` (Inst opcode' operands') =
    case (opcode   `testEquality` opcode',
          operands `testEquality` operands') of
      (Just Refl, Just Refl) -> Just Refl
      _ -> Nothing
instance OrdF (Instruction arch) where
  Inst opcode operands `compareF` Inst opcode' operands' =
    case opcode `compareF` opcode' of
      EQF -> operands `compareF` operands'
      cmp -> cmp

----------------------------------------
-- TODO: Add compressed instructions
--
-- This task is a little different from just adding an extension. In fact, it's
-- really orthogonal to it, because we aren't adding *any* new instructions! Instead,
-- we define a separate opcodeOpBits mapping for those special instructions. Since
-- those mappings will under-specify the instruction word in a sense, we will need to
-- encode a predicate that determines whether a particular instruction instance is
-- compressable or not. The encode function will have to take some kind of flag, and
-- check both the flag and the compressability of the instruction before determining
-- how to encode it.
--
-- Another approach would be to use bit lenses to, instead of creating lenses mapping
-- the operands into a 16-bit instruction word, map the 16-bit instruction word into
-- the 32-bit one. Then we could create a separate function, maybe called compress,
-- that would compress those instructions that could be compressed. This might be a
-- slicker approach. We'd still need the compressibility predicate; I'm not sure if
-- there is a clever way to weave that predicate in with the BitLayout stuff. Have to
-- think about it. My feeling is that it makes more sense to just define one
-- "compressible" function and see if that looks adequate.
--
-- Do I want the decoder to know whether it is "allowed" to decode compressed
-- instructions, or any extension for that matter? My current feeling is that we
-- might as well not bother making a distinction between the various extensions in
-- the encoding/decoding end of things if we can help it. It would be nice to somehow
-- tag instructions with the extensions they belong to, though, if only for error
-- reporting purposes.
--
-- On the decoding end of things, it's a little trickier. Right now the decoder takes
-- a 32-bit word. I'm thinking maybe I should have a separate
--
--   decodeC :: BitVector 16 -> Maybe (BitVector 32)
--
-- which returns the full 32-bit word that the compressed instruction would expand
-- to. This is assuming I use the bit lens approach rather than creating an entirely
-- separate mapping for compressed instructions into the Instruction type.
--
-- Now that I'm really thinking about it, I believe the above is exactly the right
-- approach. It captures the fact that these compressed guys are being embedded into
-- a larger instruction word.
