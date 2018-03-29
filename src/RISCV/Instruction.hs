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
    -- * Architecture types
  , BaseArch(..)
  , BaseArchRepr(..)
  , ArchWidth
  , KnownArch
    -- * Instruction formats
  , Format(..)
  , FormatRepr(..)
    -- * Opcodes
  , Opcode(..)
  , OpBits(..)
    -- * Operands
  , Operands(..)
  ) where

import Data.BitVector.Sized
import Data.Parameterized
import Data.Parameterized.TH.GADT
import GHC.TypeLits

----------------------------------------
-- Architecture types
-- | Base architecture types
data BaseArch = RV32I
              | RV32E
              | RV64I
              | RV128I

data BaseArchRepr :: BaseArch -> * where
  RV32IRepr  :: BaseArchRepr 'RV32I
  RV32ERepr  :: BaseArchRepr 'RV32E
  RV64IRepr  :: BaseArchRepr 'RV64I
  RV128IRepr :: BaseArchRepr 'RV128I

-- | Maps an architecture to its register width
type family ArchWidth (arch :: BaseArch) :: Nat where
  ArchWidth 'RV32I  = 32
  ArchWidth 'RV32E  = 32
  ArchWidth 'RV64I  = 64
  ArchWidth 'RV128I = 128

-- Instances
$(return [])
deriving instance Show (BaseArchRepr k)
instance ShowF BaseArchRepr
deriving instance Eq (BaseArchRepr k)
instance EqF BaseArchRepr where
  eqF = (==)
instance TestEquality BaseArchRepr where
  testEquality = $(structuralTypeEquality [t|BaseArchRepr|] [])
instance OrdF BaseArchRepr where
  compareF = $(structuralTypeOrd [t|BaseArchRepr|] [])
instance KnownRepr BaseArchRepr 'RV32I  where knownRepr = RV32IRepr
instance KnownRepr BaseArchRepr 'RV32E  where knownRepr = RV32ERepr
instance KnownRepr BaseArchRepr 'RV64I  where knownRepr = RV64IRepr
instance KnownRepr BaseArchRepr 'RV128I where knownRepr = RV128IRepr

type KnownArch arch = KnownNat (ArchWidth arch)

----------------------------------------
-- Formats

-- | The RISC-V instruction formats. Each RISC-V instruction has one of several
-- encoding formats, corresponding to its operands and the way those operands are
-- laid out as bits in the instruction word. We include one additional format, X,
-- inhabited only by an illegal instruction.
--
-- NOTE: Our formats differ somewhat from the RISC-V ISA manual. The manual
-- classifies instructions into formats based on (TODO: what?). Our formats, while
-- very close to those in the manual, more exactly specify bits that are fixed by the
-- instruction (i.e. the ones that never vary, no matter what the operands of the
-- instruction are). In the manual, some instructions which have the same format
-- actually have different numbers of operands! For example: add (from RV32I) has
-- three operands, and fadd.s (from RV32F) has four operands (the additional one
-- being the rounding mode). Here, "operand" means "bit vector(s) tied to a
-- particular input for the instruction's semantics."

data Format = R | I | S | B | U | J | E | X

-- | A type-level representative of the format. Particularly useful when decoding
-- instructions since we won't know ahead of time what format to classify them as.
data FormatRepr :: Format -> * where
  RRepr :: FormatRepr 'R
  IRepr :: FormatRepr 'I
  SRepr :: FormatRepr 'S
  BRepr :: FormatRepr 'B
  URepr :: FormatRepr 'U
  JRepr :: FormatRepr 'J
  ERepr :: FormatRepr 'E
  XRepr :: FormatRepr 'X

-- Instances
$(return [])
deriving instance Show (FormatRepr k)
instance ShowF FormatRepr
deriving instance Eq (FormatRepr k)
instance EqF FormatRepr where
  eqF = (==)
instance TestEquality FormatRepr where
  testEquality = $(structuralTypeEquality [t|FormatRepr|] [])
instance OrdF FormatRepr where
  compareF = $(structuralTypeOrd [t|FormatRepr|] [])
instance KnownRepr FormatRepr 'R where knownRepr = RRepr
instance KnownRepr FormatRepr 'I where knownRepr = IRepr
instance KnownRepr FormatRepr 'S where knownRepr = SRepr
instance KnownRepr FormatRepr 'B where knownRepr = BRepr
instance KnownRepr FormatRepr 'U where knownRepr = URepr
instance KnownRepr FormatRepr 'J where knownRepr = JRepr
instance KnownRepr FormatRepr 'E where knownRepr = ERepr
instance KnownRepr FormatRepr 'X where knownRepr = XRepr

----------------------------------------
-- Operands

-- TODO: Consider making the operand arguments lists of bit vector types???
-- | RISC-V Operand lists, parameterized by format. There is exactly one constructor
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

-- | RISC-V Opcodes, parameterized by format.
data Opcode :: BaseArch -> Format -> * where

  -- R type
  Add    :: Opcode arch 'R -- RV32I
  Sub    :: Opcode arch 'R
  Sll    :: Opcode arch 'R
  Slt    :: Opcode arch 'R
  Sltu   :: Opcode arch 'R
  Xor    :: Opcode arch 'R
  Srl    :: Opcode arch 'R
  Sra    :: Opcode arch 'R
  Or     :: Opcode arch 'R
  And    :: Opcode arch 'R
  Mul    :: Opcode arch 'R -- RV32M
  Mulh   :: Opcode arch 'R
  Mulhsu :: Opcode arch 'R
  Mulhu  :: Opcode arch 'R
  Div    :: Opcode arch 'R
  Divu   :: Opcode arch 'R
  Rem    :: Opcode arch 'R
  Remu   :: Opcode arch 'R

  -- I type
  Jalr    :: Opcode arch 'I -- RV32I
  Lb      :: Opcode arch 'I
  Lh      :: Opcode arch 'I
  Lw      :: Opcode arch 'I
  Lbu     :: Opcode arch 'I
  Lhu     :: Opcode arch 'I
  Addi    :: Opcode arch 'I
  Slti    :: Opcode arch 'I
  Sltiu   :: Opcode arch 'I
  Xori    :: Opcode arch 'I
  Ori     :: Opcode arch 'I
  Andi    :: Opcode arch 'I
  -- TODO: the shift instructions are also a slightly different format, we accept
  -- that for the time being.
  Slli    :: Opcode arch 'I
  Srli    :: Opcode arch 'I
  Srai    :: Opcode arch 'I
  -- TODO: Fence and FenceI are both slightly wonky; we might need to separate them
  -- out into separate formats like we did with Ecall and Ebreak. Fence uses the
  -- immediate bits to encode additional operands and FenceI requires them to be 0,
  -- so ideally we'd capture that in the type. It's still possible to fit them into
  -- the I format for now, but it's actually the case (just like with shifts) only
  -- certain operands are allowed (in the case of Fence.i, all the operands *must* be
  -- 0).
  Fence   :: Opcode arch 'I
  FenceI  :: Opcode arch 'I
  Csrrw   :: Opcode arch 'I
  Csrrs   :: Opcode arch 'I
  Csrrc   :: Opcode arch 'I
  Csrrwi  :: Opcode arch 'I
  Csrrsi  :: Opcode arch 'I
  Csrrci  :: Opcode arch 'I

  -- S type
  Sb :: Opcode arch 'S -- RV32I
  Sh :: Opcode arch 'S
  Sw :: Opcode arch 'S

  -- B type
  Beq  :: Opcode arch 'B -- RV32I
  Bne  :: Opcode arch 'B
  Blt  :: Opcode arch 'B
  Bge  :: Opcode arch 'B
  Bltu :: Opcode arch 'B
  Bgeu :: Opcode arch 'B

  -- U type
  Lui   :: Opcode arch 'U -- RV32I
  Auipc :: Opcode arch 'U

  -- J type
  Jal :: Opcode arch 'J -- RV32I

  -- E type
  Ecall   :: Opcode arch 'E -- RV32I
  Ebreak  :: Opcode arch 'E

  -- X type (illegal instruction)
  Illegal :: Opcode arch 'X -- RV32I

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
  ROpBits :: BitVector 7 -> BitVector 3 -> BitVector 7 -> OpBits 'R
  IOpBits :: BitVector 7 -> BitVector 3                -> OpBits 'I
  SOpBits :: BitVector 7 -> BitVector 3                -> OpBits 'S
  BOpBits :: BitVector 7 -> BitVector 3                -> OpBits 'B
  UOpBits :: BitVector 7                               -> OpBits 'U
  JOpBits :: BitVector 7                               -> OpBits 'J
  EOpBits :: BitVector 7 -> BitVector 25               -> OpBits 'E
  XOpBits ::                                              OpBits 'X

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

-- | RISC-V Instruction, parameterized by format.
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
