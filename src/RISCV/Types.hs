{-# LANGUAGE BinaryLiterals         #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

{-|
Module      : RISCV.Types
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module provides several data kinds and types that are used by many other
modules.

It defines the notion of a 'BaseArch', encoding which "base" RISC-V ISA a
machine uses, as well as that of 'Extensions', encoding which extensions are
present in that machine. Together, these two data kinds comprise the feature
model of the RISC-V architecture. There are more dimensions to be added,
including (but probably not limited to) which privilege levels are
supported.

We also define a data kind called 'Format', which denotes the format of an
instruction. An instruction's format specifies exactly which bits correspond to
what within an instruction word, and is therefore necessary for defining
encoding, decoding, and semantics.

Finally, we define 'Opcode', 'Operands', and 'Instruction', which encode our
representation of a RISC-V instruction.
-}

module RISCV.Types
  ( -- * Base architecture
    BaseArch(..), type RV32, type RV64, type RV128
  , BaseArchRepr(..)
  , ArchWidth
  , KnownArch
    -- * Extensions
  , Extensions(..), type Exts
  , MConfig(..), type MYes, type MNo
  , AConfig(..), type AYes, type ANo
  , FDConfig(..), type FDYes, type FYesDNo, type FDNo
  , ExtensionsRepr(..)
  , MConfigRepr(..)
  , AConfigRepr(..)
  , FDConfigRepr(..)
  , KnownExtensions
  , Extension(..), type MExt, type AExt, type FExt, type DExt
  , ExtensionsContains, type (<<)
  -- * Instructions
  , Format(..), type R, type I, type S, type B, type U, type J, type X
  , FormatRepr(..)
  , OperandTypes
  , OperandID(..)
  , Operands(..)
  , OpBitsTypes
  , OpBits(..)
  , Opcode(..)
  , Instruction(..)
  ) where

import Data.BitVector.Sized
import Data.Parameterized
import Data.Parameterized.List
import Data.Parameterized.TH.GADT
import GHC.TypeLits

----------------------------------------
-- Architecture types
-- | Base architecture types.
data BaseArch = RV32
              | RV64
              | RV128

type RV32  = 'RV32
type RV64  = 'RV64
type RV128 = 'RV128

-- | A runtime representative for 'BaseArch' for dependent typing.
data BaseArchRepr :: BaseArch -> * where
  RV32Repr  :: BaseArchRepr RV32
  RV64Repr  :: BaseArchRepr RV64
  RV128Repr :: BaseArchRepr RV128

-- | Maps an architecture to its register width.
type family ArchWidth (arch :: BaseArch) :: Nat where
  ArchWidth RV32  = 32
  ArchWidth RV64  = 64
  ArchWidth RV128 = 128

-- | Everything we might need to know about a 'BaseArch' at compile time.
type KnownArch arch = ( KnownNat (ArchWidth arch)
                      , KnownRepr BaseArchRepr arch)

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
instance KnownRepr BaseArchRepr RV32  where knownRepr = RV32Repr
instance KnownRepr BaseArchRepr RV64  where knownRepr = RV64Repr
instance KnownRepr BaseArchRepr RV128 where knownRepr = RV128Repr

----------------------------------------
-- Extension configurations

-- TODO: add a type family that determines whether an Extensions supports a particular
-- extension

-- | This data structure describes the RISC-V extensions that are enabled in a
-- particular type context.
data Extensions = Exts (MConfig, AConfig, FDConfig)

type Exts = 'Exts

-- | The M extension is either enabled or disabled.
data MConfig = MYes | MNo

type MYes = 'MYes
type MNo = 'MNo

-- | The A extension is either enabled or disabled.a
data AConfig = AYes | ANo
type AYes = 'AYes
type ANo = 'ANo

-- | The F and D extensions can be in one of three states: Both are enabled, only F
-- is enabled, or both are disabled.
data FDConfig = FDYes | FYesDNo | FDNo

type FDYes = 'FDYes
type FYesDNo = 'FYesDNo
type FDNo = 'FDNo

-- | A runtime representative for 'Extensions' for dependent typing.
data ExtensionsRepr :: Extensions -> * where
  ExtensionsRepr :: MConfigRepr m -> AConfigRepr a -> FDConfigRepr fd -> ExtensionsRepr (Exts '(m, a, fd))

instance ( KnownRepr MConfigRepr m
         , KnownRepr AConfigRepr a
         , KnownRepr FDConfigRepr fd
         ) => KnownRepr ExtensionsRepr (Exts '(m, a, fd)) where
  knownRepr = ExtensionsRepr knownRepr knownRepr knownRepr

-- | A runtime representative for 'MConfig' for dependent typing.
data MConfigRepr :: MConfig -> * where
  MYesRepr :: MConfigRepr MYes
  MNoRepr  :: MConfigRepr MNo

instance KnownRepr MConfigRepr MYes where knownRepr = MYesRepr
instance KnownRepr MConfigRepr MNo  where knownRepr = MNoRepr

-- | A runtime representative for 'AConfig' for dependent typing.
data AConfigRepr :: AConfig -> * where
  AYesRepr :: AConfigRepr AYes
  ANoRepr  :: AConfigRepr ANo

instance KnownRepr AConfigRepr AYes where knownRepr = AYesRepr
instance KnownRepr AConfigRepr ANo  where knownRepr = ANoRepr

-- | A runtime representative for 'FDConfig' for dependent typing.
data FDConfigRepr :: FDConfig -> * where
  FDYesRepr    :: FDConfigRepr FDYes
  FYesDNoRepr  :: FDConfigRepr FYesDNo
  FDNoRepr     :: FDConfigRepr FDNo

instance KnownRepr FDConfigRepr FDYes   where knownRepr = FDYesRepr
instance KnownRepr FDConfigRepr FYesDNo where knownRepr = FYesDNoRepr
instance KnownRepr FDConfigRepr FDNo    where knownRepr = FDNoRepr

-- | Everything we need to know about an 'Extensions' at compile time.
type KnownExtensions exts = KnownRepr ExtensionsRepr exts

-- | Type-level representation of a RISC-V extension.
data Extension = MExt | AExt | FExt | DExt

type MExt = 'MExt
type AExt = 'MExt
type FExt = 'FExt
type DExt = 'DExt

-- | Type operator that determines whether the 'Extensions' contains a particular
-- 'Extension'.
type family ExtensionsContains (exts :: Extensions) (e :: Extension) :: Bool where
  ExtensionsContains (Exts '( MYes, _, _))       MExt = 'True
  ExtensionsContains (Exts '(    _, AYes, _))    AExt = 'True
  ExtensionsContains (Exts '(    _, _, FDYes))   FExt = 'True
  ExtensionsContains (Exts '(    _, _, FYesDNo)) FExt = 'True
  ExtensionsContains (Exts '(    _, _, FDYes))   DExt = 'True
  ExtensionsContains _ _ = 'False

-- | 'ExtensionsContains' in constraint form.
type (<<) (e :: Extension) (exts :: Extensions)
  = ExtensionsContains exts e ~ 'True

----------------------------------------
-- Formats

-- | The RISC-V instruction formats. Each RISC-V instruction has one of several
-- encoding formats, corresponding to its operands and the way those operands are
-- laid out as bits in the instruction word. We include one additional format, X,
-- inhabited only by an illegal instruction.

data Format = R | I | S | B | U | J | A | X

type R = 'R
type I = 'I
type S = 'S
type B = 'B
type U = 'U
type J = 'J
type A = 'A
type X = 'X

-- | A runtime representative for 'Format' for dependent typing.
data FormatRepr :: Format -> * where
  RRepr :: FormatRepr R
  IRepr :: FormatRepr I
  SRepr :: FormatRepr S
  BRepr :: FormatRepr B
  URepr :: FormatRepr U
  JRepr :: FormatRepr J
  ARepr :: FormatRepr A
  XRepr :: FormatRepr X

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

instance KnownRepr FormatRepr R where knownRepr = RRepr
instance KnownRepr FormatRepr I where knownRepr = IRepr
instance KnownRepr FormatRepr S where knownRepr = SRepr
instance KnownRepr FormatRepr B where knownRepr = BRepr
instance KnownRepr FormatRepr U where knownRepr = URepr
instance KnownRepr FormatRepr J where knownRepr = JRepr
instance KnownRepr FormatRepr A where knownRepr = ARepr
instance KnownRepr FormatRepr X where knownRepr = XRepr

----------------------------------------
-- Operands

-- | Maps each format type to the list of the corresponding operand widths.
type family OperandTypes (fmt :: Format) :: [Nat] where
  OperandTypes R = '[5, 5, 5]
  OperandTypes I = '[5, 5, 12]
  OperandTypes S = '[5, 5, 12]
  OperandTypes B = '[5, 5, 12]
  OperandTypes U = '[5, 20]
  OperandTypes J = '[5, 20]
  OperandTypes A = '[5, 5, 5, 1, 1]
  OperandTypes X = '[32]

-- | An 'OperandID' is just an index into a particular format's 'OperandTypes' list.
newtype OperandID (fmt :: Format) (w :: Nat) = OperandID { unOperandID :: Index (OperandTypes fmt) w }

-- | RISC-V Operand lists, parameterized by format.
data Operands :: Format -> * where
  Operands :: FormatRepr fmt -> List BitVector (OperandTypes fmt) -> Operands fmt

$(return [])
deriving instance Show (Operands k)
instance ShowF Operands

----------------------------------------
-- OpBits

-- | Maps each format to the list of the corresponding opbits widths.
type family OpBitsTypes (fmt :: Format) :: [Nat] where
  OpBitsTypes R = '[7, 3, 7]
  OpBitsTypes I = '[7, 3]
  OpBitsTypes S = '[7, 3]
  OpBitsTypes B = '[7, 3]
  OpBitsTypes U = '[7]
  OpBitsTypes J = '[7]
  OpBitsTypes A = '[7, 3, 5]
  OpBitsTypes X = '[]

-- | Bits fixed by an opcode.
-- Holds all the bits that are fixed by a particular opcode.
data OpBits :: Format -> * where
  OpBits :: FormatRepr fmt -> List BitVector (OpBitsTypes fmt) -> OpBits fmt

$(return [])
instance TestEquality OpBits where
  testEquality = $(structuralTypeEquality [t|OpBits|]
                   [ (ConType [t|FormatRepr|] `TypeApp` AnyType, [|testEquality|])
                   , (ConType [t|List|] `TypeApp` AnyType `TypeApp` AnyType, [|testEquality|])
                   ])
instance OrdF OpBits where
  compareF = $(structuralTypeOrd [t|OpBits|]
               [ (ConType [t|FormatRepr|] `TypeApp` AnyType, [|compareF|])
               , (ConType [t|List|] `TypeApp` AnyType `TypeApp` AnyType, [|compareF|])
               ])


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

  -- RV32
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
  -- | @ecall@ and @ebreak@ combined into a single instruction.
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

  -- RV64
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

  -- RV32A
  Lrw      :: Opcode arch A
  Scw      :: Opcode arch A
  Amoswapw :: Opcode arch A
  Amoaddw  :: Opcode arch A
  Amoxorw  :: Opcode arch A
  Amoandw  :: Opcode arch A
  Amoorw   :: Opcode arch A
  Amominw  :: Opcode arch A
  Amomaxw  :: Opcode arch A
  Amominuw :: Opcode arch A
  Amomaxuw :: Opcode arch A

  -- RV64A
  Lrd      :: 64 <= ArchWidth arch => Opcode arch A
  Scd      :: 64 <= ArchWidth arch => Opcode arch A
  Amoswapd :: 64 <= ArchWidth arch => Opcode arch A
  Amoaddd  :: 64 <= ArchWidth arch => Opcode arch A
  Amoxord  :: 64 <= ArchWidth arch => Opcode arch A
  Amoandd  :: 64 <= ArchWidth arch => Opcode arch A
  Amoord   :: 64 <= ArchWidth arch => Opcode arch A
  Amomind  :: 64 <= ArchWidth arch => Opcode arch A
  Amomaxd  :: 64 <= ArchWidth arch => Opcode arch A
  Amominud :: 64 <= ArchWidth arch => Opcode arch A
  Amomaxud :: 64 <= ArchWidth arch => Opcode arch A


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
-- Instructions

-- | RISC-V Instruction, parameterized by base architecture and format.
data Instruction (arch :: BaseArch) (fmt :: Format) =
  Inst (Opcode arch fmt) (Operands fmt)

-- Instances
$(return [])
instance Show (Instruction arch fmt) where
  show (Inst opcode operands) = show opcode ++ " " ++ show operands
instance ShowF (Instruction arch)
