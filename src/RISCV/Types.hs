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
  , PrivConfig(..), type PrivM, PrivMU, PrivMSU
  , MConfig(..), type MYes, type MNo
  , AConfig(..), type AYes, type ANo
  , FDConfig(..), type FDYes, type FYesDNo, type FDNo
  , ExtensionsRepr(..)
  , PrivConfigRepr(..)
  , MConfigRepr(..)
  , AConfigRepr(..)
  , FDConfigRepr(..)
  , KnownExtensions
  , Extension(..), type AExt, type DExt, type FExt, type MExt, type SExt, type UExt
  , ExtensionsContains, type (<<)
  -- * Instructions
  , Format(..), type R, type I, type S, type B, type U, type J, type A, type X
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
data Extensions = Exts (PrivConfig, MConfig, AConfig, FDConfig)

type Exts = 'Exts

-- | The implemented privilege modes are either M, MU, or MSU.
data PrivConfig = PrivM | PrivMU | PrivMSU

type PrivM = 'PrivM
type PrivMU = 'PrivMU
type PrivMSU = 'PrivMSU

-- | The M extension is either enabled or disabled.
data MConfig = MYes | MNo

type MYes = 'MYes
type MNo = 'MNo

-- | The A extension is either enabled or disabled.
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
  ExtensionsRepr :: PrivConfigRepr priv
                 -> MConfigRepr m
                 -> AConfigRepr a
                 -> FDConfigRepr fd
                 -> ExtensionsRepr (Exts '(priv, m, a, fd))

instance ( KnownRepr PrivConfigRepr priv
         , KnownRepr MConfigRepr m
         , KnownRepr AConfigRepr a
         , KnownRepr FDConfigRepr fd
         ) => KnownRepr ExtensionsRepr (Exts '(priv, m, a, fd)) where
  knownRepr = ExtensionsRepr knownRepr knownRepr knownRepr knownRepr

-- | A runtime representative for 'PrivConfig' for dependent typing.
data PrivConfigRepr :: PrivConfig -> * where
  PrivMRepr   :: PrivConfigRepr PrivM
  PrivMURepr  :: PrivConfigRepr PrivMU
  PrivMSURepr :: PrivConfigRepr PrivMSU

instance KnownRepr PrivConfigRepr PrivM   where knownRepr = PrivMRepr
instance KnownRepr PrivConfigRepr PrivMU  where knownRepr = PrivMURepr
instance KnownRepr PrivConfigRepr PrivMSU where knownRepr = PrivMSURepr

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
data Extension = AExt | DExt | FExt | MExt | SExt | UExt

type AExt = 'MExt
type DExt = 'DExt
type FExt = 'FExt
type MExt = 'MExt
type SExt = 'SExt
type UExt = 'UExt

-- | Type operator that determines whether the 'Extensions' contains a particular
-- 'Extension'.
type family ExtensionsContains (exts :: Extensions) (e :: Extension) :: Bool where
  ExtensionsContains (Exts '(_, MYes, _, _))       MExt = 'True
  ExtensionsContains (Exts '(_,    _, AYes, _))    AExt = 'True
  ExtensionsContains (Exts '(_,    _, _, FDYes))   FExt = 'True
  ExtensionsContains (Exts '(_,    _, _, FYesDNo)) FExt = 'True
  ExtensionsContains (Exts '(_,    _, _, FDYes))   DExt = 'True
  ExtensionsContains (Exts '(PrivMU,  _, _, _))    UExt = 'True
  ExtensionsContains (Exts '(PrivMSU, _, _, _))    UExt = 'True
  ExtensionsContains (Exts '(PrivMSU, _, _, _))    SExt = 'True
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

data Format = R | I | S | B | U | J | P | A | X

type R = 'R
type I = 'I
type S = 'S
type B = 'B
type U = 'U
type J = 'J
type P = 'P
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
  PRepr :: FormatRepr P
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
instance KnownRepr FormatRepr P where knownRepr = PRepr
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
  OperandTypes P = '[]
  OperandTypes A = '[5, 5, 5, 1, 1]
  OperandTypes X = '[32]

-- | An 'OperandID' is just an index into a particular format's 'OperandTypes' list.
newtype OperandID (fmt :: Format) (w :: Nat) = OperandID { unOperandID :: Index (OperandTypes fmt) w }
  deriving Eq

$(return [])
instance TestEquality (OperandID fmt) where
  testEquality = $(structuralTypeEquality [t|OperandID|]
                    [(AnyType `TypeApp` AnyType `TypeApp` AnyType, [|testEquality|])
                    ])

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
  OpBitsTypes P = '[32]
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
data Opcode :: BaseArch -> Extensions -> Format -> * where

  -- RV32
  Add  :: Opcode arch exts R
  Sub  :: Opcode arch exts R
  Sll  :: Opcode arch exts R
  Slt  :: Opcode arch exts R
  Sltu :: Opcode arch exts R
  Xor  :: Opcode arch exts R
  Srl  :: Opcode arch exts R
  Sra  :: Opcode arch exts R
  Or   :: Opcode arch exts R
  And  :: Opcode arch exts R

  Jalr    :: Opcode arch exts I
  Lb      :: Opcode arch exts I
  Lh      :: Opcode arch exts I
  Lw      :: Opcode arch exts I
  Lbu     :: Opcode arch exts I
  Lhu     :: Opcode arch exts I
  Addi    :: Opcode arch exts I
  Slti    :: Opcode arch exts I
  Sltiu   :: Opcode arch exts I
  Xori    :: Opcode arch exts I
  Ori     :: Opcode arch exts I
  Andi    :: Opcode arch exts I
  Slli    :: Opcode arch exts I
  -- | @srai@ and @srli@ combined into a single instruction.
  Sri     :: Opcode arch exts I
  Fence   :: Opcode arch exts I
  FenceI  :: Opcode arch exts I
  Csrrw   :: Opcode arch exts I
  Csrrs   :: Opcode arch exts I
  Csrrc   :: Opcode arch exts I
  Csrrwi  :: Opcode arch exts I
  Csrrsi  :: Opcode arch exts I
  Csrrci  :: Opcode arch exts I

  Ecall   :: Opcode arch exts P
  Ebreak  :: Opcode arch exts P

  -- S type
  Sb :: Opcode arch exts S
  Sh :: Opcode arch exts S
  Sw :: Opcode arch exts S

  -- B type
  Beq  :: Opcode arch exts B
  Bne  :: Opcode arch exts B
  Blt  :: Opcode arch exts B
  Bge  :: Opcode arch exts B
  Bltu :: Opcode arch exts B
  Bgeu :: Opcode arch exts B

  -- U type
  Lui   :: Opcode arch exts U
  Auipc :: Opcode arch exts U

  -- J type
  Jal :: Opcode arch exts J


  -- X type (illegal instruction)
  Illegal :: Opcode arch exts X

  -- RV64
  Addw   :: 64 <= ArchWidth arch => Opcode arch exts R
  Subw   :: 64 <= ArchWidth arch => Opcode arch exts R
  Sllw   :: 64 <= ArchWidth arch => Opcode arch exts R
  Srlw   :: 64 <= ArchWidth arch => Opcode arch exts R
  Sraw   :: 64 <= ArchWidth arch => Opcode arch exts R
  Lwu    :: 64 <= ArchWidth arch => Opcode arch exts I
  Ld     :: 64 <= ArchWidth arch => Opcode arch exts I
  Addiw  :: 64 <= ArchWidth arch => Opcode arch exts I
  Slliw  :: 64 <= ArchWidth arch => Opcode arch exts I
  -- | @sraiw@ and @srliw@ combined into a single instruction.
  Sriw   :: 64 <= ArchWidth arch => Opcode arch exts I
  Sd     :: 64 <= ArchWidth arch => Opcode arch exts S

  -- M privileged instructions
  Mret :: Opcode arch exts P
  Wfi  :: Opcode arch exts P

  -- RV32M
  Mul    :: MExt << exts => Opcode arch exts R
  Mulh   :: MExt << exts => Opcode arch exts R
  Mulhsu :: MExt << exts => Opcode arch exts R
  Mulhu  :: MExt << exts => Opcode arch exts R
  Div    :: MExt << exts => Opcode arch exts R
  Divu   :: MExt << exts => Opcode arch exts R
  Rem    :: MExt << exts => Opcode arch exts R
  Remu   :: MExt << exts => Opcode arch exts R

  -- RV64M
  Mulw   :: (64 <= ArchWidth arch, MExt << exts) => Opcode arch exts R
  Divw   :: (64 <= ArchWidth arch, MExt << exts) => Opcode arch exts R
  Divuw  :: (64 <= ArchWidth arch, MExt << exts) => Opcode arch exts R
  Remw   :: (64 <= ArchWidth arch, MExt << exts) => Opcode arch exts R
  Remuw  :: (64 <= ArchWidth arch, MExt << exts) => Opcode arch exts R

  -- RV32A
  Lrw      :: AExt << exts => Opcode arch exts A
  Scw      :: AExt << exts => Opcode arch exts A
  Amoswapw :: AExt << exts => Opcode arch exts A
  Amoaddw  :: AExt << exts => Opcode arch exts A
  Amoxorw  :: AExt << exts => Opcode arch exts A
  Amoandw  :: AExt << exts => Opcode arch exts A
  Amoorw   :: AExt << exts => Opcode arch exts A
  Amominw  :: AExt << exts => Opcode arch exts A
  Amomaxw  :: AExt << exts => Opcode arch exts A
  Amominuw :: AExt << exts => Opcode arch exts A
  Amomaxuw :: AExt << exts => Opcode arch exts A

  -- RV64A
  Lrd      :: (64 <= ArchWidth arch, AExt << exts) => Opcode arch exts A
  Scd      :: (64 <= ArchWidth arch, AExt << exts) => Opcode arch exts A
  Amoswapd :: (64 <= ArchWidth arch, AExt << exts) => Opcode arch exts A
  Amoaddd  :: (64 <= ArchWidth arch, AExt << exts) => Opcode arch exts A
  Amoxord  :: (64 <= ArchWidth arch, AExt << exts) => Opcode arch exts A
  Amoandd  :: (64 <= ArchWidth arch, AExt << exts) => Opcode arch exts A
  Amoord   :: (64 <= ArchWidth arch, AExt << exts) => Opcode arch exts A
  Amomind  :: (64 <= ArchWidth arch, AExt << exts) => Opcode arch exts A
  Amomaxd  :: (64 <= ArchWidth arch, AExt << exts) => Opcode arch exts A
  Amominud :: (64 <= ArchWidth arch, AExt << exts) => Opcode arch exts A
  Amomaxud :: (64 <= ArchWidth arch, AExt << exts) => Opcode arch exts A

-- Instances
$(return [])
deriving instance Show (Opcode arch exts fmt)
instance ShowF (Opcode arch exts)
deriving instance Eq (Opcode arch exts fmt)
instance EqF (Opcode arch exts) where
  eqF = (==)
instance TestEquality (Opcode arch exts) where
  testEquality = $(structuralTypeEquality [t|Opcode|] [])
instance OrdF (Opcode arch exts) where
  compareF = $(structuralTypeOrd [t|Opcode|] [])

----------------------------------------
-- Instructions

-- | RISC-V Instruction, parameterized by base architecture and format.
data Instruction (arch :: BaseArch) (exts :: Extensions) (fmt :: Format) =
  Inst (Opcode arch exts fmt) (Operands fmt)

-- Instances
$(return [])
instance Show (Instruction arch exts fmt) where
  show (Inst opcode operands) = show opcode ++ " " ++ show operands
instance ShowF (Instruction arch exts)
