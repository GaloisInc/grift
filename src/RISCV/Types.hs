{-
This file is part of GRIFT (Galois RISC-V ISA Formal Tools).

GRIFT is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GRIFT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero Public License for more details.

You should have received a copy of the GNU Affero Public License
along with GRIFT.  If not, see <https://www.gnu.org/licenses/>.
-}

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
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : RISCV.Types
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module provides several data kinds and types that are used by many other
modules.

It defines the 'RV' data kind, which comprises the type-level feature model of the
RISC-V architecture. 'RV' is a wrapper for 'BaseArch' (32- vs. 64-bit) and
'Extensions' (list of enabled extensions).

We also define a data kind called 'Format', which denotes the format of an
instruction. An instruction's format specifies exactly which bits correspond to
what within an instruction word, and is therefore necessary for defining
encoding, decoding, and semantics.

Finally, we define 'Opcode', 'Operands', and 'Instruction', which encode our
representation of a RISC-V instruction.
-}

module RISCV.Types
  ( -- * RISC-V Configuration
    RV(..), type RVConfig
  , RVRepr(..)
  , RVWidth, RVFloatWidth, type (<<)
  , FDFloatWidth
  , RVFloatType
  , KnownRVWidth
  , KnownRVFloatWidth
  , KnownRVFloatType
  , KnownRV
    -- ** Common RISC-V Configurations
    -- | Provided for convenience.
  , RV32I
  , RV32IM
  , RV32IMA
  , RV32IMAF
  , RV32IMAFD
  , RV64I
  , RV64IM
  , RV64IMA
  , RV64IMAF
  , RV64IMAFD
    -- * Base architecture
  , BaseArch(..), type RV32, type RV64, type RV128
  , ArchWidth
  , BaseArchRepr(..)
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
  , ExtensionsContains
  -- * Instructions
  , Format(..)
  , type R, type I, type S, type B, type U, type J
  , type H, type P, type A, type R2, type R3, type R4
  , type RX, type X
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

type family ArchWidth (arch :: BaseArch) :: Nat where
  ArchWidth RV32 = 32
  ArchWidth RV64 = 64
  ArchWidth RV128 = 128

-- | Everything we might need to know about a 'BaseArch' at compile time.
type KnownArch arch = (KnownRepr BaseArchRepr arch, KnownNat (ArchWidth arch))

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

----------------------------------------
-- | RISC-V Configuration data kind. This is mainly provided as a wrapper for the
-- arch and exts type variables, to keep them in one place.
data RV = RVConfig (BaseArch, Extensions)

type RVConfig = 'RVConfig

-- | Type-level representation of 'RV'.
data RVRepr :: RV -> * where
  RVRepr :: BaseArchRepr arch -> ExtensionsRepr exts -> RVRepr (RVConfig '(arch, exts))

instance (KnownArch arch, KnownExtensions exts) => KnownRepr RVRepr (RVConfig '(arch, exts)) where
  knownRepr = RVRepr knownRepr knownRepr

-- | The width of the GPRs are known at compile time.
type KnownRVWidth rv = KnownNat (RVWidth rv)

-- | The width of the floating point registers are known at compile time.
type KnownRVFloatWidth rv = KnownNat (RVFloatWidth rv)

type KnownRVFloatType rv = KnownRepr FDConfigRepr (RVFloatType rv)

-- | Everything we need to know about an 'RV' at compile time.
type KnownRV rv = ( KnownRepr RVRepr rv
                  , KnownNat (RVWidth rv)
                  , KnownNat (RVFloatWidth rv))

-- | Maps a RISC-V configuration to its register width, as a 'Nat'.
type family RVWidth (rv :: RV) :: Nat where
  RVWidth (RVConfig '(arch, _)) = ArchWidth arch

type family FDFloatWidth (fd :: FDConfig) :: Nat where
  FDFloatWidth FDYes = 64
  FDFloatWidth _ = 32

-- | This should only be used in a context where FExt << rv.
type family RVFloatWidth (rv :: RV) :: Nat where
  RVFloatWidth rv = FDFloatWidth (RVFloatType rv)

type family RVFloatType (rv :: RV) :: FDConfig where
  RVFloatType (RVConfig '(_, Exts '(_, _, _, fd))) = fd

-- | 'ExtensionsContains' in constraint form.
type family (<<) (e :: Extension) (rv :: RV) where
  e << RVConfig '(_, exts)= ExtensionsContains exts e ~ 'True

-- type synonyms for common RVConfigs.
type RV32I     = RVConfig '(RV32, Exts '(PrivM, MNo,  ANo,  FDNo))
type RV32IM    = RVConfig '(RV32, Exts '(PrivM, MYes, ANo,  FDNo))
type RV32IMA   = RVConfig '(RV32, Exts '(PrivM, MYes, AYes, FDNo))
type RV32IMAF  = RVConfig '(RV32, Exts '(PrivM, MYes, AYes, FYesDNo))
type RV32IMAFD = RVConfig '(RV32, Exts '(PrivM, MYes, AYes, FDYes))
type RV64I     = RVConfig '(RV64, Exts '(PrivM, MNo,  ANo,  FDNo))
type RV64IM    = RVConfig '(RV64, Exts '(PrivM, MYes, ANo,  FDNo))
type RV64IMA   = RVConfig '(RV64, Exts '(PrivM, MYes, AYes, FDNo))
type RV64IMAF  = RVConfig '(RV64, Exts '(PrivM, MYes, AYes, FYesDNo))
type RV64IMAFD = RVConfig '(RV64, Exts '(PrivM, MYes, AYes, FDYes))

----------------------------------------
-- Formats

-- | The RISC-V instruction formats. Each RISC-V instruction has one of several
-- encoding formats, corresponding to its operands and the way those operands are
-- laid out as bits in the instruction word. We include one additional format, X,
-- inhabited only by an illegal instruction.

data Format = R | I | S | B | U | J | H | P | A | R2 | R3 | R4 | RX | X

type R  = 'R
type I  = 'I
type S  = 'S
type B  = 'B
type U  = 'U
type J  = 'J
type H  = 'H
type P  = 'P
type A  = 'A
type R2 = 'R2
type R3 = 'R3
type R4 = 'R4
type RX = 'RX
type X  = 'X

-- | A runtime representative for 'Format' for dependent typing.
data FormatRepr :: Format -> * where
  RRepr  :: FormatRepr R
  IRepr  :: FormatRepr I
  SRepr  :: FormatRepr S
  BRepr  :: FormatRepr B
  URepr  :: FormatRepr U
  JRepr  :: FormatRepr J
  HRepr  :: FormatRepr H
  PRepr  :: FormatRepr P
  ARepr  :: FormatRepr A
  R2Repr :: FormatRepr R2
  R3Repr :: FormatRepr R3
  R4Repr :: FormatRepr R4
  RXRepr :: FormatRepr RX
  XRepr  :: FormatRepr X

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

instance KnownRepr FormatRepr R  where knownRepr = RRepr
instance KnownRepr FormatRepr I  where knownRepr = IRepr
instance KnownRepr FormatRepr S  where knownRepr = SRepr
instance KnownRepr FormatRepr B  where knownRepr = BRepr
instance KnownRepr FormatRepr U  where knownRepr = URepr
instance KnownRepr FormatRepr J  where knownRepr = JRepr
instance KnownRepr FormatRepr H  where knownRepr = HRepr
instance KnownRepr FormatRepr P  where knownRepr = PRepr
instance KnownRepr FormatRepr A  where knownRepr = ARepr
instance KnownRepr FormatRepr R2 where knownRepr = R2Repr
instance KnownRepr FormatRepr R3 where knownRepr = R3Repr
instance KnownRepr FormatRepr R4 where knownRepr = R4Repr
instance KnownRepr FormatRepr RX where knownRepr = RXRepr
instance KnownRepr FormatRepr X  where knownRepr = XRepr

----------------------------------------
-- Operands

-- TODO: It might make sense to reverse the order in this and OpBitsTypes; it's
-- actually pretty confusing because it doesn't match how it looks when you're just
-- looking at the bits. Also, BitLayout uses the reverse ordering for exactly this
-- reason.
-- | Maps each format type to the list of the corresponding operand widths.
type family OperandTypes (fmt :: Format) :: [Nat] where
  OperandTypes R  = '[5, 5, 5]
  OperandTypes I  = '[5, 5, 12]
  OperandTypes S  = '[5, 5, 12]
  OperandTypes B  = '[5, 5, 12]
  OperandTypes U  = '[5, 20]
  OperandTypes J  = '[5, 20]
  OperandTypes H  = '[5, 5, 7]
  OperandTypes P  = '[]
  OperandTypes A  = '[5, 5, 5, 1, 1]
  OperandTypes R2 = '[5, 3, 5]
  OperandTypes R3 = '[5, 3, 5, 5]
  OperandTypes R4 = '[5, 3, 5, 5, 5]
  OperandTypes RX = '[5, 5]
  OperandTypes X  = '[32]

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
  OpBitsTypes R  = '[7, 3, 7]
  OpBitsTypes I  = '[7, 3]
  OpBitsTypes S  = '[7, 3]
  OpBitsTypes B  = '[7, 3]
  OpBitsTypes U  = '[7]
  OpBitsTypes J  = '[7]
  OpBitsTypes H  = '[7, 3, 5]
  OpBitsTypes P  = '[32]
  OpBitsTypes A  = '[7, 3, 5]
  OpBitsTypes R2 = '[7, 12]
  OpBitsTypes R3 = '[7, 7]
  OpBitsTypes R4 = '[7, 2]
  OpBitsTypes RX = '[7, 3, 12]
  OpBitsTypes X  = '[]

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

-- | RISC-V Opcodes, parameterized by 'RV' and format.
data Opcode :: RV -> Format -> * where

  -- RV32
  Add  :: Opcode rv R
  Sub  :: Opcode rv R
  Sll  :: Opcode rv R
  Slt  :: Opcode rv R
  Sltu :: Opcode rv R
  Xor  :: Opcode rv R
  Srl  :: Opcode rv R
  Sra  :: Opcode rv R
  Or   :: Opcode rv R
  And  :: Opcode rv R

  Jalr    :: Opcode rv I
  Lb      :: Opcode rv I
  Lh      :: Opcode rv I
  Lw      :: Opcode rv I
  Lbu     :: Opcode rv I
  Lhu     :: Opcode rv I
  Addi    :: Opcode rv I
  Slti    :: Opcode rv I
  Sltiu   :: Opcode rv I
  Xori    :: Opcode rv I
  Ori     :: Opcode rv I
  Andi    :: Opcode rv I
  Fence   :: Opcode rv I
  FenceI  :: Opcode rv I
  Csrrw   :: Opcode rv I
  Csrrs   :: Opcode rv I
  Csrrc   :: Opcode rv I
  Csrrwi  :: Opcode rv I
  Csrrsi  :: Opcode rv I
  Csrrci  :: Opcode rv I

  Slli    :: Opcode rv H
  Srli    :: Opcode rv H
  Srai    :: Opcode rv H

  Ecall   :: Opcode rv P
  Ebreak  :: Opcode rv P

  Sb :: Opcode rv S
  Sh :: Opcode rv S
  Sw :: Opcode rv S

  Beq  :: Opcode rv B
  Bne  :: Opcode rv B
  Blt  :: Opcode rv B
  Bge  :: Opcode rv B
  Bltu :: Opcode rv B
  Bgeu :: Opcode rv B

  Lui   :: Opcode rv U
  Auipc :: Opcode rv U

  Jal :: Opcode rv J

  Illegal :: Opcode rv X

  -- RV64
  Addw   :: 64 <= RVWidth rv => Opcode rv R
  Subw   :: 64 <= RVWidth rv => Opcode rv R
  Sllw   :: 64 <= RVWidth rv => Opcode rv R
  Srlw   :: 64 <= RVWidth rv => Opcode rv R
  Sraw   :: 64 <= RVWidth rv => Opcode rv R
  Slliw  :: 64 <= RVWidth rv => Opcode rv R
  Srliw  :: 64 <= RVWidth rv => Opcode rv R
  Sraiw  :: 64 <= RVWidth rv => Opcode rv R
  Lwu    :: 64 <= RVWidth rv => Opcode rv I
  Ld     :: 64 <= RVWidth rv => Opcode rv I
  Addiw  :: 64 <= RVWidth rv => Opcode rv I
  Sd     :: 64 <= RVWidth rv => Opcode rv S

  -- M privileged instructions
  Mret :: Opcode rv P
  Wfi  :: Opcode rv P

  -- RV32M
  Mul    :: MExt << rv => Opcode rv R
  Mulh   :: MExt << rv => Opcode rv R
  Mulhsu :: MExt << rv => Opcode rv R
  Mulhu  :: MExt << rv => Opcode rv R
  Div    :: MExt << rv => Opcode rv R
  Divu   :: MExt << rv => Opcode rv R
  Rem    :: MExt << rv => Opcode rv R
  Remu   :: MExt << rv => Opcode rv R

  -- RV64M
  Mulw   :: (64 <= RVWidth rv, MExt << rv) => Opcode rv R
  Divw   :: (64 <= RVWidth rv, MExt << rv) => Opcode rv R
  Divuw  :: (64 <= RVWidth rv, MExt << rv) => Opcode rv R
  Remw   :: (64 <= RVWidth rv, MExt << rv) => Opcode rv R
  Remuw  :: (64 <= RVWidth rv, MExt << rv) => Opcode rv R

  -- RV32A
  Lrw      :: AExt << rv => Opcode rv A
  Scw      :: AExt << rv => Opcode rv A
  Amoswapw :: AExt << rv => Opcode rv A
  Amoaddw  :: AExt << rv => Opcode rv A
  Amoxorw  :: AExt << rv => Opcode rv A
  Amoandw  :: AExt << rv => Opcode rv A
  Amoorw   :: AExt << rv => Opcode rv A
  Amominw  :: AExt << rv => Opcode rv A
  Amomaxw  :: AExt << rv => Opcode rv A
  Amominuw :: AExt << rv => Opcode rv A
  Amomaxuw :: AExt << rv => Opcode rv A

  -- RV64A
  Lrd      :: (64 <= RVWidth rv, AExt << rv) => Opcode rv A
  Scd      :: (64 <= RVWidth rv, AExt << rv) => Opcode rv A
  Amoswapd :: (64 <= RVWidth rv, AExt << rv) => Opcode rv A
  Amoaddd  :: (64 <= RVWidth rv, AExt << rv) => Opcode rv A
  Amoxord  :: (64 <= RVWidth rv, AExt << rv) => Opcode rv A
  Amoandd  :: (64 <= RVWidth rv, AExt << rv) => Opcode rv A
  Amoord   :: (64 <= RVWidth rv, AExt << rv) => Opcode rv A
  Amomind  :: (64 <= RVWidth rv, AExt << rv) => Opcode rv A
  Amomaxd  :: (64 <= RVWidth rv, AExt << rv) => Opcode rv A
  Amominud :: (64 <= RVWidth rv, AExt << rv) => Opcode rv A
  Amomaxud :: (64 <= RVWidth rv, AExt << rv) => Opcode rv A

  -- RV32F
  Flw       :: FExt << rv => Opcode rv I
  Fsw       :: FExt << rv => Opcode rv S
  Fmadd_s   :: FExt << rv => Opcode rv R4
  Fmsub_s   :: FExt << rv => Opcode rv R4
  Fnmsub_s  :: FExt << rv => Opcode rv R4
  Fnmadd_s  :: FExt << rv => Opcode rv R4
  Fadd_s    :: FExt << rv => Opcode rv R3
  Fsub_s    :: FExt << rv => Opcode rv R3
  Fmul_s    :: FExt << rv => Opcode rv R3
  Fdiv_s    :: FExt << rv => Opcode rv R3
  Fsqrt_s   :: FExt << rv => Opcode rv R2
  Fsgnj_s   :: FExt << rv => Opcode rv R
  Fsgnjn_s  :: FExt << rv => Opcode rv R
  Fsgnjx_s  :: FExt << rv => Opcode rv R
  Fmin_s    :: FExt << rv => Opcode rv R
  Fmax_s    :: FExt << rv => Opcode rv R
  Fcvt_w_s  :: FExt << rv => Opcode rv R2
  Fcvt_wu_s :: FExt << rv => Opcode rv R2
  Fmv_x_w   :: FExt << rv => Opcode rv RX
  Feq_s     :: FExt << rv => Opcode rv R
  Flt_s     :: FExt << rv => Opcode rv R
  Fle_s     :: FExt << rv => Opcode rv R
  Fclass_s  :: FExt << rv => Opcode rv RX
  Fcvt_s_w  :: FExt << rv => Opcode rv R2
  Fcvt_s_wu :: FExt << rv => Opcode rv R2
  Fmv_w_x   :: FExt << rv => Opcode rv RX

  -- RV64F
  Fcvt_l_s  :: (64 <= RVWidth rv, FExt << rv) => Opcode rv R2
  Fcvt_lu_s :: (64 <= RVWidth rv, FExt << rv) => Opcode rv R2
  Fcvt_s_l  :: (64 <= RVWidth rv, FExt << rv) => Opcode rv R2
  Fcvt_s_lu :: (64 <= RVWidth rv, FExt << rv) => Opcode rv R2

  -- RV32D
  Fld       :: DExt << rv => Opcode rv I
  Fsd       :: DExt << rv => Opcode rv S
  Fmadd_d   :: DExt << rv => Opcode rv R4
  Fmsub_d   :: DExt << rv => Opcode rv R4
  Fnmsub_d  :: DExt << rv => Opcode rv R4
  Fnmadd_d  :: DExt << rv => Opcode rv R4
  Fadd_d    :: DExt << rv => Opcode rv R3
  Fsub_d    :: DExt << rv => Opcode rv R3
  Fmul_d    :: DExt << rv => Opcode rv R3
  Fdiv_d    :: DExt << rv => Opcode rv R3
  Fsqrt_d   :: DExt << rv => Opcode rv R2
  Fsgnj_d   :: DExt << rv => Opcode rv R
  Fsgnjn_d  :: DExt << rv => Opcode rv R
  Fsgnjx_d  :: DExt << rv => Opcode rv R
  Fmin_d    :: DExt << rv => Opcode rv R
  Fmax_d    :: DExt << rv => Opcode rv R
  Fcvt_s_d  :: DExt << rv => Opcode rv R2
  Fcvt_d_s  :: DExt << rv => Opcode rv R2
  Feq_d     :: DExt << rv => Opcode rv R
  Flt_d     :: DExt << rv => Opcode rv R
  Fle_d     :: DExt << rv => Opcode rv R
  Fclass_d  :: DExt << rv => Opcode rv RX
  Fcvt_w_d  :: DExt << rv => Opcode rv R2
  Fcvt_wu_d :: DExt << rv => Opcode rv R2
  Fcvt_d_w  :: DExt << rv => Opcode rv R2
  Fcvt_d_wu :: DExt << rv => Opcode rv R2

  -- RV64D
  Fcvt_l_d  :: (64 <= RVWidth rv, DExt << rv) => Opcode rv R2
  Fcvt_lu_d :: (64 <= RVWidth rv, DExt << rv) => Opcode rv R2
  Fmv_x_d   :: (64 <= RVWidth rv, DExt << rv) => Opcode rv RX
  Fcvt_d_l  :: (64 <= RVWidth rv, DExt << rv) => Opcode rv R2
  Fcvt_d_lu :: (64 <= RVWidth rv, DExt << rv) => Opcode rv R2
  Fmv_d_x   :: (64 <= RVWidth rv, DExt << rv) => Opcode rv RX

-- Instances
$(return [])
deriving instance Show (Opcode rv fmt)
instance ShowF (Opcode rv)
deriving instance Eq (Opcode rv fmt)
instance EqF (Opcode rv) where
  eqF = (==)
instance TestEquality (Opcode rv) where
  testEquality = $(structuralTypeEquality [t|Opcode|] [])
instance OrdF (Opcode rv) where
  compareF = $(structuralTypeOrd [t|Opcode|] [])

----------------------------------------
-- Instructions

-- | RISC-V Instruction, parameterized by base architecture and format.
data Instruction (rv :: RV) (fmt :: Format) =
  Inst (Opcode rv fmt) (Operands fmt)

-- Instances
$(return [])
instance Show (Instruction rv fmt) where
  show (Inst opcode operands) = show opcode ++ " " ++ show operands
instance ShowF (Instruction rv)
