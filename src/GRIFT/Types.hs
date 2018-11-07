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
Module      : GRIFT.Types
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

module GRIFT.Types
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
  , withRVWidth
  , withRVFloatWidth
    -- ** Common RISC-V Configurations
    -- | Provided for convenience.
  , RV32I
  , RV32IM
  , RV32IMA
  , RV32IMAF
  , RV32G
  , RV32GC
  , RV64I
  , RV64IM
  , RV64IMA
  , RV64IMAF
  , RV64G
  , RV64GC
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
  , CConfig(..), type CYes, type CNo
  , ExtensionsRepr(..)
  , PrivConfigRepr(..)
  , MConfigRepr(..)
  , AConfigRepr(..)
  , FDConfigRepr(..)
  , CConfigRepr(..)
  , KnownExtensions
  , Extension(..), type AExt, type DExt, type FExt, type MExt, type CExt, type SExt, type UExt
  , ExtensionsContains
  -- * Instructions
  , Format(..)
  , type R, type I, type S, type B, type U, type J
  , type H, type P, type A, type R2, type R3, type R4
  , type RX, type X
  , FormatRepr(..)
  , OperandTypes
  , OperandName(..)
  , OperandID(..)
  , Operands(..)
  , OpBitsTypes
  , OpBits(..)
  , Opcode(..)
  , Instruction(..)
  , mkInst
  , opcodeCast
  , readOpcode
  -- * Utils
  , PrettyF(..)
  ) where

import Control.Monad
import Data.BitVector.Sized
import Data.Char
import Data.Parameterized
import Data.Parameterized.List
import Data.Parameterized.TH.GADT
import GHC.TypeLits
import Numeric
import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJClass

class PrettyF (a :: k -> *) where
  pPrintF :: a tp -> Doc

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
data Extensions = Exts (PrivConfig, MConfig, AConfig, FDConfig, CConfig)

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

-- | The C extension is either enabled or disabled.
data CConfig = CYes | CNo

type CYes = 'CYes
type CNo = 'CNo

-- | A runtime representative for 'Extensions' for dependent typing.
data ExtensionsRepr :: Extensions -> * where
  ExtensionsRepr :: PrivConfigRepr priv
                 -> MConfigRepr m
                 -> AConfigRepr a
                 -> FDConfigRepr fd
                 -> CConfigRepr c
                 -> ExtensionsRepr (Exts '(priv, m, a, fd, c))

instance ( KnownRepr PrivConfigRepr priv
         , KnownRepr MConfigRepr m
         , KnownRepr AConfigRepr a
         , KnownRepr FDConfigRepr fd
         , KnownRepr CConfigRepr c
         ) => KnownRepr ExtensionsRepr (Exts '(priv, m, a, fd, c)) where
  knownRepr = ExtensionsRepr knownRepr knownRepr knownRepr knownRepr knownRepr

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

-- | A runtime representative for 'CConfig' for dependent typing.
data CConfigRepr :: CConfig -> * where
  CYesRepr :: CConfigRepr CYes
  CNoRepr  :: CConfigRepr CNo

instance KnownRepr CConfigRepr CYes where knownRepr = CYesRepr
instance KnownRepr CConfigRepr CNo  where knownRepr = CNoRepr

-- | Everything we need to know about an 'Extensions' at compile time.
type KnownExtensions exts = KnownRepr ExtensionsRepr exts

-- | Type-level representation of a RISC-V extension.
data Extension = AExt | DExt | FExt | MExt | CExt | SExt | UExt

type AExt = 'MExt
type DExt = 'DExt
type FExt = 'FExt
type MExt = 'MExt
type CExt = 'CExt
type SExt = 'SExt
type UExt = 'UExt

-- | Type operator that determines whether the 'Extensions' contains a particular
-- 'Extension'.
type family ExtensionsContains (exts :: Extensions) (e :: Extension) :: Bool where
  ExtensionsContains (Exts '(_, MYes, _, _, _))       MExt = 'True
  ExtensionsContains (Exts '(_,    _, AYes, _, _))    AExt = 'True
  ExtensionsContains (Exts '(_,    _, _, FDYes, _))   FExt = 'True
  ExtensionsContains (Exts '(_,    _, _, FYesDNo, _)) FExt = 'True
  ExtensionsContains (Exts '(_,    _, _, FDYes, _))   DExt = 'True
  ExtensionsContains (Exts '(PrivMU,  _, _, _, _))    UExt = 'True
  ExtensionsContains (Exts '(PrivMSU, _, _, _, _))    UExt = 'True
  ExtensionsContains (Exts '(PrivMSU, _, _, _, _))    SExt = 'True
  ExtensionsContains (Exts '(_, _, _, _, CYes))       CExt = 'True
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

-- | Maps a RISC-V configuration to its register width.
type family RVWidth (rv :: RV) :: Nat where
  RVWidth (RVConfig '(arch, _)) = ArchWidth arch

-- | Maps a 'FDConfig' to its corresponding floating point register width.
type family FDFloatWidth (fd :: FDConfig) :: Nat where
  FDFloatWidth FDYes = 64
  FDFloatWidth _ = 32

-- | Maps a RISC-V configuration to its floating point register width.
type family RVFloatWidth (rv :: RV) :: Nat where
  RVFloatWidth rv = FDFloatWidth (RVFloatType rv)

-- | Maps a RISC-V configuration to its 'FDConfig'.
type family RVFloatType (rv :: RV) :: FDConfig where
  RVFloatType (RVConfig '(_, Exts '(_, _, _, fd, _))) = fd

-- | 'ExtensionsContains' in constraint form.
type family (<<) (e :: Extension) (rv :: RV) where
  e << RVConfig '(_, exts)= ExtensionsContains exts e ~ 'True

-- | Satisfy a 'KnownRVWidth' constraint from an explicit 'RVRepr'.
withRVWidth :: RVRepr rv -> (KnownRVWidth rv => b) -> b
withRVWidth (RVRepr RV32Repr _) b = b
withRVWidth (RVRepr RV64Repr _) b = b
withRVWidth (RVRepr RV128Repr _) b = b

-- | Satisfy a 'KnownRVFloatWidth' constraint from an explicit 'RVRepr'.
withRVFloatWidth :: RVRepr rv -> (KnownRVFloatWidth rv => b) -> b
withRVFloatWidth (RVRepr _ (ExtensionsRepr _ _ _ FDYesRepr _)) b = b
withRVFloatWidth (RVRepr _ (ExtensionsRepr _ _ _ FYesDNoRepr _)) b = b
withRVFloatWidth (RVRepr _ (ExtensionsRepr _ _ _ FDNoRepr _)) b = b

-- type synonyms for common RVConfigs.
type RV32I     = RVConfig '(RV32, Exts '(PrivM, MNo,  ANo,  FDNo,    CNo))
type RV32IM    = RVConfig '(RV32, Exts '(PrivM, MYes, ANo,  FDNo,    CNo))
type RV32IMA   = RVConfig '(RV32, Exts '(PrivM, MYes, AYes, FDNo,    CNo))
type RV32IMAF  = RVConfig '(RV32, Exts '(PrivM, MYes, AYes, FYesDNo, CNo))
type RV32G     = RVConfig '(RV32, Exts '(PrivM, MYes, AYes, FDYes,   CNo))
type RV32GC    = RVConfig '(RV32, Exts '(PrivM, MYes, AYes, FDYes,   CYes))
type RV64I     = RVConfig '(RV64, Exts '(PrivM, MNo,  ANo,  FDNo,    CNo))
type RV64IM    = RVConfig '(RV64, Exts '(PrivM, MYes, ANo,  FDNo,    CNo))
type RV64IMA   = RVConfig '(RV64, Exts '(PrivM, MYes, AYes, FDNo,    CNo))
type RV64IMAF  = RVConfig '(RV64, Exts '(PrivM, MYes, AYes, FYesDNo, CNo))
type RV64G     = RVConfig '(RV64, Exts '(PrivM, MYes, AYes, FDYes,   CNo))
type RV64GC    = RVConfig '(RV64, Exts '(PrivM, MYes, AYes, FDYes,   CYes))

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

data OperandName :: Nat -> * where
  Aq     :: OperandName 1
  Rl     :: OperandName 1
  Rm     :: OperandName 3
  Rd     :: OperandName 5
  Rs1    :: OperandName 5
  Rs2    :: OperandName 5
  Rs3    :: OperandName 5
  Imm5   :: OperandName 5
  Shamt5 :: OperandName 5
  Shamt7 :: OperandName 7
  Imm12  :: OperandName 12
  Csr    :: OperandName 12
  Imm20  :: OperandName 20
  Imm32  :: OperandName 32

-- | An 'OperandID is just an index into a particular format's 'OperandTypes' list.
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

prettyReg :: BitVector 5 -> Doc
prettyReg bv = text "x" <> integer (bvIntegerU bv)

prettyImm :: BitVector w -> Doc
prettyImm bv = text $ "0x" ++ showHex (bvIntegerS bv) ""

_prettyAddr :: BitVector w -> BitVector 5 -> Doc
_prettyAddr offset reg = prettyImm offset <> parens (prettyReg reg)

commas :: [Doc] -> Doc
commas = hcat . punctuate (comma <> space)

-- TODO: Change pretty printing for addresses to use offset(reg). This involves
-- dealing with individual instructions (yuck).
instance PrettyF Operands where
  pPrintF (Operands RRepr (rd :< rs1 :< rs2 :< Nil)) =
    commas [prettyReg rd, prettyReg rs1, prettyReg rs2]
  pPrintF (Operands IRepr (rd :< rs1 :< imm :< Nil)) =
    commas [prettyReg rd, prettyReg rs1, prettyImm imm]
  pPrintF (Operands SRepr (rs1 :< rs2 :< imm :< Nil)) =
    commas [prettyReg rs2, prettyImm imm, parens (prettyReg rs1)]
  pPrintF (Operands BRepr (rs1 :< rs2 :< imm :< Nil)) =
    commas [prettyReg rs1, prettyReg rs2, comma <+> prettyImm imm]
  pPrintF (Operands URepr (rd :< imm :< Nil)) =
    commas [prettyReg rd, prettyImm imm]
  pPrintF (Operands JRepr (rd :< imm :< Nil)) =
    commas [prettyReg rd, prettyImm imm]
  pPrintF (Operands HRepr (rd :< rs1 :< imm :< Nil)) =
    commas [prettyReg rd, prettyReg rs1, prettyImm imm]
  pPrintF (Operands PRepr _) = empty
  pPrintF (Operands ARepr (rd :< rs1 :< rs2 :< _rl :< _aq :< Nil)) =
    commas [prettyReg rd, prettyReg rs2, parens (prettyReg rs1)]
  pPrintF (Operands R2Repr (rd :< _rm :< rs1 :< Nil)) =
    commas [prettyReg rd, prettyReg rs1]
  pPrintF (Operands R3Repr (rd :< _rm :< rs1 :< rs2 :< Nil)) =
    commas [prettyReg rd, prettyReg rs1, prettyReg rs2]
  pPrintF (Operands R4Repr (rd :< _rm :< rs1 :< rs2 :< rs3 :< Nil)) =
    commas [prettyReg rd, prettyReg rs1, prettyReg rs2, prettyReg rs3]
  pPrintF (Operands RXRepr (rd :< rs1 :< Nil)) =
    commas [prettyReg rd, prettyReg rs1]
  pPrintF (Operands XRepr (ill :< Nil)) = text (show ill)

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

withRV64 :: RVRepr rv -> ((64 <= RVWidth rv) => a) -> Maybe a
withRV64 (RVRepr RV64Repr _) a = Just a
withRV64 (RVRepr RV128Repr _) a = Just a
withRV64 _ _ = Nothing

withM :: RVRepr rv -> ((MExt << rv) => a) -> Maybe a
withM (RVRepr _ (ExtensionsRepr _ MYesRepr _ _ _)) a = Just a
withM _ _ = Nothing

withA :: RVRepr rv -> ((AExt << rv) => a) -> Maybe a
withA (RVRepr _ (ExtensionsRepr _ _ AYesRepr _ _)) a = Just a
withA _ _ = Nothing

withF :: RVRepr rv -> ((FExt << rv) => a) -> Maybe a
withF (RVRepr _ (ExtensionsRepr _ _ _ FYesDNoRepr _)) a = Just a
withF (RVRepr _ (ExtensionsRepr _ _ _ FDYesRepr _)) a = Just a
withF _ _ = Nothing

withD :: RVRepr rv -> ((DExt << rv) => a) -> Maybe a
withD (RVRepr _ (ExtensionsRepr _ _ _ FDYesRepr _)) a = Just a
withD _ _ = Nothing

-- TODO: Create invertible parser for instructions.
readOpcode :: String -> Maybe (Some (Opcode RV64GC))
readOpcode str = case (toLower <$> str) of
  "add" -> Just (Some Add)
  "sub" -> Just (Some Sub)
  "sll" -> Just (Some Sll)
  "slt" -> Just (Some Slt)
  "sltu" -> Just (Some Sltu)
  "xor" -> Just (Some Xor)
  "srl" -> Just (Some Srl)
  "sra" -> Just (Some Sra)
  "or" -> Just (Some Or)
  "and" -> Just (Some And)

  "jalr" -> Just (Some Jalr)
  "lb" -> Just (Some Lb)
  "lh" -> Just (Some Lh)
  "lw" -> Just (Some Lw)
  "lbu" -> Just (Some Lbu)
  "lhu" -> Just (Some Lhu)
  "addi" -> Just (Some Addi)
  "slti" -> Just (Some Slti)
  "sltiu" -> Just (Some Sltiu)
  "xori" -> Just (Some Xori)
  "ori" -> Just (Some Ori)
  "andi" -> Just (Some Andi)
  "fence" -> Just (Some Fence)
  "fencei" -> Just (Some FenceI)
  "csrrw" -> Just (Some Csrrw)
  "csrrs" -> Just (Some Csrrs)
  "csrrc" -> Just (Some Csrrc)
  "csrrwi" -> Just (Some Csrrwi)
  "csrrsi" -> Just (Some Csrrsi)
  "csrrci" -> Just (Some Csrrci)

  "slli" -> Just (Some Slli)
  "srli" -> Just (Some Srli)
  "srai" -> Just (Some Srai)

  "ecall" -> Just (Some Ecall)
  "ebreak" -> Just (Some Ebreak)

  "sb" -> Just (Some Sb)
  "sh" -> Just (Some Sh)
  "sw" -> Just (Some Sw)
  "beq" -> Just (Some Beq)
  "bne" -> Just (Some Bne)
  "blt" -> Just (Some Blt)
  "bge" -> Just (Some Bge)
  "bltu" -> Just (Some Bltu)
  "bgeu" -> Just (Some Bgeu)

  "lui" -> Just (Some Lui)
  "auipc" -> Just (Some Auipc)

  "jal" -> Just (Some Jal)

  "illegal" -> Just (Some Illegal)

  "addw" -> Just (Some Addw)
  "subw" -> Just (Some Subw)
  "sllw" -> Just (Some Sllw)
  "srlw" -> Just (Some Srlw)
  "sraw" -> Just (Some Sraw)
  "slliw" -> Just (Some Slliw)
  "srliw" -> Just (Some Srliw)
  "sraiw" -> Just (Some Sraiw)
  "lwu" -> Just (Some Lwu)
  "ld" -> Just (Some Ld)
  "addiw" -> Just (Some Addiw)
  "sd" -> Just (Some Sd)

  "mret" -> Just (Some Mret)
  "wfi" -> Just (Some Wfi)

  "mul" -> Just (Some Mul)
  "mulh" -> Just (Some Mulh)
  "mulhsu" -> Just (Some Mulhsu)
  "mulhu" -> Just (Some Mulhu)
  "div" -> Just (Some Div)
  "divu" -> Just (Some Divu)
  "rem" -> Just (Some Rem)
  "remu" -> Just (Some Remu)

  "mulw" -> Just (Some Mulw)
  "divw" -> Just (Some Divw)
  "divuw" -> Just (Some Divuw)
  "remw" -> Just (Some Remw)
  "remuw" -> Just (Some Remuw)

  "lrw" -> Just (Some Lrw)
  "scw" -> Just (Some Scw)
  "amoswapw" -> Just (Some Amoswapw)
  "amoaddw" -> Just (Some Amoaddw)
  "amoxorw" -> Just (Some Amoxorw)
  "amoandw" -> Just (Some Amoandw)
  "amoorw" -> Just (Some Amoorw)
  "amominw" -> Just (Some Amominw)
  "amomaxw" -> Just (Some Amomaxw)
  "amominuw" -> Just (Some Amominuw)
  "amomaxuw" -> Just (Some Amomaxuw)

  "lrd" -> Just (Some Lrd)
  "scd" -> Just (Some Scd)
  "amoswapd" -> Just (Some Amoswapd)
  "amoaddd" -> Just (Some Amoaddd)
  "amoxord" -> Just (Some Amoxord)
  "amoandd" -> Just (Some Amoandd)
  "amoord" -> Just (Some Amoord)
  "amomind" -> Just (Some Amomind)
  "amomaxd" -> Just (Some Amomaxd)
  "amominud" -> Just (Some Amominud)
  "amomaxud" -> Just (Some Amomaxud)

  -- RV32F
  "flw" -> Just (Some Flw)
  "fsw" -> Just (Some Fsw)
  "fmadd_s" -> Just (Some Fmadd_s)
  "fmsub_s" -> Just (Some Fmsub_s)
  "fnmsub_s" -> Just (Some Fnmsub_s)
  "fnmadd_s" -> Just (Some Fnmadd_s)
  "fadd_s" -> Just (Some Fadd_s)
  "fsub_s" -> Just (Some Fsub_s)
  "fmul_s" -> Just (Some Fmul_s)
  "fdiv_s" -> Just (Some Fdiv_s)
  "fsqrt_s" -> Just (Some Fsqrt_s)
  "fsgnj_s" -> Just (Some Fsgnj_s)
  "fsgnjn_s" -> Just (Some Fsgnjn_s)
  "fsgnjx_s" -> Just (Some Fsgnjx_s)
  "fmin_s" -> Just (Some Fmin_s)
  "fmax_s" -> Just (Some Fmax_s)
  "fcvt_w_s" -> Just (Some Fcvt_w_s)
  "fcvt_wu_s" -> Just (Some Fcvt_wu_s)
  "fmv_x_w" -> Just (Some Fmv_x_w)
  "feq_s" -> Just (Some Feq_s)
  "flt_s" -> Just (Some Flt_s)
  "fle_s" -> Just (Some Fle_s)
  "fclass_s" -> Just (Some Fclass_s)
  "fcvt_s_w" -> Just (Some Fcvt_s_w)
  "fcvt_s_wu" -> Just (Some Fcvt_s_wu)
  "fmv_w_x" -> Just (Some Fmv_w_x)

  "fcvt_l_s" -> Just (Some Fcvt_l_s)
  "fcvt_lu_s" -> Just (Some Fcvt_lu_s)
  "fcvt_s_l" -> Just (Some Fcvt_s_l)
  "fcvt_s_lu" -> Just (Some Fcvt_s_lu)

  "fld" -> Just (Some Fld)
  "fsd" -> Just (Some Fsd)
  "fmadd_d" -> Just (Some Fmadd_d)
  "fmsub_d" -> Just (Some Fmsub_d)
  "fnmsub_d" -> Just (Some Fnmsub_d)
  "fnmadd_d" -> Just (Some Fnmadd_d)
  "fadd_d" -> Just (Some Fadd_d)
  "fsub_d" -> Just (Some Fsub_d)
  "fmul_d" -> Just (Some Fmul_d)
  "fdiv_d" -> Just (Some Fdiv_d)
  "fsqrt_d" -> Just (Some Fsqrt_d)
  "fsgnj_d" -> Just (Some Fsgnj_d)
  "fsgnjn_d" -> Just (Some Fsgnjn_d)
  "fsgnjx_d" -> Just (Some Fsgnjx_d)
  "fmin_d" -> Just (Some Fmin_d)
  "fmax_d" -> Just (Some Fmax_d)
  "fcvt_s_d" -> Just (Some Fcvt_s_d)
  "fcvt_d_s" -> Just (Some Fcvt_d_s)
  "feq_d" -> Just (Some Feq_d)
  "flt_d" -> Just (Some Flt_d)
  "fle_d" -> Just (Some Fle_d)
  "fclass_d" -> Just (Some Fclass_d)
  "fcvt_w_d" -> Just (Some Fcvt_w_d)
  "fcvt_wu_d" -> Just (Some Fcvt_wu_d)
  "fcvt_d_w" -> Just (Some Fcvt_d_w)
  "fcvt_d_wu" -> Just (Some Fcvt_d_wu)

  "fcvt_l_d" -> Just (Some Fcvt_l_d)
  "fcvt_lu_d" -> Just (Some Fcvt_lu_d)
  "fmv_x_d" -> Just (Some Fmv_x_d)
  "fcvt_d_l" -> Just (Some Fcvt_d_l)
  "fcvt_d_lu" -> Just (Some Fcvt_d_lu)
  "fmv_d_x" -> Just (Some Fmv_d_x)

  _ -> Nothing

opcodeCast :: RVRepr rv -> Opcode rv' fmt -> Maybe (Opcode rv fmt)
opcodeCast _ Add = Just Add
opcodeCast _ Sub = Just Sub
opcodeCast _ Sll = Just Sll
opcodeCast _ Slt = Just Slt
opcodeCast _ Sltu = Just Sltu
opcodeCast _ Xor = Just Xor
opcodeCast _ Srl = Just Srl
opcodeCast _ Sra = Just Sra
opcodeCast _ Or  = Just Or
opcodeCast _ And = Just And
opcodeCast _ Jalr   = Just Jalr
opcodeCast _ Lb     = Just Lb
opcodeCast _ Lh     = Just Lh
opcodeCast _ Lw     = Just Lw
opcodeCast _ Lbu    = Just Lbu
opcodeCast _ Lhu    = Just Lhu
opcodeCast _ Addi   = Just Addi
opcodeCast _ Slti   = Just Slti
opcodeCast _ Sltiu  = Just Sltiu
opcodeCast _ Xori   = Just Xori
opcodeCast _ Ori    = Just Ori
opcodeCast _ Andi   = Just Andi
opcodeCast _ Fence  = Just Fence
opcodeCast _ FenceI = Just FenceI
opcodeCast _ Csrrw  = Just Csrrw
opcodeCast _ Csrrs  = Just Csrrs
opcodeCast _ Csrrc  = Just Csrrc
opcodeCast _ Csrrwi = Just Csrrwi
opcodeCast _ Csrrsi = Just Csrrsi
opcodeCast _ Csrrci = Just Csrrci
opcodeCast _ Slli   = Just Slli
opcodeCast _ Srli   = Just Srli
opcodeCast _ Srai   = Just Srai
opcodeCast _ Ecall  = Just Ecall
opcodeCast _ Ebreak = Just Ebreak
opcodeCast _ Sb = Just Sb
opcodeCast _ Sh = Just Sh
opcodeCast _ Sw = Just Sw
opcodeCast _ Beq = Just Beq
opcodeCast _ Bne = Just Bne
opcodeCast _ Blt = Just Blt
opcodeCast _ Bge = Just Bge
opcodeCast _ Bltu = Just Bltu
opcodeCast _ Bgeu = Just Bgeu
opcodeCast _ Lui  = Just Lui
opcodeCast _ Auipc = Just Auipc
opcodeCast _ Jal = Just Jal
opcodeCast _ Illegal = Just Illegal
opcodeCast _ Mret = Just Mret
opcodeCast _ Wfi = Just Wfi

opcodeCast rv Addw = withRV64 rv Addw
opcodeCast rv Subw = withRV64 rv Subw
opcodeCast rv Sllw = withRV64 rv Sllw
opcodeCast rv Srlw = withRV64 rv Srlw
opcodeCast rv Sraw = withRV64 rv Sraw
opcodeCast rv Slliw = withRV64 rv Slliw
opcodeCast rv Srliw = withRV64 rv Srliw
opcodeCast rv Sraiw = withRV64 rv Sraiw
opcodeCast rv Lwu = withRV64 rv Lwu
opcodeCast rv Ld = withRV64 rv Ld
opcodeCast rv Addiw = withRV64 rv Addiw
opcodeCast rv Sd = withRV64 rv Sd

opcodeCast rv Mul = withM rv Mul
opcodeCast rv Mulh = withM rv Mulh
opcodeCast rv Mulhsu = withM rv Mulhsu
opcodeCast rv Mulhu = withM rv Mulhu
opcodeCast rv Div = withM rv Div
opcodeCast rv Divu = withM rv Divu
opcodeCast rv Rem = withM rv Rem
opcodeCast rv Remu = withM rv Remu

opcodeCast rv Mulw = join (withRV64 rv (withM rv Mulw))
opcodeCast rv Divw = join (withRV64 rv (withM rv Divw))
opcodeCast rv Divuw = join (withRV64 rv (withM rv Divuw))
opcodeCast rv Remw = join (withRV64 rv (withM rv Remw))
opcodeCast rv Remuw = join (withRV64 rv (withM rv Remuw))

opcodeCast rv Lrw = withA rv Lrw
opcodeCast rv Scw = withA rv Scw
opcodeCast rv Amoswapw = withA rv Amoswapw
opcodeCast rv Amoaddw = withA rv Amoaddw
opcodeCast rv Amoxorw = withA rv Amoxorw
opcodeCast rv Amoandw = withA rv Amoandw
opcodeCast rv Amoorw = withA rv Amoorw
opcodeCast rv Amominw = withA rv Amominw
opcodeCast rv Amomaxw = withA rv Amomaxw
opcodeCast rv Amominuw = withA rv Amominuw
opcodeCast rv Amomaxuw = withA rv Amomaxuw

opcodeCast rv Lrd = join (withRV64 rv (withA rv Lrd))
opcodeCast rv Scd = join (withRV64 rv (withA rv Scd))
opcodeCast rv Amoswapd = join (withRV64 rv (withA rv Amoswapd))
opcodeCast rv Amoaddd = join (withRV64 rv (withA rv Amoaddd))
opcodeCast rv Amoxord = join (withRV64 rv (withA rv Amoxord))
opcodeCast rv Amoandd = join (withRV64 rv (withA rv Amoandd))
opcodeCast rv Amoord = join (withRV64 rv (withA rv Amoord))
opcodeCast rv Amomind = join (withRV64 rv (withA rv Amomind))
opcodeCast rv Amomaxd = join (withRV64 rv (withA rv Amomaxd))
opcodeCast rv Amominud = join (withRV64 rv (withA rv Amominud))
opcodeCast rv Amomaxud = join (withRV64 rv (withA rv Amomaxud))

opcodeCast rv Flw = withF rv Flw
opcodeCast rv Fsw = withF rv Fsw
opcodeCast rv Fmadd_s = withF rv Fmadd_s
opcodeCast rv Fmsub_s = withF rv Fmsub_s
opcodeCast rv Fnmsub_s = withF rv Fnmsub_s
opcodeCast rv Fnmadd_s = withF rv Fnmadd_s
opcodeCast rv Fadd_s = withF rv Fadd_s
opcodeCast rv Fsub_s = withF rv Fsub_s
opcodeCast rv Fmul_s = withF rv Fmul_s
opcodeCast rv Fdiv_s = withF rv Fdiv_s
opcodeCast rv Fsqrt_s = withF rv Fsqrt_s
opcodeCast rv Fsgnj_s = withF rv Fsgnj_s
opcodeCast rv Fsgnjn_s = withF rv Fsgnjn_s
opcodeCast rv Fsgnjx_s = withF rv Fsgnjx_s
opcodeCast rv Fmin_s = withF rv Fmin_s
opcodeCast rv Fmax_s = withF rv Fmax_s
opcodeCast rv Fcvt_w_s = withF rv Fcvt_w_s
opcodeCast rv Fcvt_wu_s = withF rv Fcvt_wu_s
opcodeCast rv Fmv_x_w = withF rv Fmv_x_w
opcodeCast rv Feq_s = withF rv Feq_s
opcodeCast rv Flt_s = withF rv Flt_s
opcodeCast rv Fle_s = withF rv Fle_s
opcodeCast rv Fclass_s = withF rv Fclass_s
opcodeCast rv Fcvt_s_w = withF rv Fcvt_s_w
opcodeCast rv Fcvt_s_wu = withF rv Fcvt_s_wu
opcodeCast rv Fmv_w_x = withF rv Fmv_w_x

opcodeCast rv Fcvt_l_s = join (withRV64 rv (withF rv Fcvt_l_s))
opcodeCast rv Fcvt_lu_s = join (withRV64 rv (withF rv Fcvt_lu_s))
opcodeCast rv Fcvt_s_l = join (withRV64 rv (withF rv Fcvt_s_l))
opcodeCast rv Fcvt_s_lu = join (withRV64 rv (withF rv Fcvt_s_lu))

opcodeCast rv Fld = withD rv Fld
opcodeCast rv Fsd = withD rv Fsd
opcodeCast rv Fmadd_d = withD rv Fmadd_d
opcodeCast rv Fmsub_d = withD rv Fmsub_d
opcodeCast rv Fnmsub_d = withD rv Fnmsub_d
opcodeCast rv Fnmadd_d = withD rv Fnmadd_d
opcodeCast rv Fadd_d = withD rv Fadd_d
opcodeCast rv Fsub_d = withD rv Fsub_d
opcodeCast rv Fmul_d = withD rv Fmul_d
opcodeCast rv Fdiv_d = withD rv Fdiv_d
opcodeCast rv Fsqrt_d = withD rv Fsqrt_d
opcodeCast rv Fsgnj_d = withD rv Fsgnj_d
opcodeCast rv Fsgnjn_d = withD rv Fsgnjn_d
opcodeCast rv Fsgnjx_d = withD rv Fsgnjx_d
opcodeCast rv Fmin_d = withD rv Fmin_d
opcodeCast rv Fmax_d = withD rv Fmax_d
opcodeCast rv Fcvt_s_d = withD rv Fcvt_s_d
opcodeCast rv Fcvt_d_s = withD rv Fcvt_d_s
opcodeCast rv Feq_d = withD rv Feq_d
opcodeCast rv Flt_d = withD rv Flt_d
opcodeCast rv Fle_d = withD rv Fle_d
opcodeCast rv Fclass_d = withD rv Fclass_d
opcodeCast rv Fcvt_w_d = withD rv Fcvt_w_d
opcodeCast rv Fcvt_wu_d = withD rv Fcvt_wu_d
opcodeCast rv Fcvt_d_w = withD rv Fcvt_d_w
opcodeCast rv Fcvt_d_wu = withD rv Fcvt_d_wu

opcodeCast rv Fcvt_l_d = join (withRV64 rv (withD rv Fcvt_l_d))
opcodeCast rv Fcvt_lu_d = join (withRV64 rv (withD rv Fcvt_lu_d))
opcodeCast rv Fmv_x_d = join (withRV64 rv (withD rv Fmv_x_d))
opcodeCast rv Fcvt_d_l = join (withRV64 rv (withD rv Fcvt_d_l))
opcodeCast rv Fcvt_d_lu = join (withRV64 rv (withD rv Fcvt_d_lu))
opcodeCast rv Fmv_d_x = join (withRV64 rv (withD rv Fmv_d_x))

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

instance Pretty (Opcode rv fmt) where

  pPrint Add  = text "add"
  pPrint Sub  = text "sub"
  pPrint Sll  = text "sll"
  pPrint Slt  = text "slt"
  pPrint Sltu = text "sltu"
  pPrint Xor  = text "xor"
  pPrint Srl  = text "srl"
  pPrint Sra  = text "sra"
  pPrint Or   = text "or"
  pPrint And  = text "and"

  pPrint Jalr   = text "jalr"
  pPrint Lb     = text "lb"
  pPrint Lh     = text "lh"
  pPrint Lw     = text "lw"
  pPrint Lbu    = text "lbu"
  pPrint Lhu    = text "lhu"
  pPrint Addi   = text "addi"
  pPrint Slti   = text "slti"
  pPrint Sltiu  = text "sltiu"
  pPrint Xori   = text "xori"
  pPrint Ori    = text "ori"
  pPrint Andi   = text "andi"
  pPrint Fence  = text "fence"
  pPrint FenceI = text "fencei"
  pPrint Csrrw  = text "csrrw"
  pPrint Csrrs  = text "csrrs"
  pPrint Csrrc  = text "csrrc"
  pPrint Csrrwi = text "csrrwi"
  pPrint Csrrsi = text "csrrsi"
  pPrint Csrrci = text "csrrci"

  pPrint Slli   = text "slli"
  pPrint Srli   = text "srli"
  pPrint Srai   = text "srai"

  pPrint Ecall  = text "ecall"
  pPrint Ebreak = text "ebreak"

  pPrint Sb     = text "sb"
  pPrint Sh     = text "sh"
  pPrint Sw     = text "sw"

  pPrint Beq    = text "beq"
  pPrint Bne    = text "bne"
  pPrint Blt    = text "blt"
  pPrint Bge    = text "bge"
  pPrint Bltu = text "bltu"
  pPrint Bgeu   = text "bgeu"

  pPrint Lui   = text "lui"
  pPrint Auipc = text "auipc"

  pPrint Jal = text "jal"

  pPrint Illegal = text "illegal"

  -- RV64
  pPrint Addw = text "addw"
  pPrint Subw = text "subw"
  pPrint Sllw = text "sllw"
  pPrint Srlw = text "srlw"
  pPrint Sraw = text "sraw"
  pPrint Slliw = text "slliw"
  pPrint Srliw = text "srliw"
  pPrint Sraiw = text "sraiw"
  pPrint Lwu = text "lwu"
  pPrint Ld = text "ld"
  pPrint Addiw = text "addiw"
  pPrint Sd = text "sd"

  -- M privileged instructions
  pPrint Mret = text "mret"
  pPrint Wfi = text "wfi"

  -- RV32M
  pPrint Mul = text "mul"
  pPrint Mulh = text "mulh"
  pPrint Mulhsu = text "mulhsu"
  pPrint Mulhu = text "mulhu"
  pPrint Div = text "div"
  pPrint Divu = text "divu"
  pPrint Rem = text "rem"
  pPrint Remu = text "remu"

  -- RV64M
  pPrint Mulw = text "mulw"
  pPrint Divw = text "divw"
  pPrint Divuw = text "divuw"
  pPrint Remw = text "remw"
  pPrint Remuw = text "remuw"

  -- RV32A
  pPrint Lrw = text "lrw"
  pPrint Scw = text "scw"
  pPrint Amoswapw = text "amoswapw"
  pPrint Amoaddw = text "amoaddw"
  pPrint Amoxorw = text "amoxorw"
  pPrint Amoandw = text "amoandw"
  pPrint Amoorw = text "amoorw"
  pPrint Amominw = text "amominw"
  pPrint Amomaxw = text "amomaxw"
  pPrint Amominuw = text "amominuw"
  pPrint Amomaxuw = text "amomaxuw"

  -- RV64A
  pPrint Lrd = text "lrd"
  pPrint Scd = text "scd"
  pPrint Amoswapd = text "amoswapd"
  pPrint Amoaddd = text "amoaddd"
  pPrint Amoxord = text "amoxord"
  pPrint Amoandd = text "amoandd"
  pPrint Amoord = text "amoord"
  pPrint Amomind = text "amomind"
  pPrint Amomaxd = text "amomaxd"
  pPrint Amominud = text "amominud"
  pPrint Amomaxud = text "amomaxud"

  -- RV32F
  pPrint Flw = text "flw"
  pPrint Fsw = text "fsw"
  pPrint Fmadd_s = text "fmadd_s"
  pPrint Fmsub_s = text "fmsub_s"
  pPrint Fnmsub_s = text "fnmsub_s"
  pPrint Fnmadd_s = text "fnmadd_s"
  pPrint Fadd_s = text "fadd_s"
  pPrint Fsub_s = text "fsub_s"
  pPrint Fmul_s = text "fmul_s"
  pPrint Fdiv_s = text "fdiv_s"
  pPrint Fsqrt_s = text "fsqrt_s"
  pPrint Fsgnj_s = text "fsgnj_s"
  pPrint Fsgnjn_s = text "fsgnjn_s"
  pPrint Fsgnjx_s = text "fsgnjx_s"
  pPrint Fmin_s = text "fmin_s"
  pPrint Fmax_s = text "fmax_s"
  pPrint Fcvt_w_s = text "fcvt_w_s"
  pPrint Fcvt_wu_s = text "fcvt_wu_s"
  pPrint Fmv_x_w = text "fmv_x_w"
  pPrint Feq_s = text "feq_s"
  pPrint Flt_s = text "flt_s"
  pPrint Fle_s = text "fle_s"
  pPrint Fclass_s = text "fclass_s"
  pPrint Fcvt_s_w = text "fcvt_s_w"
  pPrint Fcvt_s_wu = text "fcvt_s_wu"
  pPrint Fmv_w_x = text "fmv_w_x"

  -- RV64F
  pPrint Fcvt_l_s = text "fcvt_l_s"
  pPrint Fcvt_lu_s = text "fcvt_lu_s"
  pPrint Fcvt_s_l = text "fcvt_s_l"
  pPrint Fcvt_s_lu = text "fcvt_s_lu"

  -- RV32D
  pPrint Fld = text "fld"
  pPrint Fsd = text "fsd"
  pPrint Fmadd_d = text "fmadd_d"
  pPrint Fmsub_d = text "fmsub_d"
  pPrint Fnmsub_d = text "fnmsub_d"
  pPrint Fnmadd_d = text "fnmadd_d"
  pPrint Fadd_d = text "fadd_d"
  pPrint Fsub_d = text "fsub_d"
  pPrint Fmul_d = text "fmul_d"
  pPrint Fdiv_d = text "fdiv_d"
  pPrint Fsqrt_d = text "fsqrt_d"
  pPrint Fsgnj_d = text "fsgnj_d"
  pPrint Fsgnjn_d = text "fsgnjn_d"
  pPrint Fsgnjx_d = text "fsgnjx_d"
  pPrint Fmin_d = text "fmin_d"
  pPrint Fmax_d = text "fmax_d"
  pPrint Fcvt_s_d = text "fcvt_s_d"
  pPrint Fcvt_d_s = text "fcvt_d_s"
  pPrint Feq_d = text "feq_d"
  pPrint Flt_d = text "flt_d"
  pPrint Fle_d = text "fle_d"
  pPrint Fclass_d = text "fclass_d"
  pPrint Fcvt_w_d = text "fcvt_w_d"
  pPrint Fcvt_wu_d = text "fcvt_wu_d"
  pPrint Fcvt_d_w = text "fcvt_d_w"
  pPrint Fcvt_d_wu = text "fcvt_d_wu"

  -- RV64D
  pPrint Fcvt_l_d = text "fcvt_l_d"
  pPrint Fcvt_lu_d = text "fcvt_lu_d"
  pPrint Fmv_x_d = text "fmv_x_d"
  pPrint Fcvt_d_l = text "fcvt_d_l"
  pPrint Fcvt_d_lu = text "fcvt_d_lu"
  pPrint Fmv_d_x = text "fmv_d_x"

----------------------------------------
-- Instructions

-- | RISC-V Instruction, parameterized by base architecture and format.
data Instruction (rv :: RV) (fmt :: Format) =
  Inst (Opcode rv fmt) (Operands fmt)

mkInst :: KnownRepr FormatRepr fmt => Opcode rv fmt -> List BitVector (OperandTypes fmt) -> Instruction rv fmt
mkInst opcode operands = Inst opcode (Operands knownRepr operands)

-- Instances
$(return [])
instance Show (Instruction rv fmt) where
  show (Inst opcode operands) = show opcode ++ " " ++ show operands
instance ShowF (Instruction rv)
instance PrettyF (Instruction rv) where
  pPrintF (Inst opcode operands) = pPrint opcode <+> pPrintF operands
