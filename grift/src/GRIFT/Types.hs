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
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
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
  , RVWidth, RVFloatWidth
  , type (<<)
  , KnownRV
  , rvBaseArch, RVBaseArch
  , rvExts, RVExts
  , withRV
  , withRVWidth
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
  , rv32IRepr
  , rv32IMRepr
  , rv32IMARepr
  , rv32IMAFRepr
  , rv32GRepr
  , rv32GCRepr
  , rv64IRepr
  , rv64IMRepr
  , rv64IMARepr
  , rv64IMAFRepr
  , rv64GRepr
  , rv64GCRepr
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
  , extsPriv, ExtsPriv
  , extsM, ExtsM
  , extsA, ExtsA
  , extsFD, ExtsFD
  , extsC, ExtsC
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
  , SizedBV(..)
  , asSignedSized
  , asUnsignedSized
  , concatSized
  , sizedBV
  , sizedBVInteger
  , unSized
  , widthSized
  , OpBitsTypes
  , OpBits(..)
  , Opcode(..)
  , Instruction(..)
  , mkInst
  , opcodeCast
  , readOpcode
  ) where

import Control.Monad ( join )
import Data.BitVector.Sized as BV
import Data.Char ( toLower )
import Data.Parameterized
import Data.Parameterized.List
import Data.Parameterized.TH.GADT
import GHC.TypeLits ( KnownNat, Nat )
import Numeric ( showHex )
import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJClass

import GRIFT.BitVector.BVApp ( zextOrId )

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

instance Pretty (BaseArchRepr rv) where
  pPrint RV32Repr = text "RV32"
  pPrint RV64Repr = text "RV64"
  pPrint RV128Repr = text "RV128"

----------------------------------------
-- Extension configurations

-- TODO: add a type family that determines whether an Extensions supports a particular
-- extension

-- | This data structure describes the RISC-V extensions that are enabled in a
-- particular type context.
newtype Extensions = Exts (PrivConfig, MConfig, AConfig, FDConfig, CConfig)

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

type family ExtsPriv (exts :: Extensions) :: PrivConfig where
  ExtsPriv (Exts '(priv, _, _, _, _)) = priv

type family ExtsM (exts :: Extensions) :: MConfig where
  ExtsM (Exts '(_, m, _, _, _)) = m

type family ExtsA (exts :: Extensions) :: AConfig where
  ExtsA (Exts '(_, _, a, _, _)) = a

type family ExtsFD (exts :: Extensions) :: FDConfig where
  ExtsFD (Exts '(_, _, _, fd, _)) = fd

type family ExtsC (exts :: Extensions) :: CConfig where
  ExtsC (Exts '(_, _, _, _, c)) = c

extsPriv :: ExtensionsRepr exts -> PrivConfigRepr (ExtsPriv exts)
extsPriv (ExtensionsRepr privRepr _ _ _ _) = privRepr

extsM :: ExtensionsRepr exts -> MConfigRepr (ExtsM exts)
extsM (ExtensionsRepr _ mRepr _ _ _) = mRepr

extsA :: ExtensionsRepr exts -> AConfigRepr (ExtsA exts)
extsA (ExtensionsRepr _ _ aRepr _ _) = aRepr

extsFD :: ExtensionsRepr exts -> FDConfigRepr (ExtsFD exts)
extsFD (ExtensionsRepr _ _ _ fdRepr _) = fdRepr

extsC :: ExtensionsRepr exts -> CConfigRepr (ExtsC exts)
extsC (ExtensionsRepr _ _ _ _ cRepr) = cRepr

instance Pretty (ExtensionsRepr exts) where
  pPrint (ExtensionsRepr _privRepr MYesRepr AYesRepr FDYesRepr cRepr) =
    text "G" <> pPrint cRepr
  pPrint (ExtensionsRepr _privRepr mRepr aRepr fdRepr cRepr) =
    text "I" <>
    pPrint mRepr <>
    pPrint aRepr <>
    pPrint fdRepr <>
    pPrint cRepr

-- | A runtime representative for 'PrivConfig' for dependent typing.
data PrivConfigRepr :: PrivConfig -> * where
  PrivMRepr   :: PrivConfigRepr PrivM
  PrivMURepr  :: PrivConfigRepr PrivMU
  PrivMSURepr :: PrivConfigRepr PrivMSU

instance KnownRepr PrivConfigRepr PrivM   where knownRepr = PrivMRepr
instance KnownRepr PrivConfigRepr PrivMU  where knownRepr = PrivMURepr
instance KnownRepr PrivConfigRepr PrivMSU where knownRepr = PrivMSURepr

-- For now, don't pretty print the supported privilege level
instance Pretty (PrivConfigRepr pc) where
  pPrint _ = text ""

-- | A runtime representative for 'MConfig' for dependent typing.
data MConfigRepr :: MConfig -> * where
  MYesRepr :: MConfigRepr MYes
  MNoRepr  :: MConfigRepr MNo

instance KnownRepr MConfigRepr MYes where knownRepr = MYesRepr
instance KnownRepr MConfigRepr MNo  where knownRepr = MNoRepr

instance Pretty (MConfigRepr mc) where
  pPrint MYesRepr = text "M"
  pPrint _ = text ""

-- | A runtime representative for 'AConfig' for dependent typing.
data AConfigRepr :: AConfig -> * where
  AYesRepr :: AConfigRepr AYes
  ANoRepr  :: AConfigRepr ANo

instance KnownRepr AConfigRepr AYes where knownRepr = AYesRepr
instance KnownRepr AConfigRepr ANo  where knownRepr = ANoRepr

instance Pretty (AConfigRepr ac) where
  pPrint AYesRepr = text "A"
  pPrint _ = text ""

-- | A runtime representative for 'FDConfig' for dependent typing.
data FDConfigRepr :: FDConfig -> * where
  FDYesRepr    :: FDConfigRepr FDYes
  FYesDNoRepr  :: FDConfigRepr FYesDNo
  FDNoRepr     :: FDConfigRepr FDNo

instance KnownRepr FDConfigRepr FDYes   where knownRepr = FDYesRepr
instance KnownRepr FDConfigRepr FYesDNo where knownRepr = FYesDNoRepr
instance KnownRepr FDConfigRepr FDNo    where knownRepr = FDNoRepr

instance Pretty (FDConfigRepr fdc) where
  pPrint FDYesRepr = text "FD"
  pPrint FYesDNoRepr = text "F"
  pPrint _ = text ""

-- | A runtime representative for 'CConfig' for dependent typing.
data CConfigRepr :: CConfig -> * where
  CYesRepr :: CConfigRepr CYes
  CNoRepr  :: CConfigRepr CNo

instance KnownRepr CConfigRepr CYes where knownRepr = CYesRepr
instance KnownRepr CConfigRepr CNo  where knownRepr = CNoRepr

instance Pretty (CConfigRepr cc) where
  pPrint CYesRepr = text "C"
  pPrint _ = text ""

-- | Everything we need to know about an 'Extensions' at compile time.
type KnownExtensions exts = KnownRepr ExtensionsRepr exts

-- | Type-level representation of a RISC-V extension.
data Extension = AExt | DExt | FExt | MExt | CExt | SExt | UExt

type AExt = 'AExt
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
newtype RV = RVConfig (BaseArch, Extensions)

type RVConfig = 'RVConfig

-- | Type-level representation of 'RV'.
data RVRepr :: RV -> * where
  RVRepr :: BaseArchRepr arch -> ExtensionsRepr exts -> RVRepr (RVConfig '(arch, exts))

type family RVBaseArch (rv :: RV) :: BaseArch where
  RVBaseArch (RVConfig '(arch, _)) = arch

type family RVExts (rv :: RV) :: Extensions where
  RVExts (RVConfig '(_, exts)) = exts

-- | Get a 'BaseArchRepr' from an 'RVRepr'.
rvBaseArch :: RVRepr rv -> BaseArchRepr (RVBaseArch rv)
rvBaseArch (RVRepr archRepr _) = archRepr

-- | Get an 'ExtensionsRepr' from an 'RVRepr'.
rvExts :: RVRepr rv -> ExtensionsRepr (RVExts rv)
rvExts (RVRepr _ extsRepr) = extsRepr

instance (KnownArch arch, KnownExtensions exts) => KnownRepr RVRepr (RVConfig '(arch, exts)) where
  knownRepr = RVRepr knownRepr knownRepr

instance Pretty (RVRepr rv) where
  pPrint (RVRepr baseRepr extsRepr) = pPrint baseRepr <> pPrint extsRepr

-- | The width of the GPRs are known at compile time.
type KnownRVWidth rv = (KnownNat (RVWidth rv), 1 <= RVWidth rv)

-- | The width of the floating point registers are known at compile time.
type KnownRVFloatWidth rv = (KnownNat (RVFloatWidth rv), 1 <= RVFloatWidth rv)

-- | Everything we need to know about an 'RV' at compile time.
type KnownRV rv = ( KnownRepr RVRepr rv
                  , KnownNat (RVWidth rv)
                  , 1 <= RVWidth rv
                  , KnownNat (RVFloatWidth rv)
                  , 1 <= RVFloatWidth rv
                  )

-- | Maps a RISC-V configuration to its register width.
type family RVWidth (rv :: RV) :: Nat where
  RVWidth rv = ArchWidth (RVBaseArch rv)

-- | Maps a 'FDConfig' to its corresponding floating point register width.
type family FDFloatWidth (fd :: FDConfig) :: Nat where
  FDFloatWidth FDYes = 64
  FDFloatWidth _ = 32

-- | Maps a RISC-V configuration to its floating point register width.
type family RVFloatWidth (rv :: RV) :: Nat where
  RVFloatWidth rv = FDFloatWidth (ExtsFD (RVExts rv))

-- | Maps a RISC-V configuration to its 'FDConfig'.
type family RVFloatType (rv :: RV) :: FDConfig where
  RVFloatType (RVConfig '(_, Exts '(_, _, _, fd, _))) = fd

-- | Maps a RISC-V configuration to its C configuration.
type family RVCConfig (rv :: RV) :: CConfig where
  RVCConfig (RVConfig '(_, Exts '(_, _, _, _, c))) = c

-- | 'ExtensionsContains' in constraint form.
type family (<<) (e :: Extension) (rv :: RV) where
  e << RVConfig '(_, exts) = ExtensionsContains exts e ~ 'True

withBaseArch :: BaseArchRepr arch -> (KnownRepr BaseArchRepr arch => b) -> b
withBaseArch RV32Repr b = b
withBaseArch RV64Repr b = b
withBaseArch RV128Repr b = b

withPriv :: PrivConfigRepr priv -> (KnownRepr PrivConfigRepr priv => b) -> b
withPriv PrivMRepr b = b
withPriv PrivMURepr b = b
withPriv PrivMSURepr b = b

withM :: MConfigRepr m -> (KnownRepr MConfigRepr m => b) -> b
withM MYesRepr b = b
withM MNoRepr b = b

withA :: AConfigRepr a -> (KnownRepr AConfigRepr a => b) -> b
withA AYesRepr b = b
withA ANoRepr b = b

withFD :: FDConfigRepr fd -> (KnownRepr FDConfigRepr fd => b) -> b
withFD FDYesRepr b = b
withFD FYesDNoRepr b = b
withFD FDNoRepr b = b

withC :: CConfigRepr c -> (KnownRepr CConfigRepr c => b) -> b
withC CYesRepr b = b
withC CNoRepr b = b

withExts :: ExtensionsRepr exts -> (KnownRepr ExtensionsRepr exts => b) -> b
withExts (ExtensionsRepr priv m a fd c) b =
  withPriv priv $
  withM m $
  withA a $
  withFD fd $
  withC c b

-- | Satisfy a 'KnownRVWidth' constraint from an explicit 'RVRepr'.
withRVWidth :: RVRepr rv -> ((KnownRVWidth rv, 32 <= RVWidth rv) => b) -> b
withRVWidth (RVRepr RV32Repr _) b = b
withRVWidth (RVRepr RV64Repr _) b = b
withRVWidth (RVRepr RV128Repr _) b = b

-- | Satisfy a 'KnownRVFloatWidth' constraint from an explicit 'RVRepr'.
withRVFloatWidth :: RVRepr rv -> (KnownRVFloatWidth rv => b) -> b
withRVFloatWidth (RVRepr _ (ExtensionsRepr _ _ _ FDYesRepr _)) b = b
withRVFloatWidth (RVRepr _ (ExtensionsRepr _ _ _ FYesDNoRepr _)) b = b
withRVFloatWidth (RVRepr _ (ExtensionsRepr _ _ _ FDNoRepr _)) b = b

-- | Satisfy a 'KnownRV' constraint from an explicit 'RVRepr'.
withRV :: RVRepr rv -> (KnownRV rv => b) -> b
withRV rvRepr@(RVRepr baseRepr extsRepr) b =
  withBaseArch baseRepr $
  withExts extsRepr $
  withRVWidth rvRepr $
  withRVFloatWidth rvRepr b

needsRV64 :: RVRepr rv -> ((64 <= RVWidth rv) => a) -> Maybe a
needsRV64 (RVRepr RV64Repr _) a = Just a
needsRV64 (RVRepr RV128Repr _) a = Just a
needsRV64 _ _ = Nothing

needsM :: RVRepr rv -> ((MExt << rv) => a) -> Maybe a
needsM (RVRepr _ (ExtensionsRepr _ MYesRepr _ _ _)) a = Just a
needsM _ _ = Nothing

needsA :: RVRepr rv -> ((AExt << rv) => a) -> Maybe a
needsA (RVRepr _ (ExtensionsRepr _ _ AYesRepr _ _)) a = Just a
needsA _ _ = Nothing

needsF :: RVRepr rv -> ((FExt << rv) => a) -> Maybe a
needsF (RVRepr _ (ExtensionsRepr _ _ _ FYesDNoRepr _)) a = Just a
needsF (RVRepr _ (ExtensionsRepr _ _ _ FDYesRepr _)) a = Just a
needsF _ _ = Nothing

needsD :: RVRepr rv -> ((DExt << rv) => a) -> Maybe a
needsD (RVRepr _ (ExtensionsRepr _ _ _ FDYesRepr _)) a = Just a
needsD _ _ = Nothing

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

rv32IRepr :: RVRepr RV32I
rv32IRepr = RVRepr RV32Repr (ExtensionsRepr PrivMRepr MNoRepr ANoRepr FDNoRepr CNoRepr)

rv32IMRepr :: RVRepr RV32IM
rv32IMRepr = RVRepr RV32Repr (ExtensionsRepr PrivMRepr MYesRepr ANoRepr FDNoRepr CNoRepr)

rv32IMARepr :: RVRepr RV32IMA
rv32IMARepr = RVRepr RV32Repr (ExtensionsRepr PrivMRepr MYesRepr AYesRepr FDNoRepr CNoRepr)

rv32IMAFRepr :: RVRepr RV32IMAF
rv32IMAFRepr = RVRepr RV32Repr (ExtensionsRepr PrivMRepr MYesRepr AYesRepr FYesDNoRepr CNoRepr)

rv32GRepr :: RVRepr RV32G
rv32GRepr = RVRepr RV32Repr (ExtensionsRepr PrivMRepr MYesRepr AYesRepr FDYesRepr CNoRepr)

rv32GCRepr :: RVRepr RV32GC
rv32GCRepr = RVRepr RV32Repr (ExtensionsRepr PrivMRepr MYesRepr AYesRepr FDYesRepr CYesRepr)

rv64IRepr :: RVRepr RV64I
rv64IRepr = RVRepr RV64Repr (ExtensionsRepr PrivMRepr MNoRepr ANoRepr FDNoRepr CNoRepr)

rv64IMRepr :: RVRepr RV64IM
rv64IMRepr = RVRepr RV64Repr (ExtensionsRepr PrivMRepr MYesRepr ANoRepr FDNoRepr CNoRepr)

rv64IMARepr :: RVRepr RV64IMA
rv64IMARepr = RVRepr RV64Repr (ExtensionsRepr PrivMRepr MYesRepr AYesRepr FDNoRepr CNoRepr)

rv64IMAFRepr :: RVRepr RV64IMAF
rv64IMAFRepr = RVRepr RV64Repr (ExtensionsRepr PrivMRepr MYesRepr AYesRepr FYesDNoRepr CNoRepr)

rv64GRepr :: RVRepr RV64G
rv64GRepr = RVRepr RV64Repr (ExtensionsRepr PrivMRepr MYesRepr AYesRepr FDYesRepr CNoRepr)

rv64GCRepr :: RVRepr RV64GC
rv64GCRepr = RVRepr RV64Repr (ExtensionsRepr PrivMRepr MYesRepr AYesRepr FDYesRepr CYesRepr)

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

deriving instance Show (OperandName w)

instance ShowF OperandName where
  showF x = show x

instance Pretty (OperandName w) where
  pPrint Aq = "aq"
  pPrint Rl = "rl"
  pPrint Rm = "rm"
  pPrint Rd = "rd"
  pPrint Rs1 = "rs1"
  pPrint Rs2 = "rs2"
  pPrint Rs3 = "rs3"
  pPrint Imm5 = "imm5"
  pPrint Shamt5 = "shamt5"
  pPrint Shamt7 = "shamt7"
  pPrint Imm12 = "imm12"
  pPrint Csr = "csr"
  pPrint Imm20 = "imm20"
  pPrint Imm32 = "imm32"

-- | An 'OperandID is just an index into a particular format's 'OperandTypes' list.
newtype OperandID (fmt :: Format) (w :: Nat) = OperandID { unOperandID :: Index (OperandTypes fmt) w }
  deriving Eq

$(return [])
instance TestEquality (OperandID fmt) where
  testEquality = $(structuralTypeEquality [t|OperandID|]
                    [(AnyType `TypeApp` AnyType `TypeApp` AnyType, [|testEquality|])
                    ])

instance OrdF (OperandID fmt) where
  OperandID ix1 `compareF` OperandID ix2 = ix1 `compareF` ix2

-- BV no longer contains a width witness, but we need it for some purposes, so
-- this wrapper adds it.
data SizedBV w where
  SizedBV :: !(NatRepr w) -> BV w -> SizedBV w
  deriving ( Eq, Show )

unSized :: SizedBV w -> BV w
unSized (SizedBV _ bv) = bv

widthSized :: SizedBV w -> NatRepr w
widthSized (SizedBV w _) = w

asSignedSized :: 1 <= w => SizedBV w -> Integer
asSignedSized (SizedBV w bv) = asSigned w bv

asUnsignedSized :: SizedBV w -> Integer
asUnsignedSized (SizedBV _ bv) = asUnsigned bv

concatSized :: SizedBV a -> SizedBV b -> SizedBV (a + b)
concatSized (SizedBV a va) (SizedBV b vb) =
  SizedBV (addNat a b) (BV.concat a b va vb)

instance ShowF SizedBV

-- | Mostly for the convenience of numeric literals
instance (KnownNat w, 1 <= w) => Num (SizedBV w) where
  (+) (SizedBV _ bva) (SizedBV _ bvb) =
    SizedBV knownNat (add knownNat (zextOrId bva) (zextOrId bvb))
  (*) (SizedBV _ bva) (SizedBV _ bvb) =
    SizedBV knownNat (mul knownNat (zextOrId bva) (zextOrId bvb))
  abs (SizedBV _ bv) = SizedBV knownNat (BV.abs knownNat bv)
  signum (SizedBV _ bv) = SizedBV knownNat (BV.signum knownNat bv)
  fromInteger = sizedBVInteger
  negate (SizedBV _ bv) = SizedBV knownNat (BV.negate knownNat bv)

instance KnownNat w => Enum (SizedBV w) where
  fromEnum (SizedBV _ (BV i)) = fromIntegral i
  toEnum = sizedBVInteger . fromIntegral

instance Ord (SizedBV w) where
  (SizedBV _ (BV a)) `compare` (SizedBV _ (BV b)) = a `compare` b

sizedBV :: KnownNat w => BV w -> SizedBV w
sizedBV = SizedBV knownNat

sizedBVInteger :: KnownNat w => Integer -> SizedBV w
sizedBVInteger = sizedBV . mkBV knownNat

instance TestEquality SizedBV where
  testEquality (SizedBV w1 _) (SizedBV w2 _) =
    case testEquality w1 w2 of
      Just Refl -> Just Refl
      Nothing -> Nothing

instance OrdF SizedBV where
  compareF (SizedBV w1 _) (SizedBV w2 _) = compareF w1 w2

-- | RISC-V Operand lists, parameterized by format.
data Operands :: Format -> * where
  Operands :: FormatRepr fmt -> List SizedBV (OperandTypes fmt) -> Operands fmt

prettyReg :: SizedBV 5 -> Doc
prettyReg (SizedBV _ bv) = text "x" <> integer (asUnsigned bv)

prettyImm :: (KnownNat w, 1 <= w) => SizedBV w -> Doc
prettyImm (SizedBV _ bv) = text $ "0x" ++ showHex (asSigned knownNat bv) ""

_prettyAddr :: (KnownNat w, 1 <= w) => SizedBV w -> SizedBV 5 -> Doc
_prettyAddr offset reg = prettyImm offset <> parens (prettyReg reg)

commas :: [Doc] -> Doc
commas = hcat . punctuate (comma <> space)

-- TODO: Change pretty printing for addresses to use offset(reg). This involves
-- dealing with individual instructions (yuck).
instance Pretty (Operands fmt) where
  pPrint (Operands RRepr (rd :< rs1 :< rs2 :< Nil)) =
    commas [prettyReg rd, prettyReg rs1, prettyReg rs2]
  pPrint (Operands IRepr (rd :< rs1 :< imm :< Nil)) =
    commas [prettyReg rd, prettyReg rs1, prettyImm imm]
  pPrint (Operands SRepr (rs1 :< rs2 :< imm :< Nil)) =
    commas [prettyReg rs2, prettyImm imm, parens (prettyReg rs1)]
  pPrint (Operands BRepr (rs1 :< rs2 :< imm :< Nil)) =
    commas [prettyReg rs1, prettyReg rs2, comma <+> prettyImm imm]
  pPrint (Operands URepr (rd :< imm :< Nil)) =
    commas [prettyReg rd, prettyImm imm]
  pPrint (Operands JRepr (rd :< imm :< Nil)) =
    commas [prettyReg rd, prettyImm imm]
  pPrint (Operands HRepr (rd :< rs1 :< imm :< Nil)) =
    commas [prettyReg rd, prettyReg rs1, prettyImm imm]
  pPrint (Operands PRepr _) = empty
  pPrint (Operands ARepr (rd :< rs1 :< rs2 :< _rl :< _aq :< Nil)) =
    commas [prettyReg rd, prettyReg rs2, parens (prettyReg rs1)]
  pPrint (Operands R2Repr (rd :< _rm :< rs1 :< Nil)) =
    commas [prettyReg rd, prettyReg rs1]
  pPrint (Operands R3Repr (rd :< _rm :< rs1 :< rs2 :< Nil)) =
    commas [prettyReg rd, prettyReg rs1, prettyReg rs2]
  pPrint (Operands R4Repr (rd :< _rm :< rs1 :< rs2 :< rs3 :< Nil)) =
    commas [prettyReg rd, prettyReg rs1, prettyReg rs2, prettyReg rs3]
  pPrint (Operands RXRepr (rd :< rs1 :< Nil)) =
    commas [prettyReg rd, prettyReg rs1]
  pPrint (Operands XRepr (ill :< Nil)) = text (show ill)

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
  OpBits :: FormatRepr fmt -> List SizedBV (OpBitsTypes fmt) -> OpBits fmt

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
  "fence.i" -> Just (Some FenceI)
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

  "lr.w" -> Just (Some Lrw)
  "sc.w" -> Just (Some Scw)
  "amoswap.w" -> Just (Some Amoswapw)
  "amoadd.w" -> Just (Some Amoaddw)
  "amoxor.w" -> Just (Some Amoxorw)
  "amoand.w" -> Just (Some Amoandw)
  "amoor.w" -> Just (Some Amoorw)
  "amomin.w" -> Just (Some Amominw)
  "amomax.w" -> Just (Some Amomaxw)
  "amominu.w" -> Just (Some Amominuw)
  "amomaxu.w" -> Just (Some Amomaxuw)

  "lr.d" -> Just (Some Lrd)
  "sc.d" -> Just (Some Scd)
  "amoswap.d" -> Just (Some Amoswapd)
  "amoadd.d" -> Just (Some Amoaddd)
  "amoxor.d" -> Just (Some Amoxord)
  "amoand.d" -> Just (Some Amoandd)
  "amoor.d" -> Just (Some Amoord)
  "amomin.d" -> Just (Some Amomind)
  "amomax.d" -> Just (Some Amomaxd)
  "amominu.d" -> Just (Some Amominud)
  "amomaxu.d" -> Just (Some Amomaxud)

  -- RV32F
  "flw" -> Just (Some Flw)
  "fsw" -> Just (Some Fsw)
  "fmadd.s" -> Just (Some Fmadd_s)
  "fmsub.s" -> Just (Some Fmsub_s)
  "fnmsub.s" -> Just (Some Fnmsub_s)
  "fnmadd.s" -> Just (Some Fnmadd_s)
  "fadd.s" -> Just (Some Fadd_s)
  "fsub.s" -> Just (Some Fsub_s)
  "fmul.s" -> Just (Some Fmul_s)
  "fdiv.s" -> Just (Some Fdiv_s)
  "fsqrt.s" -> Just (Some Fsqrt_s)
  "fsgnj.s" -> Just (Some Fsgnj_s)
  "fsgnjn.s" -> Just (Some Fsgnjn_s)
  "fsgnjx.s" -> Just (Some Fsgnjx_s)
  "fmin.s" -> Just (Some Fmin_s)
  "fmax.s" -> Just (Some Fmax_s)
  "fcvt.w.s" -> Just (Some Fcvt_w_s)
  "fcvt.wu.s" -> Just (Some Fcvt_wu_s)
  "fmv.x.w" -> Just (Some Fmv_x_w)
  "feq.s" -> Just (Some Feq_s)
  "flt.s" -> Just (Some Flt_s)
  "fle.s" -> Just (Some Fle_s)
  "fclass.s" -> Just (Some Fclass_s)
  "fcvt.s.w" -> Just (Some Fcvt_s_w)
  "fcvt.s.wu" -> Just (Some Fcvt_s_wu)
  "fmv.w.x" -> Just (Some Fmv_w_x)

  "fcvt.l.s" -> Just (Some Fcvt_l_s)
  "fcvt.lu.s" -> Just (Some Fcvt_lu_s)
  "fcvt.s.l" -> Just (Some Fcvt_s_l)
  "fcvt.s.lu" -> Just (Some Fcvt_s_lu)

  "fld" -> Just (Some Fld)
  "fsd" -> Just (Some Fsd)
  "fmadd.d" -> Just (Some Fmadd_d)
  "fmsub.d" -> Just (Some Fmsub_d)
  "fnmsub.d" -> Just (Some Fnmsub_d)
  "fnmadd.d" -> Just (Some Fnmadd_d)
  "fadd.d" -> Just (Some Fadd_d)
  "fsub.d" -> Just (Some Fsub_d)
  "fmul.d" -> Just (Some Fmul_d)
  "fdiv.d" -> Just (Some Fdiv_d)
  "fsqrt.d" -> Just (Some Fsqrt_d)
  "fsgnj.d" -> Just (Some Fsgnj_d)
  "fsgnjn.d" -> Just (Some Fsgnjn_d)
  "fsgnjx.d" -> Just (Some Fsgnjx_d)
  "fmin.d" -> Just (Some Fmin_d)
  "fmax.d" -> Just (Some Fmax_d)
  "fcvt.s.d" -> Just (Some Fcvt_s_d)
  "fcvt.d.s" -> Just (Some Fcvt_d_s)
  "feq.d" -> Just (Some Feq_d)
  "flt.d" -> Just (Some Flt_d)
  "fle.d" -> Just (Some Fle_d)
  "fclass.d" -> Just (Some Fclass_d)
  "fcvt.w.d" -> Just (Some Fcvt_w_d)
  "fcvt.wu.d" -> Just (Some Fcvt_wu_d)
  "fcvt.d.w" -> Just (Some Fcvt_d_w)
  "fcvt.d.wu" -> Just (Some Fcvt_d_wu)

  "fcvt.l.d" -> Just (Some Fcvt_l_d)
  "fcvt.lu.d" -> Just (Some Fcvt_lu_d)
  "fmv.x.d" -> Just (Some Fmv_x_d)
  "fcvt.d.l" -> Just (Some Fcvt_d_l)
  "fcvt.d.lu" -> Just (Some Fcvt_d_lu)
  "fmv.d.x" -> Just (Some Fmv_d_x)

  _ -> Nothing

opcodeCast :: RVRepr rv -> Opcode rv' fmt -> Maybe (Opcode rv fmt, FormatRepr fmt)
opcodeCast _ Add = Just (Add, knownRepr)
opcodeCast _ Sub = Just (Sub, knownRepr)
opcodeCast _ Sll = Just (Sll, knownRepr)
opcodeCast _ Slt = Just (Slt, knownRepr)
opcodeCast _ Sltu = Just (Sltu, knownRepr)
opcodeCast _ Xor = Just (Xor, knownRepr)
opcodeCast _ Srl = Just (Srl, knownRepr)
opcodeCast _ Sra = Just (Sra, knownRepr)
opcodeCast _ Or  = Just (Or, knownRepr)
opcodeCast _ And = Just (And, knownRepr)
opcodeCast _ Jalr   = Just (Jalr, knownRepr)
opcodeCast _ Lb     = Just (Lb, knownRepr)
opcodeCast _ Lh     = Just (Lh, knownRepr)
opcodeCast _ Lw     = Just (Lw, knownRepr)
opcodeCast _ Lbu    = Just (Lbu, knownRepr)
opcodeCast _ Lhu    = Just (Lhu, knownRepr)
opcodeCast _ Addi   = Just (Addi, knownRepr)
opcodeCast _ Slti   = Just (Slti, knownRepr)
opcodeCast _ Sltiu  = Just (Sltiu, knownRepr)
opcodeCast _ Xori   = Just (Xori, knownRepr)
opcodeCast _ Ori    = Just (Ori, knownRepr)
opcodeCast _ Andi   = Just (Andi, knownRepr)
opcodeCast _ Fence  = Just (Fence, knownRepr)
opcodeCast _ FenceI = Just (FenceI, knownRepr)
opcodeCast _ Csrrw  = Just (Csrrw, knownRepr)
opcodeCast _ Csrrs  = Just (Csrrs, knownRepr)
opcodeCast _ Csrrc  = Just (Csrrc, knownRepr)
opcodeCast _ Csrrwi = Just (Csrrwi, knownRepr)
opcodeCast _ Csrrsi = Just (Csrrsi, knownRepr)
opcodeCast _ Csrrci = Just (Csrrci, knownRepr)
opcodeCast _ Slli   = Just (Slli, knownRepr)
opcodeCast _ Srli   = Just (Srli, knownRepr)
opcodeCast _ Srai   = Just (Srai, knownRepr)
opcodeCast _ Ecall  = Just (Ecall, knownRepr)
opcodeCast _ Ebreak = Just (Ebreak, knownRepr)
opcodeCast _ Sb = Just (Sb, knownRepr)
opcodeCast _ Sh = Just (Sh, knownRepr)
opcodeCast _ Sw = Just (Sw, knownRepr)
opcodeCast _ Beq = Just (Beq, knownRepr)
opcodeCast _ Bne = Just (Bne, knownRepr)
opcodeCast _ Blt = Just (Blt, knownRepr)
opcodeCast _ Bge = Just (Bge, knownRepr)
opcodeCast _ Bltu = Just (Bltu, knownRepr)
opcodeCast _ Bgeu = Just (Bgeu, knownRepr)
opcodeCast _ Lui  = Just (Lui, knownRepr)
opcodeCast _ Auipc = Just (Auipc, knownRepr)
opcodeCast _ Jal = Just (Jal, knownRepr)
opcodeCast _ Illegal = Just (Illegal, knownRepr)
opcodeCast _ Mret = Just (Mret, knownRepr)
opcodeCast _ Wfi = Just (Wfi, knownRepr)

opcodeCast rv Addw = needsRV64 rv (Addw, knownRepr)
opcodeCast rv Subw = needsRV64 rv (Subw, knownRepr)
opcodeCast rv Sllw = needsRV64 rv (Sllw, knownRepr)
opcodeCast rv Srlw = needsRV64 rv (Srlw, knownRepr)
opcodeCast rv Sraw = needsRV64 rv (Sraw, knownRepr)
opcodeCast rv Slliw = needsRV64 rv (Slliw, knownRepr)
opcodeCast rv Srliw = needsRV64 rv (Srliw, knownRepr)
opcodeCast rv Sraiw = needsRV64 rv (Sraiw, knownRepr)
opcodeCast rv Lwu = needsRV64 rv (Lwu, knownRepr)
opcodeCast rv Ld = needsRV64 rv (Ld, knownRepr)
opcodeCast rv Addiw = needsRV64 rv (Addiw, knownRepr)
opcodeCast rv Sd = needsRV64 rv (Sd, knownRepr)

opcodeCast rv Mul = needsM rv (Mul, knownRepr)
opcodeCast rv Mulh = needsM rv (Mulh, knownRepr)
opcodeCast rv Mulhsu = needsM rv (Mulhsu, knownRepr)
opcodeCast rv Mulhu = needsM rv (Mulhu, knownRepr)
opcodeCast rv Div = needsM rv (Div, knownRepr)
opcodeCast rv Divu = needsM rv (Divu, knownRepr)
opcodeCast rv Rem = needsM rv (Rem, knownRepr)
opcodeCast rv Remu = needsM rv (Remu, knownRepr)

opcodeCast rv Mulw = join (needsRV64 rv (needsM rv (Mulw, knownRepr)))
opcodeCast rv Divw = join (needsRV64 rv (needsM rv (Divw, knownRepr)))
opcodeCast rv Divuw = join (needsRV64 rv (needsM rv (Divuw, knownRepr)))
opcodeCast rv Remw = join (needsRV64 rv (needsM rv (Remw, knownRepr)))
opcodeCast rv Remuw = join (needsRV64 rv (needsM rv (Remuw, knownRepr)))

opcodeCast rv Lrw = needsA rv (Lrw, knownRepr)
opcodeCast rv Scw = needsA rv (Scw, knownRepr)
opcodeCast rv Amoswapw = needsA rv (Amoswapw, knownRepr)
opcodeCast rv Amoaddw = needsA rv (Amoaddw, knownRepr)
opcodeCast rv Amoxorw = needsA rv (Amoxorw, knownRepr)
opcodeCast rv Amoandw = needsA rv (Amoandw, knownRepr)
opcodeCast rv Amoorw = needsA rv (Amoorw, knownRepr)
opcodeCast rv Amominw = needsA rv (Amominw, knownRepr)
opcodeCast rv Amomaxw = needsA rv (Amomaxw, knownRepr)
opcodeCast rv Amominuw = needsA rv (Amominuw, knownRepr)
opcodeCast rv Amomaxuw = needsA rv (Amomaxuw, knownRepr)

opcodeCast rv Lrd = join (needsRV64 rv (needsA rv (Lrd, knownRepr)))
opcodeCast rv Scd = join (needsRV64 rv (needsA rv (Scd, knownRepr)))
opcodeCast rv Amoswapd = join (needsRV64 rv (needsA rv (Amoswapd, knownRepr)))
opcodeCast rv Amoaddd = join (needsRV64 rv (needsA rv (Amoaddd, knownRepr)))
opcodeCast rv Amoxord = join (needsRV64 rv (needsA rv (Amoxord, knownRepr)))
opcodeCast rv Amoandd = join (needsRV64 rv (needsA rv (Amoandd, knownRepr)))
opcodeCast rv Amoord = join (needsRV64 rv (needsA rv (Amoord, knownRepr)))
opcodeCast rv Amomind = join (needsRV64 rv (needsA rv (Amomind, knownRepr)))
opcodeCast rv Amomaxd = join (needsRV64 rv (needsA rv (Amomaxd, knownRepr)))
opcodeCast rv Amominud = join (needsRV64 rv (needsA rv (Amominud, knownRepr)))
opcodeCast rv Amomaxud = join (needsRV64 rv (needsA rv (Amomaxud, knownRepr)))

opcodeCast rv Flw = needsF rv (Flw, knownRepr)
opcodeCast rv Fsw = needsF rv (Fsw, knownRepr)
opcodeCast rv Fmadd_s = needsF rv (Fmadd_s, knownRepr)
opcodeCast rv Fmsub_s = needsF rv (Fmsub_s, knownRepr)
opcodeCast rv Fnmsub_s = needsF rv (Fnmsub_s, knownRepr)
opcodeCast rv Fnmadd_s = needsF rv (Fnmadd_s, knownRepr)
opcodeCast rv Fadd_s = needsF rv (Fadd_s, knownRepr)
opcodeCast rv Fsub_s = needsF rv (Fsub_s, knownRepr)
opcodeCast rv Fmul_s = needsF rv (Fmul_s, knownRepr)
opcodeCast rv Fdiv_s = needsF rv (Fdiv_s, knownRepr)
opcodeCast rv Fsqrt_s = needsF rv (Fsqrt_s, knownRepr)
opcodeCast rv Fsgnj_s = needsF rv (Fsgnj_s, knownRepr)
opcodeCast rv Fsgnjn_s = needsF rv (Fsgnjn_s, knownRepr)
opcodeCast rv Fsgnjx_s = needsF rv (Fsgnjx_s, knownRepr)
opcodeCast rv Fmin_s = needsF rv (Fmin_s, knownRepr)
opcodeCast rv Fmax_s = needsF rv (Fmax_s, knownRepr)
opcodeCast rv Fcvt_w_s = needsF rv (Fcvt_w_s, knownRepr)
opcodeCast rv Fcvt_wu_s = needsF rv (Fcvt_wu_s, knownRepr)
opcodeCast rv Fmv_x_w = needsF rv (Fmv_x_w, knownRepr)
opcodeCast rv Feq_s = needsF rv (Feq_s, knownRepr)
opcodeCast rv Flt_s = needsF rv (Flt_s, knownRepr)
opcodeCast rv Fle_s = needsF rv (Fle_s, knownRepr)
opcodeCast rv Fclass_s = needsF rv (Fclass_s, knownRepr)
opcodeCast rv Fcvt_s_w = needsF rv (Fcvt_s_w, knownRepr)
opcodeCast rv Fcvt_s_wu = needsF rv (Fcvt_s_wu, knownRepr)
opcodeCast rv Fmv_w_x = needsF rv (Fmv_w_x, knownRepr)

opcodeCast rv Fcvt_l_s = join (needsRV64 rv (needsF rv (Fcvt_l_s, knownRepr)))
opcodeCast rv Fcvt_lu_s = join (needsRV64 rv (needsF rv (Fcvt_lu_s, knownRepr)))
opcodeCast rv Fcvt_s_l = join (needsRV64 rv (needsF rv (Fcvt_s_l, knownRepr)))
opcodeCast rv Fcvt_s_lu = join (needsRV64 rv (needsF rv (Fcvt_s_lu, knownRepr)))

opcodeCast rv Fld = needsD rv (Fld, knownRepr)
opcodeCast rv Fsd = needsD rv (Fsd, knownRepr)
opcodeCast rv Fmadd_d = needsD rv (Fmadd_d, knownRepr)
opcodeCast rv Fmsub_d = needsD rv (Fmsub_d, knownRepr)
opcodeCast rv Fnmsub_d = needsD rv (Fnmsub_d, knownRepr)
opcodeCast rv Fnmadd_d = needsD rv (Fnmadd_d, knownRepr)
opcodeCast rv Fadd_d = needsD rv (Fadd_d, knownRepr)
opcodeCast rv Fsub_d = needsD rv (Fsub_d, knownRepr)
opcodeCast rv Fmul_d = needsD rv (Fmul_d, knownRepr)
opcodeCast rv Fdiv_d = needsD rv (Fdiv_d, knownRepr)
opcodeCast rv Fsqrt_d = needsD rv (Fsqrt_d, knownRepr)
opcodeCast rv Fsgnj_d = needsD rv (Fsgnj_d, knownRepr)
opcodeCast rv Fsgnjn_d = needsD rv (Fsgnjn_d, knownRepr)
opcodeCast rv Fsgnjx_d = needsD rv (Fsgnjx_d, knownRepr)
opcodeCast rv Fmin_d = needsD rv (Fmin_d, knownRepr)
opcodeCast rv Fmax_d = needsD rv (Fmax_d, knownRepr)
opcodeCast rv Fcvt_s_d = needsD rv (Fcvt_s_d, knownRepr)
opcodeCast rv Fcvt_d_s = needsD rv (Fcvt_d_s, knownRepr)
opcodeCast rv Feq_d = needsD rv (Feq_d, knownRepr)
opcodeCast rv Flt_d = needsD rv (Flt_d, knownRepr)
opcodeCast rv Fle_d = needsD rv (Fle_d, knownRepr)
opcodeCast rv Fclass_d = needsD rv (Fclass_d, knownRepr)
opcodeCast rv Fcvt_w_d = needsD rv (Fcvt_w_d, knownRepr)
opcodeCast rv Fcvt_wu_d = needsD rv (Fcvt_wu_d, knownRepr)
opcodeCast rv Fcvt_d_w = needsD rv (Fcvt_d_w, knownRepr)
opcodeCast rv Fcvt_d_wu = needsD rv (Fcvt_d_wu, knownRepr)

opcodeCast rv Fcvt_l_d = join (needsRV64 rv (needsD rv (Fcvt_l_d, knownRepr)))
opcodeCast rv Fcvt_lu_d = join (needsRV64 rv (needsD rv (Fcvt_lu_d, knownRepr)))
opcodeCast rv Fmv_x_d = join (needsRV64 rv (needsD rv (Fmv_x_d, knownRepr)))
opcodeCast rv Fcvt_d_l = join (needsRV64 rv (needsD rv (Fcvt_d_l, knownRepr)))
opcodeCast rv Fcvt_d_lu = join (needsRV64 rv (needsD rv (Fcvt_d_lu, knownRepr)))
opcodeCast rv Fmv_d_x = join (needsRV64 rv (needsD rv (Fmv_d_x, knownRepr)))

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

  pPrint Add  = "add"
  pPrint Sub  = "sub"
  pPrint Sll  = "sll"
  pPrint Slt  = "slt"
  pPrint Sltu = "sltu"
  pPrint Xor  = "xor"
  pPrint Srl  = "srl"
  pPrint Sra  = "sra"
  pPrint Or   = "or"
  pPrint And  = "and"

  pPrint Jalr   = "jalr"
  pPrint Lb     = "lb"
  pPrint Lh     = "lh"
  pPrint Lw     = "lw"
  pPrint Lbu    = "lbu"
  pPrint Lhu    = "lhu"
  pPrint Addi   = "addi"
  pPrint Slti   = "slti"
  pPrint Sltiu  = "sltiu"
  pPrint Xori   = "xori"
  pPrint Ori    = "ori"
  pPrint Andi   = "andi"
  pPrint Fence  = "fence"
  pPrint FenceI = "fence.i"
  pPrint Csrrw  = "csrrw"
  pPrint Csrrs  = "csrrs"
  pPrint Csrrc  = "csrrc"
  pPrint Csrrwi = "csrrwi"
  pPrint Csrrsi = "csrrsi"
  pPrint Csrrci = "csrrci"

  pPrint Slli   = "slli"
  pPrint Srli   = "srli"
  pPrint Srai   = "srai"

  pPrint Ecall  = "ecall"
  pPrint Ebreak = "ebreak"

  pPrint Sb     = "sb"
  pPrint Sh     = "sh"
  pPrint Sw     = "sw"

  pPrint Beq    = "beq"
  pPrint Bne    = "bne"
  pPrint Blt    = "blt"
  pPrint Bge    = "bge"
  pPrint Bltu = "bltu"
  pPrint Bgeu   = "bgeu"

  pPrint Lui   = "lui"
  pPrint Auipc = "auipc"

  pPrint Jal = "jal"

  pPrint Illegal = "illegal"

  -- RV64
  pPrint Addw = "addw"
  pPrint Subw = "subw"
  pPrint Sllw = "sllw"
  pPrint Srlw = "srlw"
  pPrint Sraw = "sraw"
  pPrint Slliw = "slliw"
  pPrint Srliw = "srliw"
  pPrint Sraiw = "sraiw"
  pPrint Lwu = "lwu"
  pPrint Ld = "ld"
  pPrint Addiw = "addiw"
  pPrint Sd = "sd"

  -- M privileged instructions
  pPrint Mret = "mret"
  pPrint Wfi = "wfi"

  -- RV32M
  pPrint Mul = "mul"
  pPrint Mulh = "mulh"
  pPrint Mulhsu = "mulhsu"
  pPrint Mulhu = "mulhu"
  pPrint Div = "div"
  pPrint Divu = "divu"
  pPrint Rem = "rem"
  pPrint Remu = "remu"

  -- RV64M
  pPrint Mulw = "mulw"
  pPrint Divw = "divw"
  pPrint Divuw = "divuw"
  pPrint Remw = "remw"
  pPrint Remuw = "remuw"

  -- RV32A
  pPrint Lrw = "lrw"
  pPrint Scw = "scw"
  pPrint Amoswapw = "amoswapw"
  pPrint Amoaddw = "amoaddw"
  pPrint Amoxorw = "amoxorw"
  pPrint Amoandw = "amoandw"
  pPrint Amoorw = "amoorw"
  pPrint Amominw = "amominw"
  pPrint Amomaxw = "amomaxw"
  pPrint Amominuw = "amominuw"
  pPrint Amomaxuw = "amomaxuw"

  -- RV64A
  pPrint Lrd = "lrd"
  pPrint Scd = "scd"
  pPrint Amoswapd = "amoswapd"
  pPrint Amoaddd = "amoaddd"
  pPrint Amoxord = "amoxord"
  pPrint Amoandd = "amoandd"
  pPrint Amoord = "amoord"
  pPrint Amomind = "amomind"
  pPrint Amomaxd = "amomaxd"
  pPrint Amominud = "amominud"
  pPrint Amomaxud = "amomaxud"

  -- RV32F
  pPrint Flw = "flw"
  pPrint Fsw = "fsw"
  pPrint Fmadd_s = "fmadd.s"
  pPrint Fmsub_s = "fmsub.s"
  pPrint Fnmsub_s = "fnmsub.s"
  pPrint Fnmadd_s = "fnmadd.s"
  pPrint Fadd_s = "fadd.s"
  pPrint Fsub_s = "fsub.s"
  pPrint Fmul_s = "fmul.s"
  pPrint Fdiv_s = "fdiv.s"
  pPrint Fsqrt_s = "fsqrt.s"
  pPrint Fsgnj_s = "fsgnj.s"
  pPrint Fsgnjn_s = "fsgnjn.s"
  pPrint Fsgnjx_s = "fsgnjx.s"
  pPrint Fmin_s = "fmin.s"
  pPrint Fmax_s = "fmax.s"
  pPrint Fcvt_w_s = "fcvt.w.s"
  pPrint Fcvt_wu_s = "fcvt.wu.s"
  pPrint Fmv_x_w = "fmv.x.w"
  pPrint Feq_s = "feq.s"
  pPrint Flt_s = "flt.s"
  pPrint Fle_s = "fle.s"
  pPrint Fclass_s = "fclass.s"
  pPrint Fcvt_s_w = "fcvt.s.w"
  pPrint Fcvt_s_wu = "fcvt.s.wu"
  pPrint Fmv_w_x = "fmv.w.x"

  -- RV64F
  pPrint Fcvt_l_s = "fcvt.l.s"
  pPrint Fcvt_lu_s = "fcvt.lu.s"
  pPrint Fcvt_s_l = "fcvt.s.l"
  pPrint Fcvt_s_lu = "fcvt.s.lu"

  -- RV32D
  pPrint Fld = "fld"
  pPrint Fsd = "fsd"
  pPrint Fmadd_d = "fmadd.d"
  pPrint Fmsub_d = "fmsub.d"
  pPrint Fnmsub_d = "fnmsub.d"
  pPrint Fnmadd_d = "fnmadd.d"
  pPrint Fadd_d = "fadd.d"
  pPrint Fsub_d = "fsub.d"
  pPrint Fmul_d = "fmul.d"
  pPrint Fdiv_d = "fdiv.d"
  pPrint Fsqrt_d = "fsqrt.d"
  pPrint Fsgnj_d = "fsgnj.d"
  pPrint Fsgnjn_d = "fsgnjn.d"
  pPrint Fsgnjx_d = "fsgnjx.d"
  pPrint Fmin_d = "fmin.d"
  pPrint Fmax_d = "fmax.d"
  pPrint Fcvt_s_d = "fcvt.s.d"
  pPrint Fcvt_d_s = "fcvt.d.s"
  pPrint Feq_d = "feq.d"
  pPrint Flt_d = "flt.d"
  pPrint Fle_d = "fle.d"
  pPrint Fclass_d = "fclass.d"
  pPrint Fcvt_w_d = "fcvt.w.d"
  pPrint Fcvt_wu_d = "fcvt.wu.d"
  pPrint Fcvt_d_w = "fcvt.d.w"
  pPrint Fcvt_d_wu = "fcvt.d.wu"

  -- RV64D
  pPrint Fcvt_l_d = "fcvt.l.d"
  pPrint Fcvt_lu_d = "fcvt.lu.d"
  pPrint Fmv_x_d = "fmv.x.d"
  pPrint Fcvt_d_l = "fcvt.d.l"
  pPrint Fcvt_d_lu = "fcvt.d.lu"
  pPrint Fmv_d_x = "fmv.d.x"

----------------------------------------
-- Instructions

-- | RISC-V Instruction, parameterized by base architecture and format.
data Instruction (rv :: RV) (fmt :: Format) =
  Inst (Opcode rv fmt) (Operands fmt)

-- | Create a new instruction with an associated operand list.
mkInst ::
  KnownRepr FormatRepr fmt =>
  Opcode rv fmt -> List SizedBV (OperandTypes fmt) -> Instruction rv fmt
mkInst opcode operands = Inst opcode (Operands knownRepr operands)

-- Instances
$(return [])
instance Show (Instruction rv fmt) where
  show (Inst opcode operands) = show opcode ++ " " ++ show operands
instance ShowF (Instruction rv)
instance Pretty (Instruction rv fmt) where
  pPrint (Inst opcode operands) = pPrint opcode <+> pPrint operands
