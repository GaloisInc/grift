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
Module      : RISCV.Types
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Definitions for RISC-V base ISA and extensions
-}

module RISCV.Types where

import Data.Parameterized
import Data.Parameterized.TH.GADT
import GHC.TypeLits

----------------------------------------
-- Architecture types
-- | Base architecture types.
data BaseArch = RV32I
              | RV32E
              | RV64I
              | RV128I

type RV32I  = 'RV32I
type RV32E  = 'RV32E
type RV64I  = 'RV64I
type RV128I = 'RV128I

-- | A runtime representative for 'BaseArch' for dependent typing.
data BaseArchRepr :: BaseArch -> * where
  RV32IRepr  :: BaseArchRepr RV32I
  RV32ERepr  :: BaseArchRepr RV32E
  RV64IRepr  :: BaseArchRepr RV64I
  RV128IRepr :: BaseArchRepr RV128I

-- | Maps an architecture to its register width.
type family ArchWidth (arch :: BaseArch) :: Nat where
  ArchWidth RV32I  = 32
  ArchWidth RV32E  = 32
  ArchWidth RV64I  = 64
  ArchWidth RV128I = 128

-- TODO: Is there any way we can avoid needing the KnownNat (ArchWidth arch)?
-- TODO: Ok now this is just fucking gross, I must fix it.
-- | Everything we might need to know about a 'BaseArch' at compile time.
type KnownArch arch = ( KnownNat (ArchWidth arch)
                      , KnownNat (ArchWidth arch + ArchWidth arch)
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
instance KnownRepr BaseArchRepr RV32I  where knownRepr = RV32IRepr
instance KnownRepr BaseArchRepr RV32E  where knownRepr = RV32ERepr
instance KnownRepr BaseArchRepr RV64I  where knownRepr = RV64IRepr
instance KnownRepr BaseArchRepr RV128I where knownRepr = RV128IRepr

----------------------------------------
-- Extension configurations

-- TODO: add a type family that determines whether an Extensions supports a particular
-- extension

-- | This data structure describes the RISC-V extensions that are enabled in a
-- particular type context.
data Extensions = Exts (MConfig, FDConfig)

type Exts = 'Exts

-- | The M extension is either enabled or disabled.
data MConfig = MYes | MNo

type MYes = 'MYes
type MNo = 'MNo

-- | The F and D extensions can be in one of three states: Both are enabled, only F
-- is enabled, or both are disabled.
data FDConfig = FDYes | FYesDNo | FDNo

type FDYes = 'FDYes
type FYesDNo = 'FYesDNo
type FDNo = 'FDNo

-- | A runtime representative for 'Extensions' for dependent typing.
data ExtensionsRepr :: Extensions -> * where
  ExtensionsRepr :: MConfigRepr m -> FDConfigRepr fd -> ExtensionsRepr (Exts '(m, fd))

instance ( KnownRepr MConfigRepr m
         , KnownRepr FDConfigRepr fd
         ) => KnownRepr ExtensionsRepr (Exts '(m, fd)) where
  knownRepr = ExtensionsRepr knownRepr knownRepr

-- | A runtime representative for 'MConfig' for dependent typing.
data MConfigRepr :: MConfig -> * where
  MYesRepr :: MConfigRepr MYes
  MNoRepr  :: MConfigRepr MNo

instance KnownRepr MConfigRepr MYes where knownRepr = MYesRepr
instance KnownRepr MConfigRepr MNo  where knownRepr = MNoRepr

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
data Extension = M | F | D

type M = 'M
type F = 'F
type D = 'D

-- | Type operator that determines whether the 'Extensions' contains a particular
-- 'Extension'.
type family ExtensionsContains (exts :: Extensions) (e :: Extension) :: Bool where
  ExtensionsContains (Exts '( MYes, _))       M = 'True
  ExtensionsContains (Exts '(    _, FDYes))   F = 'True
  ExtensionsContains (Exts '(    _, FYesDNo)) F = 'True
  ExtensionsContains (Exts '(    _, FDYes))   D = 'True
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

data Format = R | I | S | B | U | J | X

type R = 'R
type I = 'I
type S = 'S
type B = 'B
type U = 'U
type J = 'J
type X = 'X

-- | A runtime representative for 'Format' for dependent typing.
data FormatRepr :: Format -> * where
  RRepr :: FormatRepr R
  IRepr :: FormatRepr I
  SRepr :: FormatRepr S
  BRepr :: FormatRepr B
  URepr :: FormatRepr U
  JRepr :: FormatRepr J
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
instance KnownRepr FormatRepr X where knownRepr = XRepr

----------------------------------------
-- Operands

-- | Operand types
data OperandType = RegID | Imm12 | Imm20 | Imm32

type RegID = 'RegID
type Imm12 = 'Imm12
type Imm20 = 'Imm20
type Imm32 = 'Imm32

-- | Operand identifier, parameterized by a 'Format' and an 'OperandType'.
data OperandID :: Format -> OperandType -> * where
  RRd    :: OperandID R RegID
  RRs1   :: OperandID R RegID
  RRs2   :: OperandID R RegID

  IRd    :: OperandID I RegID
  IRs1   :: OperandID I RegID
  IImm12 :: OperandID I Imm12

  SRs1   :: OperandID S RegID
  SRs2   :: OperandID S RegID
  SImm12 :: OperandID S Imm12

  BRs1   :: OperandID B RegID
  BRs2   :: OperandID B RegID
  BImm12 :: OperandID B Imm12

  URd    :: OperandID U RegID
  UImm20 :: OperandID U Imm20

  JRd    :: OperandID J RegID
  JImm20 :: OperandID J Imm20

  XImm32 :: OperandID X Imm32

-- | Maps an 'OperandType' to its length as a 'BitVector'.
type family OperandWidth (otp :: OperandType) :: Nat where
  OperandWidth RegID = 5
  OperandWidth Imm12 = 12
  OperandWidth Imm20 = 20
  OperandWidth Imm32 = 32

-- Instances
$(return [])
deriving instance Show (OperandID fmt ot)

instance ShowF (OperandID fmt)
deriving instance Eq (OperandID fmt ot)
instance EqF (OperandID fmt) where
  eqF = (==)
instance TestEquality (OperandID fmt) where
  testEquality = $(structuralTypeEquality [t|OperandID|] [])
instance OrdF (OperandID fmt) where
  compareF = $(structuralTypeOrd [t|OperandID|] [])
