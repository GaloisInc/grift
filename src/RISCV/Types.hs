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
type (*>>) (exts :: Extensions) (e :: Extension)
  = ExtensionsContains exts e ~ 'True
