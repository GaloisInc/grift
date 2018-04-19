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

Definitions for RISC-V base ISA and extensions
-}

module RISCV.Types where

import Data.BitVector.Sized
import Data.Parameterized
import Data.Parameterized.List
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
-- deriving instance Eq (FormatRepr k)
-- instance EqF FormatRepr where
--   eqF = (==)
-- instance TestEquality FormatRepr where
--   testEquality = $(structuralTypeEquality [t|FormatRepr|] [])
-- instance OrdF FormatRepr where
--   compareF = $(structuralTypeOrd [t|FormatRepr|] [])
instance KnownRepr FormatRepr R where knownRepr = RRepr
instance KnownRepr FormatRepr I where knownRepr = IRepr
instance KnownRepr FormatRepr S where knownRepr = SRepr
instance KnownRepr FormatRepr B where knownRepr = BRepr
instance KnownRepr FormatRepr U where knownRepr = URepr
instance KnownRepr FormatRepr J where knownRepr = JRepr
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
  OperandTypes X = '[32]

type OperandID (fmt :: Format) = Index (OperandTypes fmt)

-- | RISC-V Operand lists, parameterized by format.
data Operands :: Format -> * where
  Operands :: FormatRepr fmt -> List BitVector (OperandTypes fmt) -> Operands fmt

$(return [])
deriving instance Show (Operands k)
instance ShowF Operands

----------------------------------------
-- OpBits

-- type family OpBitsTypes (fmt :: Format) :: [Nat] where
--   OpBitsTypes R = '[7, 3, 7]
--   OpBitsTypes I = '[7, 3]
--   OpBitsTypes S = '[7, 3]
--   OpBitsTypes B = '[7, 3]
--   OpBitsTypes U = '[7]
--   OpBitsTypes J = '[7]
--   OpBitsTypes X = '[]

-- -- | Bits fixed by an opcode.
-- -- Holds all the bits that are fixed by a particular opcode. Each format maps to a
-- -- potentially different set of bits.
-- data OpBits :: Format -> * where
--   OpBits :: FormatRepr fmt -> List BitVector (OpBitsTypes fmt) -> OpBits fmt

-- -- Instances
-- $(return [])
-- deriving instance Show (OpBits k)
-- instance ShowF OpBits
-- -- deriving instance Eq (OpBits k)
-- -- instance EqF OpBits where
-- --   eqF = (==)

-- instance TestEquality OpBits where
--   testEquality = $(structuralTypeOrd [t|OpBits|]
--                    [ (

-- instance TestEquality OpBits where
--   (OpBits RRepr (rd :< rs1 :< rs2 :< Nil)) `testEquality` (OpBits RRepr (rd' :< rs1' :< rs2' :< Nil)) =
--       if rd == rd' && rs1 == rs1' && rs2 == rs2'
--       then Just Refl
--       else Nothing
-- instance OrdF OpBits where
--   compareF = $(structuralTypeOrd [t|OpBits|] [])
