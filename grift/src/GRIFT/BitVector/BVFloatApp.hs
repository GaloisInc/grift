{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Data.BitVector.Sized.Float.App
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Module for handling floating-point expressions.
-}

module GRIFT.BitVector.BVFloatApp
  ( BVFloatApp(..)
  , RM
  , evalBVFloatAppM
  , evalBVFloatApp
  , bvFloatAppWidth
  , BVFloatExpr(..)
--  , PureBVFloatExpr(..)
--  , evalPureBVFloatExpr
  , getFRes
  -- * Miscellaneous functions
  -- ** 32-bit
  , f32Exp, f32Sig, f32Sgn
  , negate32
  , posZero32
  , negZero32
  , canonicalNaN32
  , posInfinity32
  , negInfinity32
  , isNaN32
  , isSNaN32
  , isQNaN32
  , isZero32
  , isNormal32
  , isSubnormal32
  -- ** 64-bit
  , f64Exp, f64Sig, f64Sgn
  , negate64
  , posZero64
  , negZero64
  , canonicalNaN64
  , posInfinity64
  , negInfinity64
  , isNaN64
  , isSNaN64
  , isQNaN64
  , isZero64
  , isNormal64
  , isSubnormal64
  -- * Smart constructors
  -- ** Integer to float
  , ui32ToF16E
  , ui32ToF32E
  , ui32ToF64E
  , i32ToF16E
  , i32ToF32E
  , i32ToF64E
  , ui64ToF16E
  , ui64ToF32E
  , ui64ToF64E
  , i64ToF16E
  , i64ToF32E
  , i64ToF64E
  -- ** Float to integer
  , f16ToUi32E
  , f16ToUi64E
  , f16ToI32E
  , f16ToI64E
  , f32ToUi32E
  , f32ToUi64E
  , f32ToI32E
  , f32ToI64E
  , f64ToUi32E
  , f64ToUi64E
  , f64ToI32E
  , f64ToI64E
  -- * Float to float
  , f16ToF32E
  , f16ToF64E
  , f32ToF16E
  , f32ToF64E
  , f64ToF16E
  , f64ToF32E
  -- * 16-bit operations
  , f16RoundToIntE
  , f16AddE
  , f16SubE
  , f16MulE
  , f16MulAddE
  , f16DivE
  , f16RemE
  , f16SqrtE
  , f16EqE
  , f16LeE
  , f16LtE
  , f16EqSignalingE
  , f16LeQuietE
  , f16LtQuietE
  , f16IsSignalingNaNE
  -- * 32-bit operations
  , f32RoundToIntE
  , f32AddE
  , f32SubE
  , f32MulE
  , f32MulAddE
  , f32DivE
  , f32RemE
  , f32SqrtE
  , f32EqE
  , f32LeE
  , f32LtE
  , f32EqSignalingE
  , f32LeQuietE
  , f32LtQuietE
  , f32IsSignalingNaNE
  -- * 64-bit operations
  , f64RoundToIntE
  , f64AddE
  , f64SubE
  , f64MulE
  , f64MulAddE
  , f64DivE
  , f64RemE
  , f64SqrtE
  , f64EqE
  , f64LeE
  , f64LtE
  , f64EqSignalingE
  , f64LeQuietE
  , f64LtQuietE
  , f64IsSignalingNaNE
  ) where

import Control.Monad.Identity
import qualified Data.BitVector.Sized as BV
import Data.BitVector.Sized.Float
import Data.List (foldl')
import Data.Parameterized
import Data.Parameterized.TH.GADT
import GHC.TypeLits
import GRIFT.BitVector.BVApp
import SoftFloat

-- | Type synonym for rounding-mode expression
type RM expr = expr 3

-- | Representation of a floating point operation, implicitly containing both a
-- result of the given width and the resulting exceptions (the latter of which are
-- stored in the five most significant bits of the result).
data BVFloatApp (expr :: Nat -> *) (w :: Nat) where
  Ui32ToF16App :: !(RM expr) -> !(expr 32) -> BVFloatApp expr 21
  Ui32ToF32App :: !(RM expr) -> !(expr 32) -> BVFloatApp expr 37
  Ui32ToF64App :: !(RM expr) -> !(expr 32) -> BVFloatApp expr 69
  I32ToF16App  :: !(RM expr) -> !(expr 32) -> BVFloatApp expr 21
  I32ToF32App  :: !(RM expr) -> !(expr 32) -> BVFloatApp expr 37
  I32ToF64App  :: !(RM expr) -> !(expr 32) -> BVFloatApp expr 69
  Ui64ToF16App :: !(RM expr) -> !(expr 64) -> BVFloatApp expr 21
  Ui64ToF32App :: !(RM expr) -> !(expr 64) -> BVFloatApp expr 37
  Ui64ToF64App :: !(RM expr) -> !(expr 64) -> BVFloatApp expr 69
  I64ToF16App  :: !(RM expr) -> !(expr 64) -> BVFloatApp expr 21
  I64ToF32App  :: !(RM expr) -> !(expr 64) -> BVFloatApp expr 37
  I64ToF64App  :: !(RM expr) -> !(expr 64) -> BVFloatApp expr 69

  F16ToUi32App :: !(RM expr) -> !(expr 16) -> BVFloatApp expr 37
  F16ToUi64App :: !(RM expr) -> !(expr 16) -> BVFloatApp expr 69
  F16ToI32App  :: !(RM expr) -> !(expr 16) -> BVFloatApp expr 37
  F16ToI64App  :: !(RM expr) -> !(expr 16) -> BVFloatApp expr 69
  F32ToUi32App :: !(RM expr) -> !(expr 32) -> BVFloatApp expr 37
  F32ToUi64App :: !(RM expr) -> !(expr 32) -> BVFloatApp expr 69
  F32ToI32App  :: !(RM expr) -> !(expr 32) -> BVFloatApp expr 37
  F32ToI64App  :: !(RM expr) -> !(expr 32) -> BVFloatApp expr 69
  F64ToUi32App :: !(RM expr) -> !(expr 64) -> BVFloatApp expr 37
  F64ToUi64App :: !(RM expr) -> !(expr 64) -> BVFloatApp expr 69
  F64ToI32App  :: !(RM expr) -> !(expr 64) -> BVFloatApp expr 37
  F64ToI64App  :: !(RM expr) -> !(expr 64) -> BVFloatApp expr 69

  F16ToF32App  :: !(RM expr) -> !(expr 16) -> BVFloatApp expr 37
  F16ToF64App  :: !(RM expr) -> !(expr 16) -> BVFloatApp expr 69
  F32ToF16App  :: !(RM expr) -> !(expr 32) -> BVFloatApp expr 21
  F32ToF64App  :: !(RM expr) -> !(expr 32) -> BVFloatApp expr 69
  F64ToF16App  :: !(RM expr) -> !(expr 64) -> BVFloatApp expr 21
  F64ToF32App  :: !(RM expr) -> !(expr 64) -> BVFloatApp expr 37

  F16RoundToIntApp :: !(RM expr) -> !(expr 16) -> BVFloatApp expr 21
  F16AddApp :: !(RM expr) -> !(expr 16) -> !(expr 16) -> BVFloatApp expr 21
  F16SubApp :: !(RM expr) -> !(expr 16) -> !(expr 16) -> BVFloatApp expr 21
  F16MulApp :: !(RM expr) -> !(expr 16) -> !(expr 16) -> BVFloatApp expr 21
  F16MulAddApp :: !(RM expr) -> !(expr 16) -> !(expr 16) -> !(expr 16) -> BVFloatApp expr 21
  F16DivApp :: !(RM expr) -> !(expr 16) -> !(expr 16) -> BVFloatApp expr 21
  F16RemApp :: !(RM expr) -> !(expr 16) -> !(expr 16) -> BVFloatApp expr 21
  F16SqrtApp :: !(RM expr) -> !(expr 16) -> BVFloatApp expr 21
  F16EqApp :: !(expr 16) -> !(expr 16) -> BVFloatApp expr 6
  F16LeApp :: !(expr 16) -> !(expr 16) -> BVFloatApp expr 6
  F16LtApp :: !(expr 16) -> !(expr 16) -> BVFloatApp expr 6
  F16EqSignalingApp :: !(expr 16) -> !(expr 16) -> BVFloatApp expr 6
  F16LeQuietApp :: !(expr 16) -> !(expr 16) -> BVFloatApp expr 6
  F16LtQuietApp :: !(expr 16) -> !(expr 16) -> BVFloatApp expr 6
  F16IsSignalingNaNApp :: !(expr 16) -> BVFloatApp expr 6

  F32RoundToIntApp :: !(RM expr) -> !(expr 32) -> BVFloatApp expr 37
  F32AddApp :: !(RM expr) -> !(expr 32) -> !(expr 32) -> BVFloatApp expr 37
  F32SubApp :: !(RM expr) -> !(expr 32) -> !(expr 32) -> BVFloatApp expr 37
  F32MulApp :: !(RM expr) -> !(expr 32) -> !(expr 32) -> BVFloatApp expr 37
  F32MulAddApp :: !(RM expr) -> !(expr 32) -> !(expr 32) -> !(expr 32) -> BVFloatApp expr 37
  F32DivApp :: !(RM expr) -> !(expr 32) -> !(expr 32) -> BVFloatApp expr 37
  F32RemApp :: !(RM expr) -> !(expr 32) -> !(expr 32) -> BVFloatApp expr 37
  F32SqrtApp :: !(RM expr) -> !(expr 32) -> BVFloatApp expr 37
  F32EqApp :: !(expr 32) -> !(expr 32) -> BVFloatApp expr 6
  F32LeApp :: !(expr 32) -> !(expr 32) -> BVFloatApp expr 6
  F32LtApp :: !(expr 32) -> !(expr 32) -> BVFloatApp expr 6
  F32EqSignalingApp :: !(expr 32) -> !(expr 32) -> BVFloatApp expr 6
  F32LeQuietApp :: !(expr 32) -> !(expr 32) -> BVFloatApp expr 6
  F32LtQuietApp :: !(expr 32) -> !(expr 32) -> BVFloatApp expr 6
  F32IsSignalingNaNApp :: !(expr 32) -> BVFloatApp expr 6

  F64RoundToIntApp :: !(RM expr) -> !(expr 64) -> BVFloatApp expr 69
  F64AddApp :: !(RM expr) -> !(expr 64) -> !(expr 64) -> BVFloatApp expr 69
  F64SubApp :: !(RM expr) -> !(expr 64) -> !(expr 64) -> BVFloatApp expr 69
  F64MulApp :: !(RM expr) -> !(expr 64) -> !(expr 64) -> BVFloatApp expr 69
  F64MulAddApp :: !(RM expr) -> !(expr 64) -> !(expr 64) -> !(expr 64) -> BVFloatApp expr 69
  F64DivApp :: !(RM expr) -> !(expr 64) -> !(expr 64) -> BVFloatApp expr 69
  F64RemApp :: !(RM expr) -> !(expr 64) -> !(expr 64) -> BVFloatApp expr 69
  F64SqrtApp :: !(RM expr) -> !(expr 64) -> BVFloatApp expr 69
  F64EqApp :: !(expr 64) -> !(expr 64) -> BVFloatApp expr 6
  F64LeApp :: !(expr 64) -> !(expr 64) -> BVFloatApp expr 6
  F64LtApp :: !(expr 64) -> !(expr 64) -> BVFloatApp expr 6
  F64EqSignalingApp :: !(expr 64) -> !(expr 64) -> BVFloatApp expr 6
  F64LeQuietApp :: !(expr 64) -> !(expr 64) -> BVFloatApp expr 6
  F64LtQuietApp :: !(expr 64) -> !(expr 64) -> BVFloatApp expr 6
  F64IsSignalingNaNApp :: !(expr 64) -> BVFloatApp expr 6

bvFloatAppWidth :: BVFloatApp expr w -> NatRepr w
bvFloatAppWidth (Ui32ToF16App _ _) = knownNat
bvFloatAppWidth (Ui32ToF32App _ _) = knownNat
bvFloatAppWidth (Ui32ToF64App _ _) = knownNat
bvFloatAppWidth (I32ToF16App _ _) = knownNat
bvFloatAppWidth (I32ToF32App _ _) = knownNat
bvFloatAppWidth (I32ToF64App _ _) = knownNat
bvFloatAppWidth (Ui64ToF16App _ _) = knownNat
bvFloatAppWidth (Ui64ToF32App _ _) = knownNat
bvFloatAppWidth (Ui64ToF64App _ _) = knownNat
bvFloatAppWidth (I64ToF16App _ _) = knownNat
bvFloatAppWidth (I64ToF32App _ _) = knownNat
bvFloatAppWidth (I64ToF64App _ _) = knownNat

bvFloatAppWidth (F16ToUi32App _ _) = knownNat
bvFloatAppWidth (F16ToUi64App _ _) = knownNat
bvFloatAppWidth (F16ToI32App _ _) = knownNat
bvFloatAppWidth (F16ToI64App _ _) = knownNat
bvFloatAppWidth (F32ToUi32App _ _) = knownNat
bvFloatAppWidth (F32ToUi64App _ _) = knownNat
bvFloatAppWidth (F32ToI32App _ _) = knownNat
bvFloatAppWidth (F32ToI64App _ _) = knownNat
bvFloatAppWidth (F64ToUi32App _ _) = knownNat
bvFloatAppWidth (F64ToUi64App _ _) = knownNat
bvFloatAppWidth (F64ToI32App _ _) = knownNat
bvFloatAppWidth (F64ToI64App _ _) = knownNat

bvFloatAppWidth (F16ToF32App _ _) = knownNat
bvFloatAppWidth (F16ToF64App _ _) = knownNat
bvFloatAppWidth (F32ToF16App _ _) = knownNat
bvFloatAppWidth (F32ToF64App _ _) = knownNat
bvFloatAppWidth (F64ToF16App _ _) = knownNat
bvFloatAppWidth (F64ToF32App _ _) = knownNat

bvFloatAppWidth (F16RoundToIntApp _ _) = knownNat
bvFloatAppWidth F16AddApp {} = knownNat
bvFloatAppWidth F16SubApp {} = knownNat
bvFloatAppWidth F16MulApp {} = knownNat
bvFloatAppWidth F16MulAddApp {} = knownNat
bvFloatAppWidth F16DivApp {} = knownNat
bvFloatAppWidth F16RemApp {} = knownNat
bvFloatAppWidth (F16SqrtApp _ _) = knownNat
bvFloatAppWidth (F16EqApp _ _) = knownNat
bvFloatAppWidth (F16LeApp _ _) = knownNat
bvFloatAppWidth (F16LtApp _ _) = knownNat
bvFloatAppWidth (F16EqSignalingApp _ _) = knownNat
bvFloatAppWidth (F16LeQuietApp _ _) = knownNat
bvFloatAppWidth (F16LtQuietApp _ _) = knownNat
bvFloatAppWidth (F16IsSignalingNaNApp _) = knownNat

bvFloatAppWidth (F32RoundToIntApp _ _) = knownNat
bvFloatAppWidth F32AddApp {} = knownNat
bvFloatAppWidth F32SubApp {} = knownNat
bvFloatAppWidth F32MulApp {} = knownNat
bvFloatAppWidth F32MulAddApp {} = knownNat
bvFloatAppWidth F32DivApp {} = knownNat
bvFloatAppWidth F32RemApp {} = knownNat
bvFloatAppWidth (F32SqrtApp _ _) = knownNat
bvFloatAppWidth (F32EqApp _ _) = knownNat
bvFloatAppWidth (F32LeApp _ _) = knownNat
bvFloatAppWidth (F32LtApp _ _) = knownNat
bvFloatAppWidth (F32EqSignalingApp _ _) = knownNat
bvFloatAppWidth (F32LeQuietApp _ _) = knownNat
bvFloatAppWidth (F32LtQuietApp _ _) = knownNat
bvFloatAppWidth (F32IsSignalingNaNApp _) = knownNat

bvFloatAppWidth (F64RoundToIntApp _ _) = knownNat
bvFloatAppWidth F64AddApp {} = knownNat
bvFloatAppWidth F64SubApp {} = knownNat
bvFloatAppWidth F64MulApp {} = knownNat
bvFloatAppWidth F64MulAddApp {} = knownNat
bvFloatAppWidth F64DivApp {} = knownNat
bvFloatAppWidth F64RemApp {} = knownNat
bvFloatAppWidth (F64SqrtApp _ _) = knownNat
bvFloatAppWidth (F64EqApp _ _) = knownNat
bvFloatAppWidth (F64LeApp _ _) = knownNat
bvFloatAppWidth (F64LtApp _ _) = knownNat
bvFloatAppWidth (F64EqSignalingApp _ _) = knownNat
bvFloatAppWidth (F64LeQuietApp _ _) = knownNat
bvFloatAppWidth (F64LtQuietApp _ _) = knownNat
bvFloatAppWidth (F64IsSignalingNaNApp _) = knownNat

$(return [])

instance TestEquality expr => TestEquality (BVFloatApp expr) where
  testEquality = $(structuralTypeEquality [t|BVFloatApp|]
                   [ (AnyType `TypeApp` AnyType, [|testEquality|]) ])

instance TestEquality expr => Eq (BVFloatApp expr w) where
  (==) = \x y -> isJust (testEquality x y)

instance TestEquality expr => EqF (BVFloatApp expr) where
  eqF = (==)

instance OrdF expr => OrdF (BVFloatApp expr) where
  compareF = $(structuralTypeOrd [t|BVFloatApp|]
                [ (AnyType `TypeApp` AnyType, [|compareF|]) ])

instance OrdF expr => Ord (BVFloatApp expr w) where
  compare a b =
    case compareF a b of
      LTF -> LT
      EQF -> EQ
      GTF -> GT

instance FunctorFC BVFloatApp where
  fmapFC = fmapFCDefault

instance FoldableFC BVFloatApp where
  foldMapFC = foldMapFCDefault

-- TODO: Template haskell?
instance TraversableFC BVFloatApp where
  traverseFC f (Ui32ToF16App rm x) = Ui32ToF16App <$> f rm <*> f x
  traverseFC f (Ui32ToF32App rm x) = Ui32ToF32App <$> f rm <*> f x
  traverseFC f (Ui32ToF64App rm x) = Ui32ToF64App <$> f rm <*> f x
  traverseFC f (I32ToF16App rm x) = I32ToF16App <$> f rm <*> f x
  traverseFC f (I32ToF32App rm x) = I32ToF32App <$> f rm <*> f x
  traverseFC f (I32ToF64App rm x) = I32ToF64App <$> f rm <*> f x
  traverseFC f (Ui64ToF16App rm x) = Ui64ToF16App <$> f rm <*> f x
  traverseFC f (Ui64ToF32App rm x) = Ui64ToF32App <$> f rm <*> f x
  traverseFC f (Ui64ToF64App rm x) = Ui64ToF64App <$> f rm <*> f x
  traverseFC f (I64ToF16App rm x) = I64ToF16App <$> f rm <*> f x
  traverseFC f (I64ToF32App rm x) = I64ToF32App <$> f rm <*> f x
  traverseFC f (I64ToF64App rm x) = I64ToF64App <$> f rm <*> f x


  traverseFC f (F16ToUi32App rm x) = F16ToUi32App <$> f rm <*> f x
  traverseFC f (F16ToUi64App rm x) = F16ToUi64App <$> f rm <*> f x
  traverseFC f (F16ToI32App rm x) = F16ToI32App <$> f rm <*> f x
  traverseFC f (F16ToI64App rm x) = F16ToI64App <$> f rm <*> f x
  traverseFC f (F32ToUi32App rm x) = F32ToUi32App <$> f rm <*> f x
  traverseFC f (F32ToUi64App rm x) = F32ToUi64App <$> f rm <*> f x
  traverseFC f (F32ToI32App rm x) = F32ToI32App <$> f rm <*> f x
  traverseFC f (F32ToI64App rm x) = F32ToI64App <$> f rm <*> f x
  traverseFC f (F64ToUi32App rm x) = F64ToUi32App <$> f rm <*> f x
  traverseFC f (F64ToUi64App rm x) = F64ToUi64App <$> f rm <*> f x
  traverseFC f (F64ToI32App rm x) = F64ToI32App <$> f rm <*> f x
  traverseFC f (F64ToI64App rm x) = F64ToI64App <$> f rm <*> f x

  traverseFC f (F16ToF32App rm x) = F16ToF32App <$> f rm <*> f x
  traverseFC f (F16ToF64App rm x) = F16ToF64App <$> f rm <*> f x
  traverseFC f (F32ToF16App rm x) = F32ToF16App <$> f rm <*> f x
  traverseFC f (F32ToF64App rm x) = F32ToF64App <$> f rm <*> f x
  traverseFC f (F64ToF16App rm x) = F64ToF16App <$> f rm <*> f x
  traverseFC f (F64ToF32App rm x) = F64ToF32App <$> f rm <*> f x

  traverseFC f (F16RoundToIntApp rm x) = F16RoundToIntApp <$> f rm <*> f x
  traverseFC f (F16AddApp rm x y) = F16AddApp <$> f rm <*> f x <*> f y
  traverseFC f (F16SubApp rm x y) = F16SubApp <$> f rm <*> f x <*> f y
  traverseFC f (F16MulApp rm x y) = F16MulApp <$> f rm <*> f x <*> f y
  traverseFC f (F16MulAddApp rm x y z) = F16MulAddApp <$> f rm <*> f x <*> f y <*> f z
  traverseFC f (F16DivApp rm x y) = F16DivApp <$> f rm <*> f x <*> f y
  traverseFC f (F16RemApp rm x y) = F16RemApp <$> f rm <*> f x <*> f y
  traverseFC f (F16SqrtApp rm x) = F16SqrtApp <$> f rm <*> f x
  traverseFC f (F16EqApp x y) = F16EqApp <$> f x <*> f y
  traverseFC f (F16LeApp x y) = F16EqApp <$> f x <*> f y
  traverseFC f (F16LtApp x y) = F16EqApp <$> f x <*> f y
  traverseFC f (F16EqSignalingApp x y) = F16EqApp <$> f x <*> f y
  traverseFC f (F16LeQuietApp x y) = F16EqApp <$> f x <*> f y
  traverseFC f (F16LtQuietApp x y) = F16EqApp <$> f x <*> f y
  traverseFC f (F16IsSignalingNaNApp x) = F16IsSignalingNaNApp <$> f x

  traverseFC f (F32RoundToIntApp rm x) = F32RoundToIntApp <$> f rm <*> f x
  traverseFC f (F32AddApp rm x y) = F32AddApp <$> f rm <*> f x <*> f y
  traverseFC f (F32SubApp rm x y) = F32SubApp <$> f rm <*> f x <*> f y
  traverseFC f (F32MulApp rm x y) = F32MulApp <$> f rm <*> f x <*> f y
  traverseFC f (F32MulAddApp rm x y z) = F32MulAddApp <$> f rm <*> f x <*> f y <*> f z
  traverseFC f (F32DivApp rm x y) = F32DivApp <$> f rm <*> f x <*> f y
  traverseFC f (F32RemApp rm x y) = F32RemApp <$> f rm <*> f x <*> f y
  traverseFC f (F32SqrtApp rm x) = F32SqrtApp <$> f rm <*> f x
  traverseFC f (F32EqApp x y) = F32EqApp <$> f x <*> f y
  traverseFC f (F32LeApp x y) = F32EqApp <$> f x <*> f y
  traverseFC f (F32LtApp x y) = F32EqApp <$> f x <*> f y
  traverseFC f (F32EqSignalingApp x y) = F32EqApp <$> f x <*> f y
  traverseFC f (F32LeQuietApp x y) = F32EqApp <$> f x <*> f y
  traverseFC f (F32LtQuietApp x y) = F32EqApp <$> f x <*> f y
  traverseFC f (F32IsSignalingNaNApp x) = F32IsSignalingNaNApp <$> f x

  traverseFC f (F64RoundToIntApp rm x) = F64RoundToIntApp <$> f rm <*> f x
  traverseFC f (F64AddApp rm x y) = F64AddApp <$> f rm <*> f x <*> f y
  traverseFC f (F64SubApp rm x y) = F64SubApp <$> f rm <*> f x <*> f y
  traverseFC f (F64MulApp rm x y) = F64MulApp <$> f rm <*> f x <*> f y
  traverseFC f (F64MulAddApp rm x y z) = F64MulAddApp <$> f rm <*> f x <*> f y <*> f z
  traverseFC f (F64DivApp rm x y) = F64DivApp <$> f rm <*> f x <*> f y
  traverseFC f (F64RemApp rm x y) = F64RemApp <$> f rm <*> f x <*> f y
  traverseFC f (F64SqrtApp rm x) = F64SqrtApp <$> f rm <*> f x
  traverseFC f (F64EqApp x y) = F64EqApp <$> f x <*> f y
  traverseFC f (F64LeApp x y) = F64EqApp <$> f x <*> f y
  traverseFC f (F64LtApp x y) = F64EqApp <$> f x <*> f y
  traverseFC f (F64EqSignalingApp x y) = F64EqApp <$> f x <*> f y
  traverseFC f (F64LeQuietApp x y) = F64EqApp <$> f x <*> f y
  traverseFC f (F64LtQuietApp x y) = F64EqApp <$> f x <*> f y
  traverseFC f (F64IsSignalingNaNApp x) = F64IsSignalingNaNApp <$> f x

-- TODO: Fix SoftFloat's Enum instance
bvToRM :: BV.BV 3 -> RoundingMode
bvToRM (BV.BV 0) = RoundNearEven
bvToRM (BV.BV 1) = RoundMinMag
bvToRM (BV.BV 2) = RoundMin
bvToRM (BV.BV 3) = RoundMax
bvToRM (BV.BV 4) = RoundNearMaxMag
bvToRM (BV.BV 6) = RoundOdd
bvToRM _ = RoundNearEven -- rather than throwing an error, we default.

efToBV :: ExceptionFlags -> BV.BV 5
efToBV (ExceptionFlags ieFlag ufFlag ofFlag infFlag invFlag) =
  foldl' BV.or (BV.zero knownNat)
    [ toBV ieFlag
    , BV.shl knownNat (toBV ufFlag) 1
    , BV.shl knownNat (toBV ofFlag) 2
    , BV.shl knownNat (toBV infFlag) 3
    , BV.shl knownNat (toBV invFlag) 4
    ]
  where
    toBV :: (KnownNat w, 1 <= w) => Bool -> BV.BV w
    toBV True = BV.one knownNat
    toBV False = BV.zero knownNat

-- | Type class for expressions languages with floating point expressions.
class BVExpr expr => BVFloatExpr (expr :: Nat -> *) where
  floatAppExpr :: BVFloatApp expr w -> expr w

-- data PureBVFloatExpr w = PureBVApp (BVApp PureBVFloatExpr w)
--                        | PureBVFloatApp (BVFloatApp PureBVFloatExpr w)

-- instance BVExpr PureBVFloatExpr where
--   appExpr = PureBVApp

-- instance BVFloatExpr PureBVFloatExpr where
--   floatAppExpr = PureBVFloatApp

-- | concatenate result into a single 'BitVector'.
cr :: KnownNat w => Result (BV.BV w) -> BV.BV (5 + w)
cr (Result res flags) = BV.concat knownNat knownNat (efToBV flags) res

crb :: Result Bool -> BV.BV 6
crb (Result res flags) =
  BV.concat knownNat knownNat (efToBV flags) (BV.bool res)

-- | Evaluation of floating-point expressions.
evalBVFloatAppM :: Monad m
                => (forall w' . expr w' -> m (BV.BV w'))
                -> BVFloatApp expr w
                -> m (BV.BV w)
evalBVFloatAppM eval (Ui32ToF16App rmE xE) = cr <$> (bvUi32ToF16 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (Ui32ToF32App rmE xE) = cr <$> (bvUi32ToF32 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (Ui32ToF64App rmE xE) = cr <$> (bvUi32ToF64 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (I32ToF16App rmE xE) = cr <$> (bvI32ToF16 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (I32ToF32App rmE xE) = cr <$> (bvI32ToF32 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (I32ToF64App rmE xE) = cr <$> (bvI32ToF64 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (Ui64ToF16App rmE xE) = cr <$> (bvUi64ToF16 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (Ui64ToF32App rmE xE) = cr <$> (bvUi64ToF32 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (Ui64ToF64App rmE xE) = cr <$> (bvUi64ToF64 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (I64ToF16App rmE xE) = cr <$> (bvI64ToF16 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (I64ToF32App rmE xE) = cr <$> (bvI64ToF32 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (I64ToF64App rmE xE) = cr <$> (bvI64ToF64 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F16ToUi32App rmE xE) = cr <$> (bvF16ToUi32 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F16ToUi64App rmE xE) = cr <$> (bvF16ToUi64 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F16ToI32App rmE xE) = cr <$> (bvF16ToI32 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F16ToI64App rmE xE) = cr <$> (bvF16ToI64 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F32ToUi32App rmE xE) = cr <$> (bvF32ToUi32 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F32ToUi64App rmE xE) = cr <$> (bvF32ToUi64 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F32ToI32App rmE xE) = cr <$> (bvF32ToI32 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F32ToI64App rmE xE) = cr <$> (bvF32ToI64 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F64ToUi32App rmE xE) = cr <$> (bvF64ToUi32 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F64ToUi64App rmE xE) = cr <$> (bvF64ToUi64 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F64ToI32App rmE xE) = cr <$> (bvF64ToI32 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F64ToI64App rmE xE) = cr <$> (bvF64ToI64 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F16ToF32App rmE xE) = cr <$> (bvF16ToF32 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F16ToF64App rmE xE) = cr <$> (bvF16ToF64 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F32ToF16App rmE xE) = cr <$> (bvF32ToF16 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F32ToF64App rmE xE) = cr <$> (bvF32ToF64 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F64ToF16App rmE xE) = cr <$> (bvF64ToF16 <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F64ToF32App rmE xE) = cr <$> (bvF64ToF32 <$> (bvToRM <$> eval rmE) <*> eval xE)

evalBVFloatAppM eval (F16RoundToIntApp rmE xE) = cr <$> (bvF16RoundToInt <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F16AddApp rmE xE yE) = cr <$> (bvF16Add <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE)
evalBVFloatAppM eval (F16SubApp rmE xE yE) = cr <$> (bvF16Sub <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE)
evalBVFloatAppM eval (F16MulApp rmE xE yE) = cr <$> (bvF16Mul <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE)
evalBVFloatAppM eval (F16MulAddApp rmE xE yE zE) = cr <$> (bvF16MulAdd <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE <*> eval zE)
evalBVFloatAppM eval (F16DivApp rmE xE yE) = cr <$> (bvF16Div <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE)
evalBVFloatAppM eval (F16RemApp rmE xE yE) = cr <$> (bvF16Rem <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE)
evalBVFloatAppM eval (F16SqrtApp rmE xE) = cr <$> (bvF16Sqrt <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F16EqApp xE yE) = crb <$> (bvF16Eq <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F16LeApp xE yE) = crb <$> (bvF16Le <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F16LtApp xE yE) = crb <$> (bvF16Lt <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F16EqSignalingApp xE yE) = crb <$> (bvF16EqSignaling <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F16LeQuietApp xE yE) = crb <$> (bvF16LeQuiet <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F16LtQuietApp xE yE) = crb <$> (bvF16LtQuiet <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F16IsSignalingNaNApp xE) = crb <$> (bvF16IsSignalingNaN <$> eval xE)

evalBVFloatAppM eval (F32RoundToIntApp rmE xE) = cr <$> (bvF32RoundToInt <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F32AddApp rmE xE yE) = cr <$> (bvF32Add <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE)
evalBVFloatAppM eval (F32SubApp rmE xE yE) = cr <$> (bvF32Sub <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE)
evalBVFloatAppM eval (F32MulApp rmE xE yE) = cr <$> (bvF32Mul <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE)
evalBVFloatAppM eval (F32MulAddApp rmE xE yE zE) = cr <$> (bvF32MulAdd <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE <*> eval zE)
evalBVFloatAppM eval (F32DivApp rmE xE yE) = cr <$> (bvF32Div <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE)
evalBVFloatAppM eval (F32RemApp rmE xE yE) = cr <$> (bvF32Rem <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE)
evalBVFloatAppM eval (F32SqrtApp rmE xE) = cr <$> (bvF32Sqrt <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F32EqApp xE yE) = crb <$> (bvF32Eq <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F32LeApp xE yE) = crb <$> (bvF32Le <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F32LtApp xE yE) = crb <$> (bvF32Lt <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F32EqSignalingApp xE yE) = crb <$> (bvF32EqSignaling <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F32LeQuietApp xE yE) = crb <$> (bvF32LeQuiet <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F32LtQuietApp xE yE) = crb <$> (bvF32LtQuiet <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F32IsSignalingNaNApp xE) = crb <$> (bvF32IsSignalingNaN <$> eval xE)

evalBVFloatAppM eval (F64RoundToIntApp rmE xE) = cr <$> (bvF64RoundToInt <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F64AddApp rmE xE yE) = cr <$> (bvF64Add <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE)
evalBVFloatAppM eval (F64SubApp rmE xE yE) = cr <$> (bvF64Sub <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE)
evalBVFloatAppM eval (F64MulApp rmE xE yE) = cr <$> (bvF64Mul <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE)
evalBVFloatAppM eval (F64MulAddApp rmE xE yE zE) = cr <$> (bvF64MulAdd <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE <*> eval zE)
evalBVFloatAppM eval (F64DivApp rmE xE yE) = cr <$> (bvF64Div <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE)
evalBVFloatAppM eval (F64RemApp rmE xE yE) = cr <$> (bvF64Rem <$> (bvToRM <$> eval rmE) <*> eval xE <*> eval yE)
evalBVFloatAppM eval (F64SqrtApp rmE xE) = cr <$> (bvF64Sqrt <$> (bvToRM <$> eval rmE) <*> eval xE)
evalBVFloatAppM eval (F64EqApp xE yE) = crb <$> (bvF64Eq <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F64LeApp xE yE) = crb <$> (bvF64Le <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F64LtApp xE yE) = crb <$> (bvF64Lt <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F64EqSignalingApp xE yE) = crb <$> (bvF64EqSignaling <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F64LeQuietApp xE yE) = crb <$> (bvF64LeQuiet <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F64LtQuietApp xE yE) = crb <$> (bvF64LtQuiet <$> eval xE <*> eval yE)
evalBVFloatAppM eval (F64IsSignalingNaNApp xE) = crb <$> (bvF64IsSignalingNaN <$> eval xE)

evalBVFloatApp :: (forall w' . expr w' -> BV.BV w')
               -> BVFloatApp expr w
               -> BV.BV w
evalBVFloatApp eval app = runIdentity $ evalBVFloatAppM (return . eval) app

-- evalPureBVFloatExpr :: PureBVFloatExpr w -> BitVector w
-- evalPureBVFloatExpr (PureBVApp app) = evalBVApp evalPureBVFloatExpr app
-- evalPureBVFloatExpr (PureBVFloatApp app) = evalBVFloatApp evalPureBVFloatExpr app

-- Integer to float
ui32ToF16E :: BVFloatExpr expr => RM expr -> expr 32 -> expr 21
ui32ToF16E rmE e = floatAppExpr (Ui32ToF16App rmE e)

ui32ToF32E :: BVFloatExpr expr => RM expr -> expr 32 -> expr 37
ui32ToF32E rmE e = floatAppExpr (Ui32ToF32App rmE e)

ui32ToF64E :: BVFloatExpr expr => RM expr -> expr 32 -> expr 69
ui32ToF64E rmE e = floatAppExpr (Ui32ToF64App rmE e)

i32ToF16E :: BVFloatExpr expr => RM expr -> expr 32 -> expr 21
i32ToF16E rmE e = floatAppExpr (I32ToF16App rmE e)

i32ToF32E :: BVFloatExpr expr => RM expr -> expr 32 -> expr 37
i32ToF32E rmE e = floatAppExpr (I32ToF32App rmE e)

i32ToF64E :: BVFloatExpr expr => RM expr -> expr 32 -> expr 69
i32ToF64E rmE e = floatAppExpr (I32ToF64App rmE e)

ui64ToF16E :: BVFloatExpr expr => RM expr -> expr 64 -> expr 21
ui64ToF16E rmE e = floatAppExpr (Ui64ToF16App rmE e)

ui64ToF32E :: BVFloatExpr expr => RM expr -> expr 64 -> expr 37
ui64ToF32E rmE e = floatAppExpr (Ui64ToF32App rmE e)

ui64ToF64E :: BVFloatExpr expr => RM expr -> expr 64 -> expr 69
ui64ToF64E rmE e = floatAppExpr (Ui64ToF64App rmE e)

i64ToF16E :: BVFloatExpr expr => RM expr -> expr 64 -> expr 21
i64ToF16E rmE e = floatAppExpr (I64ToF16App rmE e)

i64ToF32E :: BVFloatExpr expr => RM expr -> expr 64 -> expr 37
i64ToF32E rmE e = floatAppExpr (I64ToF32App rmE e)

i64ToF64E :: BVFloatExpr expr => RM expr -> expr 64 -> expr 69
i64ToF64E rmE e = floatAppExpr (I64ToF64App rmE e)

-- Float to integer
f16ToUi32E :: BVFloatExpr expr => RM expr -> expr 16 -> expr 37
f16ToUi32E rmE e = floatAppExpr (F16ToUi32App rmE e)

f16ToUi64E :: BVFloatExpr expr => RM expr -> expr 16 -> expr 69
f16ToUi64E rmE e = floatAppExpr (F16ToUi64App rmE e)

f16ToI32E  :: BVFloatExpr expr => RM expr -> expr 16 -> expr 37
f16ToI32E rmE e = floatAppExpr (F16ToI32App rmE e)

f16ToI64E  :: BVFloatExpr expr => RM expr -> expr 16 -> expr 69
f16ToI64E rmE e = floatAppExpr (F16ToI64App rmE e)

f32ToUi32E :: BVFloatExpr expr => RM expr -> expr 32 -> expr 37
f32ToUi32E rmE e = floatAppExpr (F32ToUi32App rmE e)

f32ToUi64E :: BVFloatExpr expr => RM expr -> expr 32 -> expr 69
f32ToUi64E rmE e = floatAppExpr (F32ToUi64App rmE e)

f32ToI32E  :: BVFloatExpr expr => RM expr -> expr 32 -> expr 37
f32ToI32E rmE e = floatAppExpr (F32ToI32App rmE e)

f32ToI64E  :: BVFloatExpr expr => RM expr -> expr 32 -> expr 69
f32ToI64E rmE e = floatAppExpr (F32ToI64App rmE e)

f64ToUi32E :: BVFloatExpr expr => RM expr -> expr 64 -> expr 37
f64ToUi32E rmE e = floatAppExpr (F64ToUi32App rmE e)

f64ToUi64E :: BVFloatExpr expr => RM expr -> expr 64 -> expr 69
f64ToUi64E rmE e = floatAppExpr (F64ToUi64App rmE e)

f64ToI32E  :: BVFloatExpr expr => RM expr -> expr 64 -> expr 37
f64ToI32E rmE e = floatAppExpr (F64ToI32App rmE e)

f64ToI64E  :: BVFloatExpr expr => RM expr -> expr 64 -> expr 69
f64ToI64E rmE e = floatAppExpr (F64ToI64App rmE e)

-- Float to float
f16ToF32E :: BVFloatExpr expr => RM expr -> expr 16 -> expr 37
f16ToF32E rmE e = floatAppExpr (F16ToF32App rmE e)

f16ToF64E :: BVFloatExpr expr => RM expr -> expr 16 -> expr 69
f16ToF64E rmE e = floatAppExpr (F16ToF64App rmE e)

f32ToF16E :: BVFloatExpr expr => RM expr -> expr 32 -> expr 21
f32ToF16E rmE e = floatAppExpr (F32ToF16App rmE e)

f32ToF64E :: BVFloatExpr expr => RM expr -> expr 32 -> expr 69
f32ToF64E rmE e = floatAppExpr (F32ToF64App rmE e)

f64ToF16E :: BVFloatExpr expr => RM expr -> expr 64 -> expr 21
f64ToF16E rmE e = floatAppExpr (F64ToF16App rmE e)

f64ToF32E :: BVFloatExpr expr => RM expr -> expr 64 -> expr 37
f64ToF32E rmE e = floatAppExpr (F64ToF32App rmE e)

-- 16-bit operations
f16RoundToIntE :: BVFloatExpr expr => RM expr -> expr 16 -> expr 21
f16RoundToIntE rmE e = floatAppExpr (F16RoundToIntApp rmE e)

f16AddE :: BVFloatExpr expr => RM expr -> expr 16 -> expr 16 -> expr 21
f16AddE rmE e1 e2 = floatAppExpr (F16AddApp rmE e1 e2)

f16SubE :: BVFloatExpr expr => RM expr -> expr 16 -> expr 16 -> expr 21
f16SubE rmE e1 e2 = floatAppExpr (F16SubApp rmE e1 e2)

f16MulE :: BVFloatExpr expr => RM expr -> expr 16 -> expr 16 -> expr 21
f16MulE rmE e1 e2 = floatAppExpr (F16MulApp rmE e1 e2)

f16MulAddE :: BVFloatExpr expr => RM expr -> expr 16 -> expr 16 -> expr 16 -> expr 21
f16MulAddE rmE e1 e2 e3 = floatAppExpr (F16MulAddApp rmE e1 e2 e3)

f16DivE :: BVFloatExpr expr => RM expr -> expr 16 -> expr 16 -> expr 21
f16DivE rmE e1 e2 = floatAppExpr (F16DivApp rmE e1 e2)

f16RemE :: BVFloatExpr expr => RM expr -> expr 16 -> expr 16 -> expr 21
f16RemE rmE e1 e2 = floatAppExpr (F16RemApp rmE e1 e2)

f16SqrtE :: BVFloatExpr expr => RM expr -> expr 16 -> expr 21
f16SqrtE rmE e = floatAppExpr (F16SqrtApp rmE e)

f16EqE :: BVFloatExpr expr => expr 16 -> expr 16 -> expr 6
f16EqE e1 e2 = floatAppExpr (F16EqApp e1 e2)

f16LeE :: BVFloatExpr expr => expr 16 -> expr 16 -> expr 6
f16LeE e1 e2 = floatAppExpr (F16LeApp e1 e2)

f16LtE :: BVFloatExpr expr => expr 16 -> expr 16 -> expr 6
f16LtE e1 e2 = floatAppExpr (F16LtApp e1 e2)

f16EqSignalingE :: BVFloatExpr expr => expr 16 -> expr 16 -> expr 6
f16EqSignalingE e1 e2 = floatAppExpr (F16EqSignalingApp e1 e2)

f16LeQuietE :: BVFloatExpr expr => expr 16 -> expr 16 -> expr 6
f16LeQuietE e1 e2 = floatAppExpr (F16LeQuietApp e1 e2)

f16LtQuietE :: BVFloatExpr expr => expr 16 -> expr 16 -> expr 6
f16LtQuietE e1 e2 = floatAppExpr (F16LtQuietApp e1 e2)

f16IsSignalingNaNE :: BVFloatExpr expr => expr 16 -> expr 6
f16IsSignalingNaNE e = floatAppExpr (F16IsSignalingNaNApp e)

-- 32-bit operations
f32RoundToIntE :: BVFloatExpr expr => RM expr -> expr 32 -> expr 37
f32RoundToIntE rmE e = floatAppExpr (F32RoundToIntApp rmE e)

f32AddE :: BVFloatExpr expr => RM expr -> expr 32 -> expr 32 -> expr 37
f32AddE rmE e1 e2 = floatAppExpr (F32AddApp rmE e1 e2)

f32SubE :: BVFloatExpr expr => RM expr -> expr 32 -> expr 32 -> expr 37
f32SubE rmE e1 e2 = floatAppExpr (F32SubApp rmE e1 e2)

f32MulE :: BVFloatExpr expr => RM expr -> expr 32 -> expr 32 -> expr 37
f32MulE rmE e1 e2 = floatAppExpr (F32MulApp rmE e1 e2)

f32MulAddE :: BVFloatExpr expr => RM expr -> expr 32 -> expr 32 -> expr 32 -> expr 37
f32MulAddE rmE e1 e2 e3 = floatAppExpr (F32MulAddApp rmE e1 e2 e3)

f32DivE :: BVFloatExpr expr => RM expr -> expr 32 -> expr 32 -> expr 37
f32DivE rmE e1 e2 = floatAppExpr (F32DivApp rmE e1 e2)

f32RemE :: BVFloatExpr expr => RM expr -> expr 32 -> expr 32 -> expr 37
f32RemE rmE e1 e2 = floatAppExpr (F32RemApp rmE e1 e2)

f32SqrtE :: BVFloatExpr expr => RM expr -> expr 32 -> expr 37
f32SqrtE rmE e = floatAppExpr (F32SqrtApp rmE e)

f32EqE :: BVFloatExpr expr => expr 32 -> expr 32 -> expr 6
f32EqE e1 e2 = floatAppExpr (F32EqApp e1 e2)

f32LeE :: BVFloatExpr expr => expr 32 -> expr 32 -> expr 6
f32LeE e1 e2 = floatAppExpr (F32LeApp e1 e2)

f32LtE :: BVFloatExpr expr => expr 32 -> expr 32 -> expr 6
f32LtE e1 e2 = floatAppExpr (F32LtApp e1 e2)

f32EqSignalingE :: BVFloatExpr expr => expr 32 -> expr 32 -> expr 6
f32EqSignalingE e1 e2 = floatAppExpr (F32EqSignalingApp e1 e2)

f32LeQuietE :: BVFloatExpr expr => expr 32 -> expr 32 -> expr 6
f32LeQuietE e1 e2 = floatAppExpr (F32LeQuietApp e1 e2)

f32LtQuietE :: BVFloatExpr expr => expr 32 -> expr 32 -> expr 6
f32LtQuietE e1 e2 = floatAppExpr (F32LtQuietApp e1 e2)

f32IsSignalingNaNE :: BVFloatExpr expr => expr 32 -> expr 6
f32IsSignalingNaNE e = floatAppExpr (F32IsSignalingNaNApp e)

-- 64-bit operations
f64RoundToIntE :: BVFloatExpr expr => RM expr -> expr 64 -> expr 69
f64RoundToIntE rmE e = floatAppExpr (F64RoundToIntApp rmE e)

f64AddE :: BVFloatExpr expr => RM expr -> expr 64 -> expr 64 -> expr 69
f64AddE rmE e1 e2 = floatAppExpr (F64AddApp rmE e1 e2)

f64SubE :: BVFloatExpr expr => RM expr -> expr 64 -> expr 64 -> expr 69
f64SubE rmE e1 e2 = floatAppExpr (F64SubApp rmE e1 e2)

f64MulE :: BVFloatExpr expr => RM expr -> expr 64 -> expr 64 -> expr 69
f64MulE rmE e1 e2 = floatAppExpr (F64MulApp rmE e1 e2)

f64MulAddE :: BVFloatExpr expr => RM expr -> expr 64 -> expr 64 -> expr 64 -> expr 69
f64MulAddE rmE e1 e2 e3 = floatAppExpr (F64MulAddApp rmE e1 e2 e3)

f64DivE :: BVFloatExpr expr => RM expr -> expr 64 -> expr 64 -> expr 69
f64DivE rmE e1 e2 = floatAppExpr (F64DivApp rmE e1 e2)

f64RemE :: BVFloatExpr expr => RM expr -> expr 64 -> expr 64 -> expr 69
f64RemE rmE e1 e2 = floatAppExpr (F64RemApp rmE e1 e2)

f64SqrtE :: BVFloatExpr expr => RM expr -> expr 64 -> expr 69
f64SqrtE rmE e = floatAppExpr (F64SqrtApp rmE e)

f64EqE :: BVFloatExpr expr => expr 64 -> expr 64 -> expr 6
f64EqE e1 e2 = floatAppExpr (F64EqApp e1 e2)

f64LeE :: BVFloatExpr expr => expr 64 -> expr 64 -> expr 6
f64LeE e1 e2 = floatAppExpr (F64LeApp e1 e2)

f64LtE :: BVFloatExpr expr => expr 64 -> expr 64 -> expr 6
f64LtE e1 e2 = floatAppExpr (F64LtApp e1 e2)

f64EqSignalingE :: BVFloatExpr expr => expr 64 -> expr 64 -> expr 6
f64EqSignalingE e1 e2 = floatAppExpr (F64EqSignalingApp e1 e2)

f64LeQuietE :: BVFloatExpr expr => expr 64 -> expr 64 -> expr 6
f64LeQuietE e1 e2 = floatAppExpr (F64LeQuietApp e1 e2)

f64LtQuietE :: BVFloatExpr expr => expr 64 -> expr 64 -> expr 6
f64LtQuietE e1 e2 = floatAppExpr (F64LtQuietApp e1 e2)

f64IsSignalingNaNE :: BVFloatExpr expr => expr 64 -> expr 6
f64IsSignalingNaNE e = floatAppExpr (F64IsSignalingNaNApp e)


-- Miscellaneous

-- 32
f32Exp :: BVExpr expr => expr 32 -> expr 8
f32Exp = extractE (knownNat @23)

f32Sig :: BVExpr expr => expr 32 -> expr 23
f32Sig = extractE (knownNat @0)

f32Sgn :: BVExpr expr => expr 32 -> expr 1
f32Sgn = extractE (knownNat @31)

negate32 :: BVExpr expr => expr 32 -> expr 32
negate32 e = notE (f32Sgn e) `concatE` extractE' (knownNat @31) (knownNat @0) e

isNaN32 :: BVExpr expr => expr 32 -> expr 1
isNaN32 e = (f32Exp e `eqE` bvInteger 0xFF) `andE` notE (f32Sig e `eqE` bvInteger 0)

isSNaN32 :: BVExpr expr => expr 32 -> expr 1
isSNaN32 e = isNaN32 e `andE` notE (extractE (knownNat @22) e)

isQNaN32 :: BVExpr expr => expr 32 -> expr 1
isQNaN32 e = isNaN32 e `andE` extractE (knownNat @22) e

isSubnormal32 :: BVExpr expr => expr 32 -> expr 1
isSubnormal32 e = (f32Exp e `eqE` bvInteger 0x0) `andE` notE (isZero32 e)

isNormal32 :: BVExpr expr => expr 32 -> expr 1
isNormal32 e = (bvInteger 0x0 `ltuE` f32Exp e) `andE` (f32Exp e `ltuE` bvInteger 0xff)

canonicalNaN32 :: BVExpr expr => expr 32
canonicalNaN32 = bvInteger 0x7FC00000

posZero32 :: BVExpr expr => expr 32
posZero32 = bvInteger 0x00000000

negZero32 :: BVExpr expr => expr 32
negZero32 = bvInteger 0x80000000

isZero32 :: BVExpr expr => expr 32 -> expr 1
isZero32 e = (e `eqE` posZero32) `orE` (e `eqE` negZero32)

posInfinity32 :: BVExpr expr => expr 32
posInfinity32 = bvInteger 0x7F800000

negInfinity32 :: BVExpr expr => expr 32
negInfinity32 = bvInteger 0xFF800000

-- 64
f64Exp :: BVExpr expr => expr 64 -> expr 11
f64Exp = extractE (knownNat @52)

f64Sig :: BVExpr expr => expr 64 -> expr 52
f64Sig = extractE (knownNat @0)

f64Sgn :: BVExpr expr => expr 64 -> expr 1
f64Sgn = extractE (knownNat @63)

negate64 :: BVExpr expr => expr 64 -> expr 64
negate64 e = notE (f64Sgn e) `concatE` extractE' (knownNat @63) (knownNat @0) e

isNaN64 :: BVExpr expr => expr 64 -> expr 1
isNaN64 e = (f64Exp e `eqE` bvInteger 0x7FF) `andE` notE (f64Sig e `eqE` bvInteger 0)

isSNaN64 :: BVExpr expr => expr 64 -> expr 1
isSNaN64 e = isNaN64 e `andE` notE (extractE (knownNat @51) e)

isQNaN64 :: BVExpr expr => expr 64 -> expr 1
isQNaN64 e = isNaN64 e `andE` extractE (knownNat @51) e

isSubnormal64 :: BVExpr expr => expr 64 -> expr 1
isSubnormal64 e = (f64Exp e `eqE` bvInteger 0x0) `andE` notE (isZero64 e)

isNormal64 :: BVExpr expr => expr 64 -> expr 1
isNormal64 e = (bvInteger 0x0 `ltuE` f64Exp e) `andE` (f64Exp e `ltuE` bvInteger 0x7ff)

canonicalNaN64 :: BVExpr expr => expr 64
canonicalNaN64 = bvInteger 0x7FF8000000000000

posZero64 :: BVExpr expr => expr 64
posZero64 = bvInteger 0x0000000000000000

negZero64 :: BVExpr expr => expr 64
negZero64 = bvInteger 0x8000000000000000

isZero64 :: BVExpr expr => expr 64 -> expr 1
isZero64 e = (e `eqE` posZero64) `orE` (e `eqE` negZero64)

posInfinity64 :: BVExpr expr => expr 64
posInfinity64 = bvInteger 0x7FF0000000000000

negInfinity64 :: BVExpr expr => expr 64
negInfinity64 = bvInteger 0xFFF0000000000000

getFRes :: forall w expr . (KnownNat w, BVExpr expr) => expr (5 + w) -> (expr w, expr 5)
getFRes e = let wRepr = knownNat @w
            in (extractE (knownNat @0) e, extractE wRepr e)
