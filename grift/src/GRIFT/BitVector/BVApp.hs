{-|
Module      : GRIFT.BitVector.BVApp
Copyright   : (c) Galois Inc. 2018
License     : BSD-3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module exports a type, 'BVApp', to aid in building expression languages over
'BitVector's. Let @expr :: Nat -> *@ be some ADT of /expressions/ that yield
'BitVector's when evaluated. Then, given one or more values of type @expr w@
(i.e. one or more of these evaluatable expressions), 'BVApp' provides the various
constructors necessary for creating compound expressions involving pure 'BitVector'
operations. The @expr@ type can (and often will) include a constructor of type @BVApp
expr w -> expr w@ in order to create a recursive expression language.

In addition to the 'BVApp' type, we provide an evaluator which, given a function
mapping values of type @expr w@ to 'BitVector's, will evaluate the compound
'BVApp' expressions.
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module GRIFT.BitVector.BVApp
  ( BVApp(..)
  , evalBVApp
  , evalBVAppM
  , bvAppWidth
  -- * Smart constructors
  , BVExpr(..)
  , mkBV'
  , bvExpr
  , bvInteger
  -- ** Bitwise
  , andE
  , orE
  , xorE
  , notE
  -- ** Arithmetic
  , addE
  , subE
  , mulE
  , quotuE
  , quotsE
  , remuE
  , remsE
  , negateE
  , absE
  , signumE
  , sllE
  , srlE
  , sraE
  -- ** Comparison
  , eqE
  , ltuE
  , ltsE
  -- ** Width-changing
  , ashr'
  , sextE, sextE', sextOrId, sextEOrId, sextEOrId'
  , zextE, zextE', zextOrId, zextEOrId, zextEOrId'
  , extractE, extractE'
  , concatE
  -- ** Control
  , iteE
  ) where

import Control.Monad.Identity ( Identity(runIdentity) )
import qualified Data.BitVector.Sized as BV
import Data.Parameterized
import Data.Parameterized.TH.GADT
    ( TypePat(AnyType, TypeApp),
      structuralTraversal,
      structuralTypeEquality,
      structuralTypeOrd,
    )
import GHC.Natural ( Natural )
import GHC.TypeLits ( KnownNat, Nat )

import Debug.Trace

-- | Represents the application of a 'BitVector' operation to one or more
-- subexpressions.
data BVApp (expr :: Nat -> *) (w :: Nat) where

  -- Bitwise operations
  AndApp :: !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  OrApp  :: !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  XorApp :: !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  NotApp :: !(NatRepr w) -> !(expr w) -> BVApp expr w

  -- Shifts
  SllApp :: !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  SrlApp :: !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  SraApp :: (1 <= w) => !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w

  -- Arithmetic operations
  AddApp   :: !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  SubApp   :: !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  MulApp   :: !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  QuotUApp :: !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  QuotSApp :: (1 <= w) => !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  RemUApp  :: !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  RemSApp  :: (1 <= w) => !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr w
  NegateApp :: !(NatRepr w) -> !(expr w) -> BVApp expr w
  AbsApp   :: (1 <= w) => !(NatRepr w) -> !(expr w) -> BVApp expr w
  SignumApp :: (1 <= w) => !(NatRepr w) -> !(expr w) -> BVApp expr w

  -- Comparisons
  EqApp  :: !(expr w) -> !(expr w) -> BVApp expr 1
  LtuApp :: !(expr w) -> !(expr w) -> BVApp expr 1
  LtsApp :: (1 <= w) => !(NatRepr w) -> !(expr w) -> !(expr w) -> BVApp expr 1

  -- Width-changing
  ZExtApp :: (w + 1 <= w') => !(NatRepr w') -> !(expr w) -> BVApp expr w'
  SExtApp ::
    (1 <= w, (w + 1) <= w') =>
    !(NatRepr w) -> !(NatRepr w') -> !(expr w) -> BVApp expr w'
  ExtractApp :: !(NatRepr w') -> !(NatRepr ix) -> !(expr w) -> BVApp expr w'
  ConcatApp :: !(NatRepr w) -> !(NatRepr w') -> !(expr w) -> !(expr w') -> BVApp expr (w + w')

  -- Other operations
  IteApp :: !(NatRepr w) -> !(expr 1) -> !(expr w) -> !(expr w) -> BVApp expr w

bvAppWidth :: BVApp expr w -> NatRepr w
bvAppWidth (AndApp wRepr _ _) = wRepr
bvAppWidth (OrApp wRepr _ _) = wRepr
bvAppWidth (XorApp wRepr _ _) = wRepr
bvAppWidth (NotApp wRepr _) = wRepr

bvAppWidth (SllApp wRepr _ _) = wRepr
bvAppWidth (SrlApp wRepr _ _) = wRepr
bvAppWidth (SraApp wRepr _ _) = wRepr

bvAppWidth (AddApp wRepr _ _) = wRepr
bvAppWidth (SubApp wRepr _ _) = wRepr
bvAppWidth (MulApp wRepr _ _) = wRepr
bvAppWidth (QuotUApp wRepr _ _) = wRepr
bvAppWidth (QuotSApp wRepr _ _) = wRepr
bvAppWidth (RemUApp wRepr _ _) = wRepr
bvAppWidth (RemSApp wRepr _ _) = wRepr
bvAppWidth (NegateApp wRepr _) = wRepr
bvAppWidth (AbsApp wRepr _) = wRepr
bvAppWidth (SignumApp wRepr _) = wRepr

bvAppWidth (EqApp _ _) = knownNat
bvAppWidth (LtuApp _ _) = knownNat
bvAppWidth LtsApp {} = knownNat

bvAppWidth (ZExtApp wRepr _) = wRepr
bvAppWidth (SExtApp _ w' _) = w'
bvAppWidth (ExtractApp wRepr _ _) = wRepr
bvAppWidth (ConcatApp w w' _ _) = addNat w w'

bvAppWidth (IteApp wRepr _ _ _) = wRepr

$(return [])

instance TestEquality expr => TestEquality (BVApp expr) where
  testEquality = $(structuralTypeEquality [t|BVApp|]
                   [ (AnyType `TypeApp` AnyType, [|testEquality|]) ])

instance TestEquality expr => Eq (BVApp expr w) where
  (==) = \x y -> isJust (testEquality x y)

instance TestEquality expr => EqF (BVApp expr) where
  eqF = (==)

instance OrdF expr => OrdF (BVApp expr) where
  compareF = $(structuralTypeOrd [t|BVApp|]
                [ (AnyType `TypeApp` AnyType, [|compareF|]) ])

instance OrdF expr => Ord (BVApp expr w) where
  compare a b =
    case compareF a b of
      LTF -> LT
      EQF -> EQ
      GTF -> GT

instance FunctorFC BVApp where
  fmapFC = fmapFCDefault

instance FoldableFC BVApp where
  foldMapFC = foldMapFCDefault

instance TraversableFC BVApp where
  traverseFC = $(structuralTraversal [t|BVApp|] [])

-- | Evaluate a 'BVApp' given a monadic evaluation function for the parameterized type @expr@.
evalBVAppM :: Monad m
           => (forall w' . expr w' -> m (BV.BV w')) -- ^ expression evaluator
           -> BVApp expr w                              -- ^ application
           -> m (BV.BV w)
evalBVAppM eval (AndApp _ e1 e2) = BV.and <$> eval e1 <*> eval e2
evalBVAppM eval (OrApp  _ e1 e2) = BV.or  <$> eval e1 <*> eval e2
evalBVAppM eval (XorApp _ e1 e2) = BV.xor <$> eval e1 <*> eval e2
evalBVAppM eval (NotApp w e)     = BV.complement w <$> eval e
evalBVAppM eval (AddApp w e1 e2) = BV.add w <$> eval e1 <*> eval e2
evalBVAppM eval (SubApp w e1 e2) = BV.add w <$> eval e1 <*> (BV.negate w <$> eval e2)
evalBVAppM eval (SllApp w e1 e2) = BV.shl w <$> eval e1 <*> (fromIntegral . BV.asUnsigned <$> eval e2)
evalBVAppM eval (SrlApp w e1 e2) = BV.lshr w <$> eval e1 <*> (fromIntegral . BV.asUnsigned <$> eval e2)
evalBVAppM eval (SraApp w e1 e2) = BV.ashr w <$> eval e1 <*> (fromIntegral . BV.asUnsigned <$> eval e2)
evalBVAppM eval (MulApp w e1 e2) = BV.mul w <$> eval e1 <*> eval e2
evalBVAppM eval (QuotSApp w e1 e2) = BV.squot w <$> eval e1 <*> eval e2
evalBVAppM eval (QuotUApp _ e1 e2) = BV.uquot <$> eval e1 <*> eval e2
evalBVAppM eval (RemSApp  w e1 e2) = BV.srem w <$> eval e1 <*> eval e2
evalBVAppM eval (RemUApp  _ e1 e2) = BV.urem <$> eval e1 <*> eval e2
evalBVAppM eval (NegateApp w e) = BV.negate w <$> eval e
evalBVAppM eval (AbsApp w e) = BV.abs w <$> eval e
evalBVAppM eval (SignumApp w e) = BV.signum w <$> eval e
evalBVAppM eval (EqApp  e1 e2) = BV.bool <$> ((==)  <$> eval e1 <*> eval e2)
evalBVAppM eval (LtuApp e1 e2) = BV.bool <$> (BV.ult <$> eval e1 <*> eval e2)
evalBVAppM eval (LtsApp w e1 e2) = BV.bool <$> (BV.slt w <$> eval e1 <*> eval e2)
evalBVAppM eval (ZExtApp w' e) = BV.zext w' <$> eval e
evalBVAppM eval (SExtApp w w' e) = BV.sext w w' <$> eval e
evalBVAppM eval (ExtractApp w ix e) = BV.select' (natValue ix) w <$> eval e
evalBVAppM eval (ConcatApp w w' e1 e2) = BV.concat w w' <$> eval e1 <*> eval e2
evalBVAppM eval (IteApp _ eTest eT eF) = do
  cond <- BV.testBit (knownNat @0) <$> eval eTest
  if cond then eval eT else eval eF

-- | Evaluate a 'BVApp' given a pure evaluation function for the parameterized type @expr@.
evalBVApp :: (forall w' . expr w' -> BV.BV w') -- ^ expression evaluator
          -> BVApp expr w                          -- ^ application
          -> BV.BV w
evalBVApp eval bvApp = runIdentity $ evalBVAppM (return . eval) bvApp

-- | Typeclass for embedding 'BVApp' constructors into larger expression types.
class BVExpr (expr :: Nat -> *) where
  litBV :: NatRepr w -> BV.BV w -> expr w
  exprWidth :: expr w -> NatRepr w
  appExpr :: BVApp expr w -> expr w

mkBV' :: KnownNat w => Integer -> BV.BV w
mkBV' = BV.mkBV knownNat

bvExpr :: (BVExpr expr, KnownNat w) => BV.BV w -> expr w
bvExpr = litBV knownNat

bvInteger :: (BVExpr expr, KnownNat w) => Integer -> expr w
bvInteger = bvExpr . mkBV'


-- -- TODO: finish
-- instance (BVExpr expr) => Num (BVApp expr w) where
--   app1 + app2 = AddApp (appExpr app1) (appExpr app2)
--   app1 * app2 = MulApp (appExpr app1) (appExpr app2)
--   abs app = AbsApp (appExpr app)
--   signum app = SignumApp (appExpr app)
--   fromInteger = undefined
--   negate app = NegateApp (appExpr app)
--   app1 - app2 = SubApp (appExpr app1) (appExpr app2)

-- -- TODO: finish
-- instance (KnownNat w, BVExpr expr, TestEquality expr) => Bits (BVApp expr w) where
--   app1 .&. app2 = AndApp (appExpr app1) (appExpr app2)
--   app1 .|. app2 = OrApp (appExpr app1) (appExpr app2)
--   app1 `xor` app2 = XorApp (appExpr app1) (appExpr app2)
--   complement app = NotApp (appExpr app)
--   shiftL = undefined
--   shiftR = undefined
--   rotate = undefined
--   bitSize = undefined
--   bitSizeMaybe = undefined
--   isSigned = undefined
--   testBit = undefined
--   bit = undefined
--   popCount = undefined

-- | Bitwise and.
andE :: BVExpr expr => expr w -> expr w -> expr w
andE e1 e2 = appExpr (AndApp (exprWidth e1) e1 e2)

-- | Bitwise or.
orE :: BVExpr expr => expr w -> expr w -> expr w
orE e1 e2 = appExpr (OrApp (exprWidth e1) e1 e2)

-- | Bitwise xor.
xorE :: BVExpr expr => expr w -> expr w -> expr w
xorE e1 e2 = appExpr (XorApp (exprWidth e1) e1 e2)

-- | Bitwise not.
notE :: BVExpr expr => expr w -> expr w
notE e = appExpr (NotApp (exprWidth e) e)

-- | Add two expressions.
addE :: BVExpr expr => expr w -> expr w -> expr w
addE e1 e2 = appExpr (AddApp (exprWidth e1) e1 e2)

-- | Subtract the second expression from the first.
subE :: BVExpr expr => expr w -> expr w -> expr w
subE e1 e2 = appExpr (SubApp (exprWidth e1) e1 e2)

-- | Signed multiply two 'BitVector's, doubling the width of the result to hold all
-- arithmetic overflow bits.
mulE :: BVExpr expr => expr w -> expr w -> expr w
mulE e1 e2 = appExpr (MulApp (exprWidth e1) e1 e2)

-- | Signed divide two 'BitVector's, rounding to zero.
quotsE :: (BVExpr expr, 1 <= w) => expr w -> expr w -> expr w
quotsE e1 e2 = appExpr (QuotSApp (exprWidth e1) e1 e2)

-- | Unsigned divide two 'BitVector's, rounding to zero.
quotuE :: BVExpr expr => expr w -> expr w -> expr w
quotuE e1 e2 = appExpr (QuotUApp (exprWidth e1) e1 e2)

-- | Remainder after signed division of two 'BitVector's, when rounded to zero.
remsE :: (BVExpr expr, 1 <= w) => expr w -> expr w -> expr w
remsE e1 e2 = appExpr (RemSApp (exprWidth e1) e1 e2)

-- | Remainder after unsigned division of two 'BitVector's, when rounded to zero.
remuE :: BVExpr expr => expr w -> expr w -> expr w
remuE e1 e2 = appExpr (RemUApp (exprWidth e1) e1 e2)

negateE :: BVExpr expr => expr w -> expr w
negateE e = appExpr (NegateApp (exprWidth e) e)

absE :: (BVExpr expr, 1 <= w) => expr w -> expr w
absE e = appExpr (AbsApp (exprWidth e) e)

signumE :: (BVExpr expr, 1 <= w) => expr w -> expr w
signumE e = appExpr (SignumApp (exprWidth e) e)

-- | Left logical shift the first expression by the second.
sllE :: BVExpr expr => expr w -> expr w -> expr w
sllE e1 e2 = appExpr (SllApp (exprWidth e1) e1 e2)

-- | Left logical shift the first expression by the second.
srlE :: BVExpr expr => expr w -> expr w -> expr w
srlE e1 e2 = appExpr (SrlApp (exprWidth e1) e1 e2)

-- | Left logical shift the first expression by the second.
sraE :: (BVExpr expr, 1 <= w) => expr w -> expr w -> expr w
sraE e1 e2 = appExpr (SraApp (exprWidth e1) e1 e2)

-- | Test for equality of two expressions.
eqE :: BVExpr expr => expr w -> expr w -> expr 1
eqE e1 e2 = appExpr (EqApp e1 e2)

-- | Signed less than
ltsE :: (BVExpr expr, KnownNat w, 1 <= w) => expr w -> expr w -> expr 1
ltsE e1 e2 = appExpr (LtsApp knownNat e1 e2)

-- | Unsigned less than
ltuE :: BVExpr expr => expr w -> expr w -> expr 1
ltuE e1 e2 = appExpr (LtuApp e1 e2)

-- | A version of ashr that doesn't require the width to be non-null
-- TODO: should this be in bv-sized?
ashr' :: forall w. KnownNat w => BV.BV w -> Natural -> BV.BV w
ashr' v n =
  case testLeq (knownNat @1) (knownNat @w) of
    Just LeqProof -> BV.ashr knownNat v n
    Nothing -> v

-- | Zero-extension when the sizes differ, identity otherwise
-- TODO: should this be in bv-sized?
zextOrId :: forall w w'.
  (KnownNat w, KnownNat w', w <= w') =>
  BV.BV w -> BV.BV w'
zextOrId v =
  case testStrictLeq @w @w' knownNat knownNat of
    Left LeqProof -> BV.zext knownNat v
    Right Refl -> v

-- | Zero-extension
zextE ::
  (BVExpr expr, KnownNat w, KnownNat w', w + 1 <= w') =>
  expr w -> expr w'
zextE e = appExpr (ZExtApp knownNat e)

-- | Zero-extension when the sizes differ, identity otherwise
zextEOrId :: forall expr w w'.
  (BVExpr expr, KnownNat w, KnownNat w', w <= w') =>
  expr w -> expr w'
zextEOrId e =
  case testStrictLeq @w @w' knownNat knownNat of
    Left LeqProof -> appExpr (ZExtApp knownNat e)
    Right Refl -> e

-- | Zero-extension with an explicit width argument
zextE' :: ((w + 1) <= w') => BVExpr expr => NatRepr w' -> expr w -> expr w'
zextE' repr e = appExpr (ZExtApp repr e)

-- | Zero-extension when the sizes differ, identity otherwise
zextEOrId' :: forall expr w w'.
  (BVExpr expr, w <= w') =>
  NatRepr w -> NatRepr w' -> expr w -> expr w'
zextEOrId' w w' e =
  case testStrictLeq w w' of
    Left LeqProof -> appExpr (ZExtApp w' e)
    Right Refl -> e

-- | Sign-extension when the sizes differ, identity otherwise
sextOrId :: forall w w'.
  (KnownNat w, KnownNat w', w <= w', 1 <= w) =>
  BV.BV w -> BV.BV w'
sextOrId v =
  case testStrictLeq @w @w' knownNat knownNat of
    Left LeqProof -> BV.sext knownNat knownNat v
    Right Refl -> v

-- | Sign-extension
sextE :: forall expr w w'.
  (BVExpr expr, KnownNat w, KnownNat w', 1 <= w, w + 1 <= w') =>
  expr w -> expr w'
sextE e = appExpr (SExtApp knownNat knownNat e)

-- | Sign-extension when the sizes differ, identity otherwise
sextEOrId :: forall expr w w'.
  (BVExpr expr, KnownNat w, KnownNat w', 1 <= w, w <= w') =>
  expr w -> expr w'
sextEOrId e =
  case testStrictLeq @w @w' knownNat knownNat of
    Left LeqProof -> sextE e
    Right Refl -> e

-- | Sign-extension with explicit width arguments
sextE' ::
  (BVExpr expr, 1 <= w, (w + 1) <= w') =>
  NatRepr w -> NatRepr w' -> expr w -> expr w'
sextE' w w' e = appExpr (SExtApp w w' e)

-- | Sign-extension with explicit width arguments
sextEOrId' ::
  (BVExpr expr, 1 <= w, w <= w') =>
  NatRepr w -> NatRepr w' -> expr w -> expr w'
sextEOrId' w w' e =
  case testStrictLeq w w' of
    Left LeqProof -> sextE' w w' e
    Right Refl -> e

-- | Extract bits
extractE :: (BVExpr expr, KnownNat w') => NatRepr ix -> expr w -> expr w'
extractE ixRepr e = appExpr (ExtractApp knownNat ixRepr e)

-- | Extract bits with an explicit width argument
extractE' :: BVExpr expr => NatRepr w' -> NatRepr ix -> expr w -> expr w'
extractE' wRepr ixRepr e = appExpr (ExtractApp wRepr ixRepr e)

-- | Concatenation
concatE :: BVExpr expr => expr w -> expr w' -> expr (w+w')
concatE e1 e2 = appExpr (ConcatApp (exprWidth e1) (exprWidth e2) e1 e2)

-- | Conditional branch.
iteE :: BVExpr expr => expr 1 -> expr w -> expr w -> expr w
iteE t e1 e2 = appExpr (IteApp (exprWidth e1) t e1 e2)
