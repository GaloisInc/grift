{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module GRIFT.Semantics.Crucible where

import Control.Monad (join)
import Control.Monad.IO.Class
import Data.BitVector.Sized.App as BV
import Data.Parameterized.TraversableFC
import What4.BaseTypes
import What4.Expr
import What4.Interface

withPosNat :: NatRepr w -> ((1 <= w) => a) -> a
withPosNat wRepr a = case isZeroOrGT1 wRepr of
  Left Refl -> error $ "Illegal NatRepr 0"
  Right LeqProof -> a

-- | Translate from a 'BVApp', GRIFT's core bitvector expression type, to a What4
-- expression.
translateApp :: (IsExprBuilder sym, BV.BVExpr bvExpr)
             => sym
             -> (forall w' . bvExpr w' -> IO (SymBV sym w'))
             -- ^ subterm translator
             -> BVApp bvExpr w
             -- ^ 'BVApp' type from GRIFT semantics
             -> IO (SymBV sym w)
             -- ^ What4 'App' type
translateApp sym trans (AndApp wRepr e1 e2) = withPosNat wRepr $
  join (bvAndBits sym <$> trans e1 <*> trans e2)
translateApp sym trans (OrApp wRepr e1 e2) = withPosNat wRepr $
  join (bvOrBits sym <$> trans e1 <*> trans e2)
translateApp sym trans (XorApp wRepr e1 e2) = withPosNat wRepr $
  join (bvXorBits sym <$> trans e1 <*> trans e2)
translateApp sym trans (NotApp wRepr e) = withPosNat wRepr $
  join (bvNotBits sym <$> trans e)
translateApp sym trans (SllApp wRepr e1 e2) = withPosNat wRepr $
  join (bvShl sym <$> trans e1 <*> trans e2)
translateApp sym trans (SrlApp wRepr e1 e2) = withPosNat wRepr $
  join (bvLshr sym <$> trans e1 <*> trans e2)
translateApp sym trans (SraApp wRepr e1 e2) = withPosNat wRepr $
  join (bvAshr sym <$> trans e1 <*> trans e2)
translateApp sym trans (AddApp wRepr e1 e2) = withPosNat wRepr $
  join (bvAdd sym <$> trans e1 <*> trans e2)
translateApp sym trans (SubApp wRepr e1 e2) = withPosNat wRepr $
  join (bvSub sym <$> trans e1 <*> trans e2)
translateApp sym trans (MulApp wRepr e1 e2) = withPosNat wRepr $
  join (bvMul sym <$> trans e1 <*> trans e2)
translateApp sym trans (QuotUApp wRepr e1 e2) = withPosNat wRepr $
  join (bvUdiv sym <$> trans e1 <*> trans e2)
translateApp sym trans (QuotSApp wRepr e1 e2) = withPosNat wRepr $
  join (bvSdiv sym <$> trans e1 <*> trans e2)
translateApp sym trans (RemUApp wRepr e1 e2) = withPosNat wRepr $
  join (bvUrem sym <$> trans e1 <*> trans e2)
translateApp sym trans (RemSApp wRepr e1 e2) = withPosNat wRepr $
  join (bvSrem sym <$> trans e1 <*> trans e2)
translateApp sym trans (NegateApp wRepr e) = withPosNat wRepr $
  join (bvNeg sym <$> trans e)
translateApp sym trans (AbsApp wRepr e) = withPosNat wRepr $ do
  bv <- trans e
  i <- bvToInteger sym bv
  absi <- intAbs sym i
  integerToBV sym absi wRepr
translateApp sym trans (SignumApp wRepr e) = withPosNat wRepr $ do
  lt <- join (bvSlt sym <$> trans e <*> bvLit sym wRepr 0)
  predToBV sym lt wRepr
translateApp sym trans (EqApp e1 e2) = withPosNat (exprWidth e1) $ do
  eq <- join (bvEq sym <$> trans e1 <*> trans e2)
  predToBV sym eq knownNat
translateApp sym trans (LtuApp e1 e2) = withPosNat (exprWidth e1) $ do
  lt <- join (bvUlt sym <$> trans e1 <*> trans e2)
  predToBV sym lt knownNat
translateApp sym trans (LtsApp e1 e2) = withPosNat (exprWidth e1) $ do
  lt <- join (bvSlt sym <$> trans e1 <*> trans e2)
  predToBV sym lt knownNat
translateApp sym trans (ZExtApp wRepr e) = withPosNat (exprWidth e) $ withPosNat wRepr $
  case compareNat (exprWidth e) wRepr of
    NatLT _ -> join (bvZext sym wRepr <$> trans e)
    NatEQ -> trans e
    NatGT _ -> join (bvTrunc sym wRepr <$> trans e)
translateApp sym trans (SExtApp wRepr e) = withPosNat (exprWidth e) $ withPosNat wRepr $
  case compareNat (exprWidth e) wRepr of
    NatLT _ -> join (bvSext sym wRepr <$> trans e)
    NatEQ -> trans e
    NatGT _ -> join (bvTrunc sym wRepr <$> trans e)
  -- ExtractApp :: NatRepr w' -> Int -> !(expr w) -> BVApp expr w'
  -- ConcatApp  :: !(NatRepr (w+w')) -> !(expr w) -> !(expr w') -> BVApp expr (w+w')

  -- -- Other operations
  -- IteApp :: !(NatRepr w) -> !(expr 1) -> !(expr w) -> !(expr w) -> BVApp expr w
