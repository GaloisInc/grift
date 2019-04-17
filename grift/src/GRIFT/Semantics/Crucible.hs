{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module GRIFT.Semantics.Crucible
  ( translateApp
  ) where

import Control.Monad.Fail
import Data.BitVector.Sized.App as BV
import Lang.Crucible.CFG.Expr
import Lang.Crucible.Syntax
import Lang.Crucible.Types

import Prelude hiding (fail)

withPosNat :: MonadFail m => NatRepr w -> ((1 <= w) => m a) -> m a
withPosNat wRepr a = case isZeroOrGT1 wRepr of
  Left Refl -> fail $ "Illegal NatRepr 0"
  Right LeqProof -> a

-- | Translate from a 'BVApp', GRIFT's core bitvector expression type, to a Crucible
-- expression.
translateApp :: (Monad m, MonadFail m, BVExpr bvExpr, IsExpr expr)
             => (forall w' . bvExpr w' -> m (expr (BVType w')))
             -- ^ subexpression translator
             -> BVApp bvExpr w
             -> m (expr (BVType w))
translateApp trans (AndApp wRepr e1 e2) = withPosNat wRepr $
  app <$> (BVAnd wRepr <$> trans e1 <*> trans e2)
translateApp trans (OrApp wRepr e1 e2) = withPosNat wRepr $
  app <$> (BVOr wRepr <$> trans e1 <*> trans e2)
translateApp trans (XorApp wRepr e1 e2) = withPosNat wRepr $
  app <$> (BVXor wRepr <$> trans e1 <*> trans e2)
translateApp trans (NotApp wRepr e) = withPosNat wRepr $
  app <$> (BVNot wRepr <$> trans e)
translateApp trans (SllApp wRepr e1 e2) = withPosNat wRepr $
  app <$> (BVShl wRepr <$> trans e1 <*> trans e2)
translateApp trans (SrlApp wRepr e1 e2) = withPosNat wRepr $
  app <$> (BVLshr wRepr <$> trans e1 <*> trans e2)
translateApp trans (SraApp wRepr e1 e2) = withPosNat wRepr $
  app <$> (BVAshr wRepr <$> trans e1 <*> trans e2)
translateApp trans (AddApp wRepr e1 e2) = withPosNat wRepr $
  app <$> (BVAdd wRepr <$> trans e1 <*> trans e2)
translateApp trans (SubApp wRepr e1 e2) = withPosNat wRepr $
  app <$> (BVSub wRepr <$> trans e1 <*> trans e2)
translateApp trans (MulApp wRepr e1 e2) = withPosNat wRepr $
  app <$> (BVMul wRepr <$> trans e1 <*> trans e2)
translateApp trans (QuotUApp wRepr e1 e2) = withPosNat wRepr $
  app <$> (BVUdiv wRepr <$> trans e1 <*> trans e2)
translateApp trans (QuotSApp wRepr e1 e2) = withPosNat wRepr $
  app <$> (BVSdiv wRepr <$> trans e1 <*> trans e2)
translateApp trans (RemUApp wRepr e1 e2) = withPosNat wRepr $
  app <$> (BVUrem wRepr <$> trans e1 <*> trans e2)
translateApp trans (RemSApp wRepr e1 e2) = withPosNat wRepr $
  app <$> (BVSrem wRepr <$> trans e1 <*> trans e2)
translateApp trans (NegateApp wRepr e) = withPosNat wRepr $
  app <$> (BVNeg wRepr <$> trans e)
translateApp trans (AbsApp wRepr e) = withPosNat wRepr $ do
  e' <- trans e
  return $ app $ BVIte (app (BVSlt wRepr e' (app (BVLit wRepr 0)))) wRepr
    (app (BVNeg wRepr e'))
    e'
translateApp trans (SignumApp wRepr e) = withPosNat wRepr $ do
  e' <- trans e
  return $ app $ BoolToBV wRepr (app (BVSlt wRepr e' (app (BVLit wRepr 0))))
translateApp trans (EqApp e1 e2) = withPosNat (exprWidth e1) $ do
  e1' <- trans e1
  e2' <- trans e2
  return $ app $ BoolToBV knownNat (app (BVEq (exprWidth e1) e1' e2'))
translateApp trans (LtuApp e1 e2) = withPosNat (exprWidth e1) $ do
  e1' <- trans e1
  e2' <- trans e2
  return $ app $ BoolToBV knownNat (app (BVUlt (exprWidth e1) e1' e2'))
translateApp trans (LtsApp e1 e2) = withPosNat (exprWidth e1) $ do
  e1' <- trans e1
  e2' <- trans e2
  return $ app $ BoolToBV knownNat (app (BVSlt (exprWidth e1) e1' e2'))
translateApp trans (ZExtApp wRepr e) = withPosNat (exprWidth e) $ withPosNat wRepr $ do
  e' <- trans e
  case compareNat (exprWidth e) wRepr of
    NatLT _ -> return $ app $ BVZext wRepr (exprWidth e) e'
    NatEQ -> trans e
    NatGT _ -> return $ app $ BVTrunc wRepr (exprWidth e) e'
translateApp trans (SExtApp wRepr e) = withPosNat (exprWidth e) $ withPosNat wRepr $ do
  e' <- trans e
  case compareNat (exprWidth e) wRepr of
    NatLT _ -> return $ app $ BVSext wRepr (exprWidth e) e'
    NatEQ -> trans e
    NatGT _ -> return $ app $ BVTrunc wRepr (exprWidth e) e'
translateApp trans (ExtractApp wRepr ixRepr e) = withPosNat wRepr $ withPosNat (exprWidth e) $ do
  e' <- trans e
  case testLeq (ixRepr `addNat` wRepr) (exprWidth e) of
    Just LeqProof -> return $ app $ BVSelect ixRepr wRepr (exprWidth e) e'
    _ -> fail "extraction exceeded bounds"
translateApp trans (ConcatApp _ e1 e2) =
  withPosNat (exprWidth e1) $
  withPosNat (exprWidth e2) $
  withLeqProof (leqAddPos (exprWidth e1) (exprWidth e2)) $
  app <$> (BVConcat (exprWidth e1) (exprWidth e2) <$> trans e1 <*> trans e2)
translateApp trans (IteApp wRepr t e1 e2) = withPosNat wRepr $ do
  t' <- trans t
  e1' <- trans e1
  e2' <- trans e2
  return $ app $ BVIte (app (BVEq knownRepr t' (app (BVLit knownRepr 1)))) wRepr e1' e2'

-- -- | Translate from a 'BVApp', GRIFT's core bitvector expression type, to a What4
-- -- expression.
-- translateAppWhat4 :: (IsExprBuilder sym, BV.BVExpr bvExpr)
--              => sym
--              -> (forall w' . bvExpr w' -> IO (SymBV sym w'))
--              -- ^ subterm translator
--              -> BVApp bvExpr w
--              -- ^ 'BVApp' type from GRIFT semantics
--              -> IO (SymBV sym w)
--              -- ^ What4 'App' type
-- translateAppWhat4 sym trans (AndApp wRepr e1 e2) = withPosNat wRepr $
--   join (bvAndBits sym <$> trans e1 <*> trans e2)
-- translateAppWhat4 sym trans (OrApp wRepr e1 e2) = withPosNat wRepr $
--   join (bvOrBits sym <$> trans e1 <*> trans e2)
-- translateAppWhat4 sym trans (XorApp wRepr e1 e2) = withPosNat wRepr $
--   join (bvXorBits sym <$> trans e1 <*> trans e2)
-- translateAppWhat4 sym trans (NotApp wRepr e) = withPosNat wRepr $
--   join (bvNotBits sym <$> trans e)
-- translateAppWhat4 sym trans (SllApp wRepr e1 e2) = withPosNat wRepr $
--   join (bvShl sym <$> trans e1 <*> trans e2)
-- translateAppWhat4 sym trans (SrlApp wRepr e1 e2) = withPosNat wRepr $
--   join (bvLshr sym <$> trans e1 <*> trans e2)
-- translateAppWhat4 sym trans (SraApp wRepr e1 e2) = withPosNat wRepr $
--   join (bvAshr sym <$> trans e1 <*> trans e2)
-- translateAppWhat4 sym trans (AddApp wRepr e1 e2) = withPosNat wRepr $
--   join (bvAdd sym <$> trans e1 <*> trans e2)
-- translateAppWhat4 sym trans (SubApp wRepr e1 e2) = withPosNat wRepr $
--   join (bvSub sym <$> trans e1 <*> trans e2)
-- translateAppWhat4 sym trans (MulApp wRepr e1 e2) = withPosNat wRepr $
--   join (bvMul sym <$> trans e1 <*> trans e2)
-- translateAppWhat4 sym trans (QuotUApp wRepr e1 e2) = withPosNat wRepr $
--   join (bvUdiv sym <$> trans e1 <*> trans e2)
-- translateAppWhat4 sym trans (QuotSApp wRepr e1 e2) = withPosNat wRepr $
--   join (bvSdiv sym <$> trans e1 <*> trans e2)
-- translateAppWhat4 sym trans (RemUApp wRepr e1 e2) = withPosNat wRepr $
--   join (bvUrem sym <$> trans e1 <*> trans e2)
-- translateAppWhat4 sym trans (RemSApp wRepr e1 e2) = withPosNat wRepr $
--   join (bvSrem sym <$> trans e1 <*> trans e2)
-- translateAppWhat4 sym trans (NegateApp wRepr e) = withPosNat wRepr $
--   join (bvNeg sym <$> trans e)
-- translateAppWhat4 sym trans (AbsApp wRepr e) = withPosNat wRepr $ do
--   bv <- trans e
--   i <- bvToInteger sym bv
--   absi <- intAbs sym i
--   integerToBV sym absi wRepr
-- translateAppWhat4 sym trans (SignumApp wRepr e) = withPosNat wRepr $ do
--   lt <- join (bvSlt sym <$> trans e <*> bvLit sym wRepr 0)
--   predToBV sym lt wRepr
-- translateAppWhat4 sym trans (EqApp e1 e2) = withPosNat (exprWidth e1) $ do
--   eq <- join (bvEq sym <$> trans e1 <*> trans e2)
--   predToBV sym eq knownNat
-- translateAppWhat4 sym trans (LtuApp e1 e2) = withPosNat (exprWidth e1) $ do
--   lt <- join (bvUlt sym <$> trans e1 <*> trans e2)
--   predToBV sym lt knownNat
-- translateAppWhat4 sym trans (LtsApp e1 e2) = withPosNat (exprWidth e1) $ do
--   lt <- join (bvSlt sym <$> trans e1 <*> trans e2)
--   predToBV sym lt knownNat
-- translateAppWhat4 sym trans (ZExtApp wRepr e) = withPosNat (exprWidth e) $ withPosNat wRepr $
--   case compareNat (exprWidth e) wRepr of
--     NatLT _ -> join (bvZext sym wRepr <$> trans e)
--     NatEQ -> trans e
--     NatGT _ -> join (bvTrunc sym wRepr <$> trans e)
-- translateAppWhat4 sym trans (SExtApp wRepr e) = withPosNat (exprWidth e) $ withPosNat wRepr $
--   case compareNat (exprWidth e) wRepr of
--     NatLT _ -> join (bvSext sym wRepr <$> trans e)
--     NatEQ -> trans e
--     NatGT _ -> join (bvTrunc sym wRepr <$> trans e)
-- translateAppWhat4 sym trans (ExtractApp wRepr ixRepr e) = withPosNat wRepr $
--   case testLeq (ixRepr `addNat` wRepr) (exprWidth e) of
--     Just LeqProof -> join (bvSelect sym ixRepr wRepr <$> trans e)
--     _ -> error "extraction exceeded bounds"
-- translateAppWhat4 sym trans (ConcatApp _ e1 e2) = withPosNat (exprWidth e1) $ withPosNat (exprWidth e2) $
--   join (bvConcat sym <$> trans e1 <*> trans e2)
-- translateAppWhat4 sym trans (IteApp _ t e1 e2) = withPosNat (exprWidth e1) $ do
--   tBool <- join (bvEq sym <$> trans t <*> bvLit sym knownNat 0)
--   join (bvIte sym tBool <$> trans e1 <*> trans e2)
