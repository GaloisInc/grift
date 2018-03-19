{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module RISCV.Semantics where

import Data.BitVector.Sized
import Data.Parameterized.NatRepr
import GHC.TypeLits

data BVExprType (w :: Nat) where
  TBV :: NatRepr w -> BVExprType w

data BVExpr w where
  LitBV :: BitVector w -> BVExpr w
  ParamBV :: NatRepr w -> String -> BVExpr w
