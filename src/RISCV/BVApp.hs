{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
Module      : RISCV.BVApp
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

An expression language over 'BitVector's.
-}

module RISCV.BVApp
  ( BVApp(..)
  , evalBVApp
  , evalBVAppM
  ) where

import Control.Monad.Identity
import Data.BitVector.Sized
import Data.Parameterized
import Foreign.Marshal.Utils (fromBool)
import GHC.TypeLits

-- | BitVector expressions.
data BVApp (f :: Nat -> *) (w :: Nat) where
  -- Literal BitVector
  LitBVApp :: BitVector w -> BVApp f w

  -- Bitwise operations
  AndApp :: !(f w) -> !(f w) -> BVApp f w
  OrApp  :: !(f w) -> !(f w) -> BVApp f w
  XorApp :: !(f w) -> !(f w) -> BVApp f w
  NotApp :: !(f w) -> BVApp f w

  -- Arithmetic operations
  AddApp :: !(f w) -> !(f w) -> BVApp f w
  SubApp :: !(f w) -> !(f w) -> BVApp f w
  MulSApp :: !(f w) -> !(f w) -> BVApp f (w+w)
  MulUApp :: !(f w) -> !(f w) -> BVApp f (w+w)
  MulSUApp :: !(f w) -> !(f w) -> BVApp f (w+w)
  DivUApp :: !(f w) -> !(f w) -> BVApp f w
  DivSApp :: !(f w) -> !(f w) -> BVApp f w
  RemUApp :: !(f w) -> !(f w) -> BVApp f w
  RemSApp :: !(f w) -> !(f w) -> BVApp f w

  -- TODO: Should we allow the shifter operand to have any width? This would simplify
  -- the semantics, but might make it more complicated to interpret.
  -- Shifts
  SllApp :: !(f w) -> !(f w) -> BVApp f w
  SrlApp :: !(f w) -> !(f w) -> BVApp f w
  SraApp :: !(f w) -> !(f w) -> BVApp f w

  -- Comparisons
  EqApp  :: !(f w) -> !(f w) -> BVApp f 1
  LtuApp :: !(f w) -> !(f w) -> BVApp f 1
  LtsApp :: !(f w) -> !(f w) -> BVApp f 1

  -- Width-changing
  ZExtApp :: NatRepr w' -> !(f w) -> BVApp f w'
  SExtApp :: NatRepr w' -> !(f w) -> BVApp f w'
  ExtractApp :: NatRepr w' -> Int -> !(f w) -> BVApp f w'
  ConcatApp :: !(f w) -> !(f w') -> BVApp f (w+w')

  -- Other operations
  IteApp :: !(f 1)
       -> !(f w)
       -> !(f w)
       -> BVApp f w

-- | Evaluate a BVApp given a monadic evaluation function for the parameterized type f.
evalBVAppM :: Monad m => (forall w' . f w' -> m (BitVector w')) -> BVApp f w -> m (BitVector w)
evalBVAppM _ (LitBVApp bv) = return bv
evalBVAppM eval (AndApp e1 e2) = bvAnd <$> eval e1 <*> eval e2
evalBVAppM eval (OrApp e1 e2) = bvOr <$> eval e1 <*> eval e2
evalBVAppM eval (XorApp e1 e2) = bvXor <$> eval e1 <*> eval e2
evalBVAppM eval (NotApp e) = bvComplement <$> eval e
evalBVAppM eval (AddApp e1 e2) = bvAdd <$> eval e1 <*> eval e2
evalBVAppM eval (SubApp e1 e2) = bvAdd <$> eval e1 <*> (bvNegate <$> eval e2)
evalBVAppM eval (MulSApp e1 e2) = bvMulFS <$> eval e1 <*> eval e2
evalBVAppM eval (MulUApp e1 e2) = bvMulFU <$> eval e1 <*> eval e2
evalBVAppM eval (MulSUApp e1 e2) = bvMulFSU <$> eval e1 <*> eval e2
evalBVAppM eval (DivSApp e1 e2) = bvQuotS <$> eval e1 <*> eval e2
evalBVAppM eval (DivUApp e1 e2) = bvQuotU <$> eval e1 <*> eval e2
evalBVAppM eval (RemSApp e1 e2) = bvRemS <$> eval e1 <*> eval e2
evalBVAppM eval (RemUApp e1 e2) = bvRemU <$> eval e1 <*> eval e2
evalBVAppM eval (SllApp e1 e2) = bvShiftL <$> eval e1 <*> (fromIntegral <$> bvIntegerU <$> eval e2)
evalBVAppM eval (SrlApp e1 e2) = bvShiftRL <$> eval e1 <*> (fromIntegral <$> bvIntegerU <$> eval e2)
evalBVAppM eval (SraApp e1 e2) = bvShiftRA <$> eval e1 <*> (fromIntegral <$> bvIntegerU <$> eval e2)
evalBVAppM eval (EqApp e1 e2) = fromBool <$> ((==) <$> eval e1 <*> eval e2)
evalBVAppM eval (LtuApp e1 e2) = fromBool <$> (bvLTU <$> eval e1 <*> eval e2)
evalBVAppM eval (LtsApp e1 e2) = fromBool <$> (bvLTS <$> eval e1 <*> eval e2)
evalBVAppM eval (ZExtApp wRepr e) = bvZextWithRepr wRepr <$> eval e
evalBVAppM eval (SExtApp wRepr e) = bvSextWithRepr wRepr <$> eval e
evalBVAppM eval (ExtractApp wRepr base e) = bvExtractWithRepr wRepr base <$> eval e
evalBVAppM eval (ConcatApp e1 e2) = do
  e1Val <- eval e1
  e2Val <- eval e2
  return $ e1Val `bvConcat` e2Val
evalBVAppM eval (IteApp teste te fe) = do
  testVal <- eval teste
  tVal <- eval te
  fVal <- eval fe
  return $ if testVal == 1 then tVal else fVal

-- | Evaluate a BVApp given a pure evaluation function for the parameterized type f.
evalBVApp :: (forall w' . f w' -> BitVector w') -> BVApp f w -> BitVector w
evalBVApp eval bvApp = runIdentity $ evalBVAppM (return . eval) bvApp
