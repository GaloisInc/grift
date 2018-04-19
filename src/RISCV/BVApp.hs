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
  ) where

import Data.BitVector.Sized
import Data.Parameterized
import GHC.TypeLits

-- | BitVector expressions. These are the building blocks for semantic formulas for
-- an instruction.
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

-- | Evaluate a BVApp given an evaluation function for the parameterized type f.
evalBVApp :: Monad m => (forall w' . f w' -> m (BitVector w')) -> BVApp f w -> m (BitVector w)
evalBVApp _ (LitBVApp bv) = return bv
