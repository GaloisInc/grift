{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : RISCV.Semantics.Helpers
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Helper functions for defining instruction semantics.
-}

module RISCV.Semantics.Helpers
  ( incrPC
  ) where

import GHC.TypeLits

import RISCV.Instruction
import RISCV.Semantics

incrPC :: KnownNat (ArchWidth arch) => FormulaBuilder arch fmt ()
incrPC = do
  ib' <- instBytes
  ib <- zextE ib'

  pc <- pcRead
  new_pc <- pc `addE` ib

  assignPC new_pc
