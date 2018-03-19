{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module RISCV.Semantics where

import Data.BitVector.Sized
import Data.Parameterized.NatRepr
import GHC.TypeLits

type RegId = BitVector 5

data Arch = RV32
          | RV64

data BVExprType (w :: Nat) where
  TBV :: NatRepr w -> BVExprType w

type family ArchWidth (arch :: Arch) :: Nat where
  ArchWidth 'RV32 = 32
  ArchWidth 'RV64 = 64

data BVExpr (arch :: Arch) (w :: Nat) where
  -- Basic constructors
  LitBV :: BitVector w -> BVExpr arch w
  ParamBV :: NatRepr w -> String -> BVExpr arch w

  -- Accessing state
  RegRead :: RegId -> BVExpr arch (ArchWidth arch)
  MemRead :: BVExpr arch (ArchWidth arch)
          -> BVExpr arch (ArchWidth arch)

  -- Arithmetic operations
  Add :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w

data Stmt (arch :: Arch) where
  AssignReg :: RegId -> BVExpr arch (ArchWidth arch) -> Stmt arch
  AssignMem :: BVExpr arch (ArchWidth arch)
            -> BVExpr arch (ArchWidth arch)
            -> Stmt arch
  AssignPC  :: BVExpr arch (ArchWidth arch) -> Stmt arch

