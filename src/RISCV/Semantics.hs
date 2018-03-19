{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module RISCV.Semantics where

import Data.BitVector.Sized
import Data.Parameterized.NatRepr
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import GHC.TypeLits

data Arch = RV32
          | RV64

data BVExprType (w :: Nat) where
  TBV :: NatRepr w -> BVExprType w

type family ArchWidth (arch :: Arch) :: Nat where
  ArchWidth 'RV32 = 32
  ArchWidth 'RV64 = 64

data Parameter (arch :: Arch) (w :: Nat)
  = Parameter String

type RegId arch = Parameter arch 5

data BVExpr (arch :: Arch) (w :: Nat) where
  -- Basic constructors
  LitBV :: BitVector w -> BVExpr arch w
  ParamBV :: NatRepr w -> Parameter arch w -> BVExpr arch w

  -- Accessing state
  RegRead :: RegId arch -> BVExpr arch (ArchWidth arch)
  MemRead :: BVExpr arch (ArchWidth arch)
          -> BVExpr arch (ArchWidth arch)

  -- Arithmetic operations
  Add :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w

  -- Other operations
  Ite :: BVExpr arch 1
      -> BVExpr arch w
      -> BVExpr arch w
      -> BVExpr arch w

data Stmt (arch :: Arch) where
  AssignReg :: RegId arch -> BVExpr arch (ArchWidth arch) -> Stmt arch
  AssignMem :: BVExpr arch (ArchWidth arch)
            -> BVExpr arch (ArchWidth arch)
            -> Stmt arch
  AssignPC  :: BVExpr arch (ArchWidth arch) -> Stmt arch

  AssignCond :: BVExpr arch 1 -> Stmt arch -> Stmt arch


