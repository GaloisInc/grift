{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


{-|
Module      : RISCV.Semantics
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Several types and functions enabling specification of semantics for RISC-V
instructions.
-}

-- TODO: get pretty printing into a separate entity rather than using Show.
module RISCV.Semantics
  ( -- * Types
    Arch(..)
  , ArchWidth
  , Parameter
  , BVExpr
  , Stmt
  , Formula
  -- * Semantics monad
  , Semantics
  , runSemantics
  , comment
  , litBV
  , regRead
  , memRead
  , add
  , ite
  , assignReg
  , assignMem
  , assignPC
  , condStmt
  , param
  ) where

import Control.Lens ( (%=) )
import Control.Lens.TH (makeLenses)
import Control.Monad.State
import Data.BitVector.Sized
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Parameterized
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import GHC.TypeLits

-- | Architecture types
data Arch = RV32
          | RV64

-- | Maps an architecture to its register width
type family ArchWidth (arch :: Arch) :: Nat where
  ArchWidth 'RV32 = 32
  ArchWidth 'RV64 = 64

----------------------------------------
-- Expressions, statements, and formulas

-- | Formula parameter (represents unknown operands)
data Parameter (arch :: Arch) (w :: Nat)
  = Parameter (NatRepr w) String

instance Show (Parameter arch w) where
  show (Parameter _repr name) = name
instance ShowF (Parameter arch)

-- | BitVector expressions. These are the building blocks for semantic formulas for
-- an instruction.
data BVExpr (arch :: Arch) (w :: Nat) where
  -- Basic constructors
  LitBV :: BitVector w -> BVExpr arch w
  ParamBV :: Parameter arch w -> BVExpr arch w

  -- Accessing state
  RegRead :: BVExpr arch 5 -> BVExpr arch (ArchWidth arch)
  MemRead :: BVExpr arch (ArchWidth arch)
          -> BVExpr arch (ArchWidth arch)

  -- Arithmetic operations
  Add :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w

  -- Other operations
  Ite :: BVExpr arch 1
      -> BVExpr arch w
      -> BVExpr arch w
      -> BVExpr arch w

instance Show (BVExpr arch w) where
  show (LitBV bv) = show bv
  show (ParamBV p) = show p
  show (RegRead r) = "x[" ++ show r ++ "]"
  show (MemRead addr) = "M[" ++ show addr ++ "]"
  show (Add e1 e2) = show e1 ++ " + " ++ show e2
  show (Ite t e1 e2) =
    "if (" ++ show t ++ ") then " ++ show e1 ++ " else " ++ show e2
instance ShowF (BVExpr arch)

-- | A 'Stmt' represents an atomic state transformation -- typically, an assignment
-- of a state component (register, memory location, etc.) to a 'BVExpr' of the
-- appropriate width.
data Stmt (arch :: Arch) where
  AssignReg :: BVExpr arch 5 -> BVExpr arch (ArchWidth arch) -> Stmt arch
  AssignMem :: BVExpr arch (ArchWidth arch)
            -> BVExpr arch (ArchWidth arch)
            -> Stmt arch
  AssignPC  :: BVExpr arch (ArchWidth arch) -> Stmt arch

  CondStmt  :: BVExpr arch 1 -> Stmt arch -> Stmt arch

instance Show (Stmt arch) where
  show (AssignReg r e) = "x[" ++ show r ++ "] = " ++ show e
  show (AssignMem addr e) = "M[" ++ show addr ++ "] = " ++ show e
  show (AssignPC pc) = "pc = " ++ show pc
  show (CondStmt t stmt) = "if (" ++ show t ++ ") " ++ show stmt

-- TODO: Parameterize Formula and Semantics by instruction format. Have
-- special-purpose param functions for each format, that return the parameters as a
-- tuple.
-- | Formula representing the semantics of an instruction. A formula has a number of
-- parameters (potentially zero), which represent the input to the formula. These are
-- going to the be the operands of the instruction -- register ids, immediate values,
-- and so forth.
--
-- At some point, we can think about how to make this more granular; each statement
-- should be, more or less, an SSA-style definition where the right hand sides of all
-- the assignments are composed of atomic operations.
data Formula arch = Formula { _fComment :: Seq String
                              -- ^ multiline comment
                            , _fParams  :: Seq (Some (Parameter arch))
                              -- ^ the inputs to the formula
                            , _fDef     :: Seq (Stmt arch)
                              -- ^ sequence of statements defining the formula
                            }
makeLenses ''Formula

instance Show (Formula arch) where
  show (Formula comments params defs) =
    showComments ++
    "Parameters: " ++ showParams ++ "\n" ++
    showDefs
    where showComments = concat (toList ((++ "\n") <$> comments))
          showParams = intercalate ", " (toList (show <$> params))
          showDefs = concat (toList ((\d -> "  " ++ show d ++ "\n") <$> defs))

-- | Every definition begins with the empty formula.
emptyFormula :: Formula arch
emptyFormula = Formula Seq.empty Seq.empty Seq.empty

-- | State monad for defining instruction semantics. When defining an instruction,
-- you shouldn't need to ever read the state directly, so we only export the type.
newtype Semantics arch a =
  Semantics { unSemantics :: State (Formula arch) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadState (Formula arch))

-- | Obtain the formula defined by a 'Semantics' action.
runSemantics :: Semantics arch () -> Formula arch
runSemantics = flip execState emptyFormula . unSemantics

-- | Add a comment.
comment :: String -> Semantics arch ()
comment c = fComment %= \cs -> cs Seq.|> c

-- | Literal bit vector.
litBV :: BitVector w -> Semantics arch (BVExpr arch w)
litBV = return . LitBV

-- | Read a register.
regRead :: BVExpr arch 5 -> Semantics arch (BVExpr arch (ArchWidth arch))
regRead = return . RegRead

-- | Read a memory location.
memRead :: BVExpr arch (ArchWidth arch)
        -> Semantics arch (BVExpr arch (ArchWidth arch))
memRead = return . MemRead

-- | Add two expressions.
add :: BVExpr arch w
    -> BVExpr arch w
    -> Semantics arch (BVExpr arch w)
add e1 e2 = return (Add e1 e2)

-- | Conditional branch.
ite :: BVExpr arch 1
    -> BVExpr arch w
    -> BVExpr arch w
    -> Semantics arch (BVExpr arch w)
ite t e1 e2 = return (Ite t e1 e2)

-- | Add a statement to the formula.
addStmt :: Stmt arch -> Semantics arch ()
addStmt stmt = fDef %= \stmts -> stmts Seq.|> stmt

-- TODO: protect against multiple assignments? (for all of the assign* functions)
-- | Add a register assignment to the formula.
assignReg :: BVExpr arch 5
          -> BVExpr arch (ArchWidth arch)
          -> Semantics arch ()
assignReg r e = addStmt (AssignReg r e)

-- TODO: Should we allow arbitrary width assignments?
-- | Add a memory location assignment to the formula.
assignMem :: BVExpr arch (ArchWidth arch)
          -> BVExpr arch (ArchWidth arch)
          -> Semantics arch ()
assignMem addr val = addStmt (AssignMem addr val)

-- | Add a PC assignment to the formula.
assignPC :: BVExpr arch (ArchWidth arch) -> Semantics arch ()
assignPC pc = addStmt (AssignPC pc)

-- | Add a conditional assignment to the formula.
condStmt :: BVExpr arch 1 -> Stmt arch -> Semantics arch ()
condStmt cond stmt = addStmt (CondStmt cond stmt)

-- TODO: check that the parameter has not already been declared.
-- | Declare a parameter for use in the formula.
param :: KnownNat w => String -> Semantics arch (BVExpr arch w)
param s = do
  let p = Parameter knownNat s
  fParams %= \params -> params Seq.|> (Some p)
  return (ParamBV p)
