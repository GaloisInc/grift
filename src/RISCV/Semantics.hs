{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}

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

module RISCV.Semantics
  ( -- * Types for semantic formulas
    LocExpr(..)
  , Expr(..)
  , Stmt(..)
  , Formula, fComments, fDefs
    -- * FormulaBuilder monad
  , FormulaBuilder
  , getFormula
    -- * FormulaBuilder operations
    -- ** Auxiliary
  , comment
  , operandEs
  , instBytes
  , litBV
    -- ** Access to state
  , readPC
  , readReg
  , readMem
  , readCSR
  , readPriv
    -- ** State actions
  , assignPC
  , assignReg
  , assignMem
  , assignCSR
  , assignPriv
  , branch
  , ($>)
  ) where

import Control.Lens ( (%=), (^.), Simple, Lens, lens )
import Control.Monad.State
import Data.Parameterized
import Data.Parameterized.List
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import GHC.TypeLits

import Data.BitVector.Sized.App
import RISCV.Types

----------------------------------------
-- Expressions, statements, and formulas

-- | This type represents an abstract component of the global state, and should be
-- used both for building expressions in our expression language and for interpreting
-- those expressions.
data LocExpr arch fmt w where
  PCExpr   ::                                   LocExpr arch fmt (ArchWidth arch)
  RegExpr  :: Expr arch fmt 5                -> LocExpr arch fmt (ArchWidth arch)
  MemExpr  :: Expr arch fmt (ArchWidth arch) -> LocExpr arch fmt 8
  CSRExpr  :: Expr arch fmt 12               -> LocExpr arch fmt (ArchWidth arch)
  PrivExpr ::                                   LocExpr arch fmt 2

-- | Expressions for computations over the RISC-V machine state.
data Expr (arch :: BaseArch) (fmt :: Format) (w :: Nat) where
  -- Accessing the instruction
  OperandExpr :: !(OperandID fmt w) -> Expr arch fmt w
  InstBytes :: Expr arch fmt (ArchWidth arch)

  -- Accessing state
  LocExpr :: LocExpr arch fmt w -> Expr arch fmt w

  -- BVApp with Expr subexpressions
  AppExpr :: !(BVApp (Expr arch fmt) w) -> Expr arch fmt w

-- | A 'Stmt' represents an atomic state transformation -- typically, an assignment
-- of a state component (register, memory location, etc.) to a 'Expr' of the
-- appropriate width.
data Stmt (arch :: BaseArch) (fmt :: Format) where
  -- | Assign a piece of state to a value.
  AssignStmt :: !(LocExpr arch fmt w) -> !(Expr arch fmt w) -> Stmt arch fmt
  -- | If-then-else branch statement.
  BranchStmt :: !(Expr arch fmt 1)
             -> !(Seq (Stmt arch fmt))
             -> !(Seq (Stmt arch fmt))
             -> Stmt arch fmt

-- | Formula representing the semantics of an instruction. A formula has a number of
-- operands (potentially zero), which represent the input to the formula. These are
-- going to the be the operands of the instruction -- register ids, immediate values,
-- and so forth.
--
-- Each definition here should be thought of as executing concurrently rather than
-- sequentially. Each assignment statement in the formula has a left-hand side and a
-- right-hand side. Everything occurring on the right-hand side should be interpreted
-- as the "pre-state" values -- so, for instance, if one statement assigns the pc, and
-- another statement reads the pc, then the latter should use the *original* value of
-- the PC rather than the new one, regardless of the orders of the statements.
data Formula arch (fmt :: Format)
  = Formula { _fComments :: !(Seq String)
              -- ^ multiline comment
            , _fDefs    :: !(Seq (Stmt arch fmt))
              -- ^ sequence of statements defining the formula
            }

-- | Lens for 'Formula' comments.
fComments :: Simple Lens (Formula arch fmt) (Seq String)
fComments = lens _fComments (\(Formula _ d) c -> Formula c d)

-- | Lens for 'Formula' statements.
fDefs :: Simple Lens (Formula arch fmt) (Seq (Stmt arch fmt))
fDefs = lens _fDefs (\(Formula c _) d -> Formula c d)

-- | Every definition begins with the empty formula.
emptyFormula :: Formula arch fmt
emptyFormula = Formula Seq.empty Seq.empty

-- | State monad for defining instruction semantics. When defining an instruction,
-- you shouldn't need to ever read the state directly, so we only export the type.
newtype FormulaBuilder arch (fmt :: Format) a =
  FormulaBuilder { unFormulaBuilder :: State (Formula arch fmt) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadState (Formula arch fmt))

----------------------------------------
-- FormulaBuilder functions for constructing expressions and statements. Some of
-- these are pure for the moment, but eventually we will want to have a more granular
-- representation of every single operation, so we keep them in a monadic style in
-- order to enable us to add some side effects later if we want to.

----------------------------------------
-- Smart constructors for BVApp functions

instance BVExpr (Expr arch fmt) where
  appExpr = AppExpr

-- | Get the operands for a particular known format
operandEs :: forall arch fmt . (KnownRepr FormatRepr fmt)
          => FormulaBuilder arch fmt (List (Expr arch fmt) (OperandTypes fmt))
operandEs = case knownRepr :: FormatRepr fmt of
  RRepr -> return (OperandExpr index0 :< OperandExpr index1 :< OperandExpr index2 :< Nil)
  IRepr -> return (OperandExpr index0 :< OperandExpr index1 :< OperandExpr index2 :< Nil)
  SRepr -> return (OperandExpr index0 :< OperandExpr index1 :< OperandExpr index2 :< Nil)
  BRepr -> return (OperandExpr index0 :< OperandExpr index1 :< OperandExpr index2 :< Nil)
  URepr -> return (OperandExpr index0 :< OperandExpr index1 :< Nil)
  JRepr -> return (OperandExpr index0 :< OperandExpr index1 :< Nil)
  XRepr -> return (OperandExpr index0 :< Nil)

-- | Obtain the formula defined by a 'FormulaBuilder' action.
getFormula :: FormulaBuilder arch fmt () -> Formula arch fmt
getFormula = flip execState emptyFormula . unFormulaBuilder

-- | Add a comment.
comment :: String -> FormulaBuilder arch fmt ()
comment c = fComments %= \cs -> cs Seq.|> c

-- | Get the width of the instruction word
instBytes :: FormulaBuilder arch fmt (Expr arch fmt (ArchWidth arch))
instBytes = return InstBytes

-- | Read the pc.
readPC :: FormulaBuilder arch fmt (Expr arch fmt (ArchWidth arch))
readPC = return (LocExpr PCExpr)

-- | Read a value from a register. Register x0 is hardwired to 0.
readReg :: KnownArch arch
        => Expr arch fmt 5
        -> FormulaBuilder arch fmt (Expr arch fmt (ArchWidth arch))
readReg ridE = return $ iteE (ridE `eqE` litBV 0) (litBV 0) (LocExpr (RegExpr ridE))

-- | Read a byte from memory.
readMem :: Expr arch fmt (ArchWidth arch) -> FormulaBuilder arch fmt (Expr arch fmt 8)
readMem addr = return (LocExpr (MemExpr addr))

-- | Read a value from a CSR.
readCSR :: KnownArch arch
        => Expr arch fmt 12
        -> FormulaBuilder arch fmt (Expr arch fmt (ArchWidth arch))
readCSR csr = return (LocExpr (CSRExpr csr))

-- | Read the current privilege level.
readPriv :: FormulaBuilder arch fmt (Expr arch fmt 2)
readPriv = return (LocExpr PrivExpr)

-- | Add a statement to the formula.
addStmt :: Stmt arch fmt -> FormulaBuilder arch fmt ()
addStmt stmt = fDefs %= \stmts -> stmts Seq.|> stmt

-- | Add a PC assignment to the formula.
assignPC :: Expr arch fmt (ArchWidth arch) -> FormulaBuilder arch fmt ()
assignPC pc = addStmt (AssignStmt PCExpr pc)

-- | Add a register assignment to the formula.
assignReg :: Expr arch fmt 5
          -> Expr arch fmt (ArchWidth arch)
          -> FormulaBuilder arch fmt ()
assignReg r e = addStmt (AssignStmt (RegExpr r) e)

-- | Add a memory location assignment to the formula.
assignMem :: Expr arch fmt (ArchWidth arch)
          -> Expr arch fmt 8
          -> FormulaBuilder arch fmt ()
assignMem addr val = addStmt (AssignStmt (MemExpr addr) val)

-- | Add a CSR assignment to the formula.
assignCSR :: Expr arch fmt 12
          -> Expr arch fmt (ArchWidth arch)
          -> FormulaBuilder arch fmt ()
assignCSR csr val = addStmt (AssignStmt (CSRExpr csr) val)

-- | Add a privilege assignment to the formula.
assignPriv :: Expr arch fmt 2 -> FormulaBuilder arch fmt ()
assignPriv priv = addStmt (AssignStmt PrivExpr priv)

-- | Left-associative application (use with 'branch' to avoid parentheses around @do@
-- notation)
($>) :: (a -> b) -> a -> b
($>) = ($)

infixl 0 $>

-- | Add a branch statement to the formula. Note that comments in the subformulas
-- will be ignored.
branch :: Expr arch fmt 1
       -> FormulaBuilder arch fmt ()
       -> FormulaBuilder arch fmt ()
       -> FormulaBuilder arch fmt ()
branch e fbTrue fbFalse = do
  let fTrue  = getFormula fbTrue  ^. fDefs
      fFalse = getFormula fbFalse ^. fDefs
  addStmt (BranchStmt e fTrue fFalse)
