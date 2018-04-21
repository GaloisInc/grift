{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

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
  ( -- * Types
    Expr(..)
  , Exception(..)
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
  , pcRead
  , regRead
  , memRead
  -- ** Bitwise
  , andE
  , orE
  , xorE
  , notE
  -- ** Arithmetic
  , addE
  , subE
  , muluE
  , mulsE
  , mulsuE
  , divuE
  , divsE
  , remuE
  , remsE
  , sllE
  , srlE
  , sraE
  -- ** Comparison
  , eqE
  , ltuE
  , ltsE
  -- ** Width-changing
  , zextE
  , sextE
  , extractE
  , extractEWithRepr
  , concatE
  -- ** Control
  , iteE
  -- ** State actions
  , assignReg
  , assignMem
  , assignPC
  , raiseException
  ) where

import Control.Lens ( (%=), Simple, Lens, lens )
import Control.Monad.State
import Data.BitVector.Sized
import Data.Parameterized
import Data.Parameterized.List
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import GHC.TypeLits

import Data.BitVector.Sized.App
import RISCV.Types

----------------------------------------
-- Expressions, statements, and formulas

-- | Expressions for computations over the RISC-V machine state.
data Expr (arch :: BaseArch) (fmt :: Format) (w :: Nat) where
  -- Accessing the instruction
  OperandExpr :: !(OperandID fmt w) -> Expr arch fmt w
  InstBytes :: Expr arch fmt (ArchWidth arch)

  -- Accessing state
  PCRead  :: Expr arch fmt (ArchWidth arch)
  RegRead :: !(Expr arch fmt 5) -> Expr arch fmt (ArchWidth arch)
  MemRead :: !(Expr arch fmt (ArchWidth arch)) -> Expr arch fmt 8

  -- BVApp with Expr subexpressions
  AppExpr :: !(BVApp (Expr arch fmt) w) -> Expr arch fmt w

-- | Runtime exception.
data Exception = EnvironmentCall
               | Breakpoint
               | IllegalInstruction
               | MemoryAccessError
  deriving (Show)

-- | A 'Stmt' represents an atomic state transformation -- typically, an assignment
-- of a state component (register, memory location, etc.) to a 'Expr' of the
-- appropriate width.
data Stmt (arch :: BaseArch) (fmt :: Format) where
  AssignReg :: !(Expr arch fmt 5) -> !(Expr arch fmt (ArchWidth arch)) -> Stmt arch fmt
  AssignMem :: !(Expr arch fmt (ArchWidth arch)) -> !(Expr arch fmt 8) -> Stmt arch fmt
  AssignPC  :: !(Expr arch fmt (ArchWidth arch)) -> Stmt arch fmt
  RaiseException :: !(Expr arch fmt 1) -> !Exception -> Stmt arch fmt

-- | Formula representing the semantics of an instruction. A formula has a number of
-- operands (potentially zero), which represent the input to the formula. These are
-- going to the be the operands of the instruction -- register ids, immediate values,
-- and so forth.
--
-- Each definition here should be thought of as executing concurrently rather than
-- sequentially. Each assignment statement in the formula has a left-hand side and a
-- right-hand side. Everything occurring on the right-hand side should be interpreted
-- as the "pre-state" values -- so, for instance, if one statement sets the pc, and
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

-- | Literal bit vector.
litBV :: BitVector w -> Expr arch fmt w
litBV = AppExpr . LitBVApp

-- | Bitwise and.
andE :: Expr arch fmt w
     -> Expr arch fmt w
     -> FormulaBuilder arch fmt (Expr arch fmt w)
andE e1 e2 = return $ AppExpr (AndApp e1 e2)

-- | Bitwise or.
orE :: Expr arch fmt w
    -> Expr arch fmt w
    -> FormulaBuilder arch fmt (Expr arch fmt w)
orE e1 e2 = return $ AppExpr (OrApp e1 e2)

-- | Bitwise xor.
xorE :: Expr arch fmt w
     -> Expr arch fmt w
     -> FormulaBuilder arch fmt (Expr arch fmt w)
xorE e1 e2 = return $ AppExpr (XorApp e1 e2)

-- | Bitwise not.
notE :: Expr arch fmt w -> FormulaBuilder arch fmt (Expr arch fmt w)
notE e = return $ AppExpr (NotApp e)

-- | Add two expressions.
addE :: Expr arch fmt w
     -> Expr arch fmt w
     -> FormulaBuilder arch fmt (Expr arch fmt w)
addE e1 e2 = return $ AppExpr (AddApp e1 e2)

-- | Subtract the second expression from the first.
subE :: Expr arch fmt w
     -> Expr arch fmt w
     -> FormulaBuilder arch fmt (Expr arch fmt w)
subE e1 e2 = return $ AppExpr (SubApp e1 e2)

-- | Signed multiply two 'BitVectors', doubling the width of the result to hold all
-- arithmetic overflow bits.
mulsE :: Expr arch fmt w
      -> Expr arch fmt w
      -> FormulaBuilder arch fmt (Expr arch fmt (w+w))
mulsE e1 e2 = return $ AppExpr (MulSApp e1 e2)

-- | Unsigned multiply two 'BitVectors', doubling the width of the result to hold
-- all arithmetic overflow bits.
muluE :: Expr arch fmt w
      -> Expr arch fmt w
      -> FormulaBuilder arch fmt (Expr arch fmt (w+w))
muluE e1 e2 = return $ AppExpr (MulUApp e1 e2)

-- | Multiply two 'BitVectors', treating the first as a signed number and the second
-- as an unsigned number, doubling the width of the result to hold all arithmetic
-- overflow bits.
mulsuE :: Expr arch fmt w
       -> Expr arch fmt w
       -> FormulaBuilder arch fmt (Expr arch fmt (w+w))
mulsuE e1 e2 = return $ AppExpr (MulSUApp e1 e2)

-- | Signed divide two 'BitVectors', rounding to zero.
divsE :: Expr arch fmt w
      -> Expr arch fmt w
      -> FormulaBuilder arch fmt (Expr arch fmt w)
divsE e1 e2 = return $ AppExpr (DivSApp e1 e2)

-- | Unsigned divide two 'BitVectors', rounding to zero.
divuE :: Expr arch fmt w
     -> Expr arch fmt w
     -> FormulaBuilder arch fmt (Expr arch fmt w)
divuE e1 e2 = return $ AppExpr (DivUApp e1 e2)

-- | Remainder after signed division of two 'BitVectors', when rounded to zero.
remsE :: Expr arch fmt w
     -> Expr arch fmt w
     -> FormulaBuilder arch fmt (Expr arch fmt w)
remsE e1 e2 = return $ AppExpr (RemSApp e1 e2)

-- | Remainder after unsigned division of two 'BitVectors', when rounded to zero.
remuE :: Expr arch fmt w
     -> Expr arch fmt w
     -> FormulaBuilder arch fmt (Expr arch fmt w)
remuE e1 e2 = return $ AppExpr (RemUApp e1 e2)

-- | Left logical shift the first expression by the second.
sllE :: Expr arch fmt w
     -> Expr arch fmt w
     -> FormulaBuilder arch fmt (Expr arch fmt w)
sllE e1 e2 = return $ AppExpr (SllApp e1 e2)

-- | Left logical shift the first expression by the second.
srlE :: Expr arch fmt w
     -> Expr arch fmt w
     -> FormulaBuilder arch fmt (Expr arch fmt w)
srlE e1 e2 = return $ AppExpr (SrlApp e1 e2)

-- | Left logical shift the first expression by the second.
sraE :: Expr arch fmt w
     -> Expr arch fmt w
     -> FormulaBuilder arch fmt (Expr arch fmt w)
sraE e1 e2 = return $ AppExpr (SraApp e1 e2)

-- | Test for equality of two expressions.
eqE :: Expr arch fmt w
    -> Expr arch fmt w
    -> FormulaBuilder arch fmt (Expr arch fmt 1)
eqE e1 e2 = return $ AppExpr (EqApp e1 e2)

-- | Signed less than
ltsE :: Expr arch fmt w
     -> Expr arch fmt w
     -> FormulaBuilder arch fmt (Expr arch fmt 1)
ltsE e1 e2 = return $ AppExpr (LtsApp e1 e2)

-- | Unsigned less than
ltuE :: Expr arch fmt w
     -> Expr arch fmt w
     -> FormulaBuilder arch fmt (Expr arch fmt 1)
ltuE e1 e2 = return $ AppExpr (LtuApp e1 e2)

-- | Zero-extension
-- zextE :: KnownNat w' => Expr arch fmt w -> FormulaBuilder arch fmt (Expr arch fmt w')
zextE :: KnownNat w' => Expr arch fmt w -> FormulaBuilder arch fmt (Expr arch fmt w')
zextE e = return $ AppExpr (ZExtApp knownNat e)

-- | Sign-extension
sextE :: KnownNat w' => Expr arch fmt w -> FormulaBuilder arch fmt (Expr arch fmt w')
sextE e = return $ AppExpr (SExtApp knownNat e)

-- | Extract bits
extractE :: KnownNat w' => Int -> Expr arch fmt w -> FormulaBuilder arch fmt (Expr arch fmt w')
extractE base e = return $ AppExpr (ExtractApp knownNat base e)

-- | Extract bits with an explicit width argument
extractEWithRepr :: NatRepr w'
                 -> Int
                 -> Expr arch fmt w
                 -> FormulaBuilder arch fmt (Expr arch fmt w')
extractEWithRepr wRepr base e = return $ AppExpr (ExtractApp wRepr base e)

-- | Concatenation
concatE :: Expr arch fmt w -> Expr arch fmt w' -> FormulaBuilder arch fmt (Expr arch fmt (w+w'))
concatE e1 e2 = return $ AppExpr (ConcatApp e1 e2)

-- | Conditional branch.
iteE :: Expr arch fmt 1
     -> Expr arch fmt w
     -> Expr arch fmt w
     -> FormulaBuilder arch fmt (Expr arch fmt w)
iteE t e1 e2 = return $ AppExpr (IteApp t e1 e2)

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
pcRead :: FormulaBuilder arch fmt (Expr arch fmt (ArchWidth arch))
pcRead = return PCRead

regRead' :: Expr arch fmt 5 -> FormulaBuilder arch fmt (Expr arch fmt (ArchWidth arch))
regRead' = return . RegRead

-- | Read a value from a register. Register x0 is hardwired to 0.
regRead :: KnownArch arch
        => Expr arch fmt 5
        -> FormulaBuilder arch fmt (Expr arch fmt (ArchWidth arch))
regRead ridE = do
  isR0 <- ridE `eqE` litBV 0
  rVal <- regRead' ridE
  iteE isR0 (litBV 0) rVal

-- | Read a byte from memory.
memRead :: Expr arch fmt (ArchWidth arch)
        -> FormulaBuilder arch fmt (Expr arch fmt 8)
memRead addr = return (MemRead addr)

-- | Add a statement to the formula.
addStmt :: Stmt arch fmt -> FormulaBuilder arch fmt ()
addStmt stmt = fDefs %= \stmts -> stmts Seq.|> stmt

-- TODO: protect against multiple assignments? (for all of the assign* functions)
-- | Add a register assignment to the formula.
assignReg :: Expr arch fmt 5
          -> Expr arch fmt (ArchWidth arch)
          -> FormulaBuilder arch fmt ()
assignReg r e = addStmt (AssignReg r e)

-- TODO: Should we allow arbitrary width assignments?
-- | Add a memory location assignment to the formula.
assignMem :: Expr arch fmt (ArchWidth arch)
          -> Expr arch fmt 8
          -> FormulaBuilder arch fmt ()
assignMem addr val = addStmt (AssignMem addr val)

-- | Add a PC assignment to the formula.
assignPC :: Expr arch fmt (ArchWidth arch) -> FormulaBuilder arch fmt ()
assignPC pc = addStmt (AssignPC pc)

-- | Conditionally raise an exception.
raiseException :: Expr arch fmt 1 -> Exception -> FormulaBuilder arch fmt ()
raiseException cond e = addStmt (RaiseException cond e)
