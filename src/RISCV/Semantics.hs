{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
    BVExpr(..)
  , RVExpr(..)
  , Exception(..)
  , Stmt(..)
  , Formula, fComments, fDefs
  , FormatParams
  -- * FormulaBuilder monad
  , FormulaBuilder
  , getFormula
  -- * FormulaBuilder operations
  -- ** Auxiliary
  , comment
  , params
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
import Data.Foldable (toList)
import Data.Parameterized
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import GHC.TypeLits

import RISCV.Types

----------------------------------------
-- Expressions, statements, and formulas

-- | BitVector expressions. These are the building blocks for semantic formulas for
-- an instruction.
data BVExpr (f :: Nat -> *) (w :: Nat) where
  -- Literal BitVector
  LitBV :: BitVector w -> BVExpr f w

  -- Bitwise operations
  AndE :: !(f w) -> !(f w) -> BVExpr f w
  OrE  :: !(f w) -> !(f w) -> BVExpr f w
  XorE :: !(f w) -> !(f w) -> BVExpr f w
  NotE :: !(f w) -> BVExpr f w

  -- Arithmetic operations
  AddE :: !(f w) -> !(f w) -> BVExpr f w
  SubE :: !(f w) -> !(f w) -> BVExpr f w
  MulSE :: !(f w) -> !(f w) -> BVExpr f (w+w)
  MulUE :: !(f w) -> !(f w) -> BVExpr f (w+w)
  MulSUE :: !(f w) -> !(f w) -> BVExpr f (w+w)
  DivUE :: !(f w) -> !(f w) -> BVExpr f w
  DivSE :: !(f w) -> !(f w) -> BVExpr f w
  RemUE :: !(f w) -> !(f w) -> BVExpr f w
  RemSE :: !(f w) -> !(f w) -> BVExpr f w

  -- TODO: Should we allow the shifter operand to have any width? This would simplify
  -- the semantics, but might make it more complicated to interpret.
  -- Shifts
  SllE :: !(f w) -> !(f w) -> BVExpr f w
  SrlE :: !(f w) -> !(f w) -> BVExpr f w
  SraE :: !(f w) -> !(f w) -> BVExpr f w

  -- Comparisons
  EqE  :: !(f w) -> !(f w) -> BVExpr f 1
  LtuE :: !(f w) -> !(f w) -> BVExpr f 1
  LtsE :: !(f w) -> !(f w) -> BVExpr f 1

  -- Width-changing
  ZExtE :: NatRepr w' -> !(f w) -> BVExpr f w'
  SExtE :: NatRepr w' -> !(f w) -> BVExpr f w'
  ExtractE :: NatRepr w' -> Int -> !(f w) -> BVExpr f w'
  ConcatE :: !(f w) -> !(f w') -> BVExpr f (w+w')

  -- Other operations
  IteE :: !(f 1)
       -> !(f w)
       -> !(f w)
       -> BVExpr f w

data RVExpr (arch :: BaseArch) (fmt :: Format) (w :: Nat) where
  -- Accessing the instruction
  ParamBV :: OperandID fmt otp -> RVExpr arch fmt (OperandWidth otp)
  InstBytes :: RVExpr arch fmt (ArchWidth arch)

  -- Accessing state
  PCRead  :: RVExpr arch fmt (ArchWidth arch)
  RegRead :: RVExpr arch fmt 5 -> RVExpr arch fmt (ArchWidth arch)
  MemRead :: RVExpr arch fmt (ArchWidth arch)
          -> RVExpr arch fmt 8

  -- BVExpr with RVExpr subexpressions
  BVExprVal :: BVExpr (RVExpr arch fmt) w -> RVExpr arch fmt w

-- deriving instance Show (RVExpr arch fmt w)

-- | Runtime exception.
data Exception = EnvironmentCall
               | Breakpoint
               | IllegalInstruction
               | MemoryAccessError
  deriving (Show)

-- | A 'Stmt' represents an atomic state transformation -- typically, an assignment
-- of a state component (register, memory location, etc.) to a 'RVExpr' of the
-- appropriate width.
data Stmt (arch :: BaseArch) (fmt :: Format) where
  AssignReg :: RVExpr arch fmt 5 -> RVExpr arch fmt (ArchWidth arch) -> Stmt arch fmt
  AssignMem :: RVExpr arch fmt (ArchWidth arch)
            -> RVExpr arch fmt 8
            -> Stmt arch fmt
  AssignPC  :: RVExpr arch fmt (ArchWidth arch) -> Stmt arch fmt
  RaiseException :: RVExpr arch fmt 1 -> Exception -> Stmt arch fmt

-- deriving instance Show (Stmt arch fmt)

-- | Formula representing the semantics of an instruction. A formula has a number of
-- parameters (potentially zero), which represent the input to the formula. These are
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
  = Formula { _fComments :: Seq String
              -- ^ multiline comment
            , _fDefs    :: Seq (Stmt arch fmt)
              -- ^ sequence of statements defining the formula
            }

-- | Lens for 'Formula' comments.
fComments :: Simple Lens (Formula arch fmt) (Seq String)
fComments = lens _fComments (\(Formula _ d) c -> Formula c d)

-- | Lens for 'Formula' statements.
fDefs :: Simple Lens (Formula arch fmt) (Seq (Stmt arch fmt))
fDefs = lens _fDefs (\(Formula c _) d -> Formula c d)

-- instance Show (Formula arch fmt) where
--   show (Formula comments defs) =
--     showComments ++
--     showDefs
--     where showComments = concat (toList ((++ "\n") <$> comments))
--           showDefs = concat (toList ((\d -> "  " ++ show d ++ "\n") <$> defs))
-- instance ShowF (Formula arch)

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
-- Smart constructors for BVExpr functions

-- | Literal bit vector.
litBV :: BitVector w -> RVExpr arch fmt w
litBV = BVExprVal . LitBV

-- | Bitwise and.
andE :: RVExpr arch fmt w
     -> RVExpr arch fmt w
     -> FormulaBuilder arch fmt (RVExpr arch fmt w)
andE e1 e2 = return $ BVExprVal (AndE e1 e2)

-- | Bitwise or.
orE :: RVExpr arch fmt w
    -> RVExpr arch fmt w
    -> FormulaBuilder arch fmt (RVExpr arch fmt w)
orE e1 e2 = return $ BVExprVal (OrE e1 e2)

-- | Bitwise xor.
xorE :: RVExpr arch fmt w
     -> RVExpr arch fmt w
     -> FormulaBuilder arch fmt (RVExpr arch fmt w)
xorE e1 e2 = return $ BVExprVal (XorE e1 e2)

-- | Bitwise not.
notE :: RVExpr arch fmt w -> FormulaBuilder arch fmt (RVExpr arch fmt w)
notE e = return $ BVExprVal (NotE e)

-- | Add two expressions.
addE :: RVExpr arch fmt w
     -> RVExpr arch fmt w
     -> FormulaBuilder arch fmt (RVExpr arch fmt w)
addE e1 e2 = return $ BVExprVal (AddE e1 e2)

-- | Subtract the second expression from the first.
subE :: RVExpr arch fmt w
     -> RVExpr arch fmt w
     -> FormulaBuilder arch fmt (RVExpr arch fmt w)
subE e1 e2 = return $ BVExprVal (SubE e1 e2)

-- | Signed multiply two 'BitVectors', doubling the width of the result to hold all
-- arithmetic overflow bits.
mulsE :: RVExpr arch fmt w
      -> RVExpr arch fmt w
      -> FormulaBuilder arch fmt (RVExpr arch fmt (w+w))
mulsE e1 e2 = return $ BVExprVal (MulSE e1 e2)

-- | Unsigned multiply two 'BitVectors', doubling the width of the result to hold
-- all arithmetic overflow bits.
muluE :: RVExpr arch fmt w
      -> RVExpr arch fmt w
      -> FormulaBuilder arch fmt (RVExpr arch fmt (w+w))
muluE e1 e2 = return $ BVExprVal (MulUE e1 e2)

-- | Multiply two 'BitVectors', treating the first as a signed number and the second
-- as an unsigned number, doubling the width of the result to hold all arithmetic
-- overflow bits.
mulsuE :: RVExpr arch fmt w
       -> RVExpr arch fmt w
       -> FormulaBuilder arch fmt (RVExpr arch fmt (w+w))
mulsuE e1 e2 = return $ BVExprVal (MulSUE e1 e2)

-- | Signed divide two 'BitVectors', rounding to zero.
divsE :: RVExpr arch fmt w
      -> RVExpr arch fmt w
      -> FormulaBuilder arch fmt (RVExpr arch fmt w)
divsE e1 e2 = return $ BVExprVal (DivSE e1 e2)

-- | Unsigned divide two 'BitVectors', rounding to zero.
divuE :: RVExpr arch fmt w
     -> RVExpr arch fmt w
     -> FormulaBuilder arch fmt (RVExpr arch fmt w)
divuE e1 e2 = return $ BVExprVal (DivUE e1 e2)

-- | Remainder after signed division of two 'BitVectors', when rounded to zero.
remsE :: RVExpr arch fmt w
     -> RVExpr arch fmt w
     -> FormulaBuilder arch fmt (RVExpr arch fmt w)
remsE e1 e2 = return $ BVExprVal (RemSE e1 e2)

-- | Remainder after unsigned division of two 'BitVectors', when rounded to zero.
remuE :: RVExpr arch fmt w
     -> RVExpr arch fmt w
     -> FormulaBuilder arch fmt (RVExpr arch fmt w)
remuE e1 e2 = return $ BVExprVal (RemUE e1 e2)

-- | Left logical shift the first expression by the second.
sllE :: RVExpr arch fmt w
     -> RVExpr arch fmt w
     -> FormulaBuilder arch fmt (RVExpr arch fmt w)
sllE e1 e2 = return $ BVExprVal (SllE e1 e2)

-- | Left logical shift the first expression by the second.
srlE :: RVExpr arch fmt w
     -> RVExpr arch fmt w
     -> FormulaBuilder arch fmt (RVExpr arch fmt w)
srlE e1 e2 = return $ BVExprVal (SrlE e1 e2)

-- | Left logical shift the first expression by the second.
sraE :: RVExpr arch fmt w
     -> RVExpr arch fmt w
     -> FormulaBuilder arch fmt (RVExpr arch fmt w)
sraE e1 e2 = return $ BVExprVal (SraE e1 e2)

-- | Test for equality of two expressions.
eqE :: RVExpr arch fmt w
    -> RVExpr arch fmt w
    -> FormulaBuilder arch fmt (RVExpr arch fmt 1)
eqE e1 e2 = return $ BVExprVal (EqE e1 e2)

-- | Signed less than
ltsE :: RVExpr arch fmt w
     -> RVExpr arch fmt w
     -> FormulaBuilder arch fmt (RVExpr arch fmt 1)
ltsE e1 e2 = return $ BVExprVal (LtsE e1 e2)

-- | Unsigned less than
ltuE :: RVExpr arch fmt w
     -> RVExpr arch fmt w
     -> FormulaBuilder arch fmt (RVExpr arch fmt 1)
ltuE e1 e2 = return $ BVExprVal (LtuE e1 e2)

-- | Zero-extension
-- zextE :: KnownNat w' => RVExpr arch fmt w -> FormulaBuilder arch fmt (RVExpr arch fmt w')
zextE :: KnownNat w' => RVExpr arch fmt w -> FormulaBuilder arch fmt (RVExpr arch fmt w')
zextE e = return $ BVExprVal (ZExtE knownNat e)

-- | Sign-extension
sextE :: KnownNat w' => RVExpr arch fmt w -> FormulaBuilder arch fmt (RVExpr arch fmt w')
sextE e = return $ BVExprVal (SExtE knownNat e)

-- | Extract bits
extractE :: KnownNat w' => Int -> RVExpr arch fmt w -> FormulaBuilder arch fmt (RVExpr arch fmt w')
extractE base e = return $ BVExprVal (ExtractE knownNat base e)

-- | Extract bits with an explicit width argument
extractEWithRepr :: NatRepr w'
                 -> Int
                 -> RVExpr arch fmt w
                 -> FormulaBuilder arch fmt (RVExpr arch fmt w')
extractEWithRepr wRepr base e = return $ BVExprVal (ExtractE wRepr base e)

-- | Concatenation
concatE :: RVExpr arch fmt w -> RVExpr arch fmt w' -> FormulaBuilder arch fmt (RVExpr arch fmt (w+w'))
concatE e1 e2 = return $ BVExprVal (ConcatE e1 e2)

-- | Conditional branch.
iteE :: RVExpr arch fmt 1
     -> RVExpr arch fmt w
     -> RVExpr arch fmt w
     -> FormulaBuilder arch fmt (RVExpr arch fmt w)
iteE t e1 e2 = return $ BVExprVal (IteE t e1 e2)

-- | Obtain the formula defined by a 'FormulaBuilder' action.
getFormula :: FormulaBuilder arch fmt () -> Formula arch fmt
getFormula = flip execState emptyFormula . unFormulaBuilder

-- | Add a comment.
comment :: String -> FormulaBuilder arch fmt ()
comment c = fComments %= \cs -> cs Seq.|> c

-- | Get the width of the instruction word
instBytes :: FormulaBuilder arch fmt (RVExpr arch fmt (ArchWidth arch))
instBytes = return InstBytes

-- | Read the pc.
pcRead :: FormulaBuilder arch fmt (RVExpr arch fmt (ArchWidth arch))
pcRead = return PCRead

-- | Read a register.
regRead :: RVExpr arch fmt 5 -> FormulaBuilder arch fmt (RVExpr arch fmt (ArchWidth arch))
regRead = return . RegRead

-- | Read a byte from memory.
memRead :: RVExpr arch fmt (ArchWidth arch)
        -> FormulaBuilder arch fmt (RVExpr arch fmt 8)
memRead addr = return (MemRead addr)

-- | Add a statement to the formula.
addStmt :: Stmt arch fmt -> FormulaBuilder arch fmt ()
addStmt stmt = fDefs %= \stmts -> stmts Seq.|> stmt

-- TODO: protect against multiple assignments? (for all of the assign* functions)
-- | Add a register assignment to the formula.
assignReg :: RVExpr arch fmt 5
          -> RVExpr arch fmt (ArchWidth arch)
          -> FormulaBuilder arch fmt ()
assignReg r e = addStmt (AssignReg r e)

-- TODO: Should we allow arbitrary width assignments?
-- | Add a memory location assignment to the formula.
assignMem :: RVExpr arch fmt (ArchWidth arch)
          -> RVExpr arch fmt 8
          -> FormulaBuilder arch fmt ()
assignMem addr val = addStmt (AssignMem addr val)

-- | Add a PC assignment to the formula.
assignPC :: RVExpr arch fmt (ArchWidth arch) -> FormulaBuilder arch fmt ()
assignPC pc = addStmt (AssignPC pc)

-- | Conditionally raise an exception.
raiseException :: RVExpr arch fmt 1 -> Exception -> FormulaBuilder arch fmt ()
raiseException cond e = addStmt (RaiseException cond e)

-- | Maps each format to the parameter types for its operands.
-- We include an extra parameter indicating the size of the instruction word for pc
-- incrementing.
type family FormatParams (arch :: BaseArch) (fmt :: Format) :: * where
  FormatParams arch R = (RVExpr arch R 5, RVExpr arch R 5, RVExpr arch R 5)
  FormatParams arch I = (RVExpr arch I 5, RVExpr arch I 5, RVExpr arch I 12)
  FormatParams arch S = (RVExpr arch S 5, RVExpr arch S 5, RVExpr arch S 12)
  FormatParams arch B = (RVExpr arch B 5, RVExpr arch B 5, RVExpr arch B 12)
  FormatParams arch U = (RVExpr arch U 5, RVExpr arch U 20)
  FormatParams arch J = (RVExpr arch J 5, RVExpr arch J 20)
  FormatParams arch X = (RVExpr arch X 32)

params' :: FormatRepr fmt
        -> FormulaBuilder arch fmt (FormatParams arch fmt)
params' repr = case repr of
    RRepr -> return (ParamBV RRd, ParamBV RRs1, ParamBV RRs2)
    IRepr -> return (ParamBV IRd, ParamBV IRs1, ParamBV IImm12)
    SRepr -> return (ParamBV SRs1, ParamBV SRs2, ParamBV SImm12)
    BRepr -> return (ParamBV BRs1, ParamBV BRs2, ParamBV BImm12)
    URepr -> return (ParamBV URd, ParamBV UImm20)
    JRepr -> return (ParamBV JRd, ParamBV JImm20)
    XRepr -> return (ParamBV XImm32)

-- | Get the parameters for a particular known format
params :: (KnownRepr FormatRepr fmt) => FormulaBuilder arch fmt (FormatParams arch fmt)
params = params' knownRepr
