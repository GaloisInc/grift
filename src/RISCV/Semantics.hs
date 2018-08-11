{-
This file is part of GRIFT (Galois RISC-V ISA Formal Tools).

GRIFT is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GRIFT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero Public License for more details.

You should have received a copy of the GNU Affero Public License
along with GRIFT.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}

{-|
Module      : RISCV.Semantics
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module defines two languages for expressing computations over the RISC-V
machine state. These languages are respresented by two types. The first,
'PureStateExpr', encapsulates any compound expression that can be constructed
using components of the machine state. The second, 'InstExpr', subsumes the first
in that it can express everything that 'PureStateExpr' can express. In addition,
'InstExpr' assumes that the expression being defined will be evaluated in the
context of an executing instruction, and so we can also access information about
that instruction, such as its width and its operands.

These two expression types are formed from the "open" expression type, 'StateExpr',
which encapsulates the notion of compound expressions involving pieces of RISC-V
machine state. The 'StateExpr' type has a type parameter @expr@, which allows us to
build compound expressions using sub-expressions of type @expr@. 'PureStateExpr' and
'InstExpr' both contain 'StateExpr' as a special case, where the @expr@ parameter is
instantiated as 'PureStateExpr' and 'InstExpr', respectively, thus "tying the knot"
and allowing us to construct arbitrary bitvector expressions involving the RISC-V
machine state and, in the case of 'InstExpr', we can also refer to the instruction
currently being executed.

We also export a typeclass 'RVStateExpr', implemented by both 'PureStateExpr' and
'InstExpr'. This contains a single method, 'stateExpr', which allows us to embed a
'StateExpr' into these types. This class is useful for defining semantic formulas
that could be executed either inside or outside the context of an executing
instruction.

Using these expressions, we can also construct assignments of pieces of state to
expressions. This is encapsulated in the 'Stmt' type, whose constructor 'AssignStmt'
does exactly this. We can also create 'BranchStmt's which conditionally execute one
of two distinct sets of statements based on a test condition.

Finally, we can combine 'Stmt's to create 'Semantics's, which are simply sequences of
statements. We export a monad, 'SemanticsBuilder,' to facilitate straightforward
definitions of these assignments, and a number of functions ('assignPC', 'assignReg',
etc.) that can be used within this monad. To see examples of its use, take a look at
RISCV.Extensions.Base, which contains the base RISC-V ISA instruction definitions,
defined using 'SemanticsBuilder'.

-}

module RISCV.Semantics
  ( -- * RISC-V semantic expressions
    LocExpr(..)
  , StateExpr(..)
  , PureStateExpr(..)
  , InstExpr(..)
  , RVStateExpr(..)
    -- ** Access to state
  , readPC
  , readReg
  , readFReg
  , readMem
  , readCSR
  , readPriv
  , checkReserved
    -- * RISC-V semantic statements and formulas
  , Stmt(..)
  , Semantics, semComments, semStmts
  , InstSemantics(..)
    -- * SemanticsBuilder monad
  , SemanticsBuilder
  , getSemantics
    -- * SemanticsBuilder operations
    -- ** Auxiliary
  , comment
  , operandEs, operandEsWithRepr
  , instBytes
  , instWord
  , litBV
    -- ** State actions
  , assignPC
  , assignReg
  , assignFReg
  , assignMem
  , assignCSR
  , assignPriv
  , reserve
  , branch
  , ($>)
  ) where

import Control.Lens ( (%=), (^.), Simple, Lens, lens )
import Control.Monad.State
import Data.Foldable (toList)
import Data.Parameterized
import Data.Parameterized.List
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import GHC.TypeLits
import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJClass

import Data.BitVector.Sized.App
import RISCV.Types

----------------------------------------
-- Expressions, statements, and semantics

-- | This type represents an abstract component of the global state. Sub-expressions
-- come from an arbitrary expression language @expr@.
data LocExpr (expr :: Nat -> *) (rv :: RV) (w :: Nat) where
  PCExpr   :: LocExpr expr rv (RVWidth rv)
  RegExpr  :: expr 5 -> LocExpr expr rv (RVWidth rv)
  FRegExpr :: FExt << rv => expr 5 -> LocExpr expr rv (RVFloatWidth rv)
  MemExpr  :: NatRepr bytes -> expr (RVWidth rv) -> LocExpr expr rv (8*bytes)
  ResExpr  :: expr (RVWidth rv) -> LocExpr expr rv 1
  CSRExpr  :: expr 12 -> LocExpr expr rv (RVWidth rv)
  PrivExpr :: LocExpr expr rv 2

-- | Expressions for general computations over the RISC-V machine state -- we can
-- access specific locations, and we can also build up compound expressions using the
-- 'BVApp' expression language. Sub-expressions come from an arbitrary expression
-- language @expr@.
data StateExpr (expr :: Nat -> *) (rv :: RV) (w :: Nat) where
  -- Accessing state
  LocExpr :: !(LocExpr expr rv w) -> StateExpr expr rv w

  -- BVApp with Expr subexpressions
  AppExpr :: !(BVApp expr w) -> StateExpr expr rv w

-- | Expressions built purely from 'StateExpr's, which are executed outside the
-- context of an executing instruction (for instance, during exception handling).
newtype PureStateExpr (rv :: RV) (w :: Nat) = PureStateExpr (StateExpr (PureStateExpr rv) rv w)

instance BVExpr (PureStateExpr rv) where
  appExpr = PureStateExpr . AppExpr

-- | Expressions for computations over the RISC-V machine state, in the context of
-- an executing instruction.
data InstExpr (fmt :: Format) (rv :: RV) (w :: Nat) where
  -- | Accessing the instruction operands
  OperandExpr :: !(OperandID fmt w) -> InstExpr fmt rv w
  -- | Accessing the instruction width, in number of bytes
  InstBytes :: InstExpr fmt rv (RVWidth rv)
  -- | Accessing the entire instruction word itself
  InstWord :: InstExpr fmt rv (RVWidth rv)

  -- | Accessing the machine state
  InstStateExpr :: !(StateExpr (InstExpr fmt rv) rv w) -> InstExpr fmt rv w

instance BVExpr (InstExpr fmt rv) where
  appExpr = InstStateExpr . AppExpr

-- TODO: When we get quantified constraints, put a forall arch. BVExpr (expr arch)
-- here
-- | A type class for expression languages that can refer to arbitrary pieces of
-- RISC-V machine state.
class RVStateExpr (expr :: RV -> Nat -> *) where
  stateExpr :: StateExpr (expr rv) rv w -> expr rv w

instance RVStateExpr PureStateExpr where
  stateExpr = PureStateExpr

instance RVStateExpr (InstExpr fmt) where
  stateExpr = InstStateExpr

-- | A 'Stmt' represents an atomic state transformation -- typically, an assignment
-- of a state component (register, memory location, etc.) to an expression of the
-- appropriate width.
data Stmt (expr :: Nat -> *) (rv :: RV) where
  -- | Assign a piece of state to a value.
  AssignStmt :: !(LocExpr expr rv w) -> !(expr w) -> Stmt expr rv
  -- | If-then-else branch statement.
  BranchStmt :: !(expr 1)
             -> !(Seq (Stmt expr rv))
             -> !(Seq (Stmt expr rv))
             -> Stmt expr rv

-- | A 'Semantics' is simply a set of simultaneous 'Stmt's.
data Semantics (expr :: Nat -> *) (rv :: RV)
  = Semantics { _semComments :: !(Seq String)
              -- ^ multiline comment
            , _semStmts    :: !(Seq (Stmt expr rv))
              -- ^ sequence of statements defining the semantics
            }

-- | A wrapper for 'Semantics's over 'InstExpr's with the 'Format' type parameter
-- appearing last, for the purposes of associating with an corresponding 'Opcode' of
-- the same format.
newtype InstSemantics (rv :: RV) (fmt :: Format)
  = InstSemantics { getInstSemantics :: Semantics (InstExpr fmt rv) rv }

-- | Lens for 'Semantics' comments.
semComments :: Simple Lens (Semantics expr rv) (Seq String)
semComments = lens _semComments (\(Semantics _ d) c -> Semantics c d)

-- | Lens for 'Semantics' statements.
semStmts :: Simple Lens (Semantics expr rv) (Seq (Stmt expr rv))
semStmts = lens _semStmts (\(Semantics c _) d -> Semantics c d)

-- | Every definition begins with the empty semantics.
emptySemantics :: Semantics expr rv
emptySemantics = Semantics Seq.empty Seq.empty

-- | State monad for defining semantics over the RISC-V machine state. We allow the
-- expression language to vary, because sometimes we are in the context of an
-- executing instruction, and sometimes we are not (for instance, when we are
-- handling an exception).
newtype SemanticsBuilder (expr :: Nat -> *) (rv :: RV) a =
  SemanticsBuilder { unSemanticsBuilder :: State (Semantics expr rv) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadState (Semantics expr rv))

-- | Get the operands for a particular known format.
operandEs :: forall rv fmt . (KnownRepr FormatRepr fmt)
          => SemanticsBuilder (InstExpr fmt rv) rv (List (InstExpr fmt rv) (OperandTypes fmt))
operandEs = return (operandEsWithRepr knownRepr)

-- | Get the operands for a particular format, where the format is supplied as an
-- explicit argument.
operandEsWithRepr :: FormatRepr fmt -> (List (InstExpr fmt rv) (OperandTypes fmt))
operandEsWithRepr repr = case repr of
  RRepr -> (OperandExpr (OperandID index0) :<
            OperandExpr (OperandID index1) :<
            OperandExpr (OperandID index2) :< Nil)
  IRepr -> (OperandExpr (OperandID index0) :<
            OperandExpr (OperandID index1) :<
            OperandExpr (OperandID index2) :< Nil)
  SRepr -> (OperandExpr (OperandID index0) :<
            OperandExpr (OperandID index1) :<
            OperandExpr (OperandID index2) :< Nil)
  BRepr -> (OperandExpr (OperandID index0) :<
            OperandExpr (OperandID index1) :<
            OperandExpr (OperandID index2) :< Nil)
  URepr -> (OperandExpr (OperandID index0) :<
            OperandExpr (OperandID index1) :< Nil)
  JRepr -> (OperandExpr (OperandID index0) :<
            OperandExpr (OperandID index1) :< Nil)
  HRepr -> (OperandExpr (OperandID index0) :<
            OperandExpr (OperandID index1) :<
            OperandExpr (OperandID index2) :< Nil)
  PRepr -> Nil
  ARepr -> (OperandExpr (OperandID index0) :<
            OperandExpr (OperandID index1) :<
            OperandExpr (OperandID index2) :<
            OperandExpr (OperandID index3) :<
            OperandExpr (OperandID index4) :< Nil)
  R2Repr -> (OperandExpr (OperandID index0) :<
             OperandExpr (OperandID index1) :<
             OperandExpr (OperandID index2) :< Nil)
  R3Repr -> (OperandExpr (OperandID index0) :<
             OperandExpr (OperandID index1) :<
             OperandExpr (OperandID index2) :<
             OperandExpr (OperandID index3) :< Nil)
  R4Repr -> (OperandExpr (OperandID index0) :<
             OperandExpr (OperandID index1) :<
             OperandExpr (OperandID index2) :<
             OperandExpr (OperandID index3) :<
             OperandExpr (OperandID index4) :< Nil)
  RXRepr -> (OperandExpr (OperandID index0) :<
             OperandExpr (OperandID index1) :< Nil)
  XRepr -> (OperandExpr (OperandID index0) :< Nil)
  where index4 = IndexThere index3

-- | Obtain the semantics defined by a 'SemanticsBuilder' action.
getSemantics :: SemanticsBuilder expr rv () -> Semantics expr rv
getSemantics = flip execState emptySemantics . unSemanticsBuilder

-- | Add a comment.
comment :: String -> SemanticsBuilder expr rv ()
comment c = semComments %= \cs -> cs Seq.|> c

-- | Get the width of the instruction word
instBytes :: SemanticsBuilder (InstExpr fmt rv) rv (InstExpr fmt rv (RVWidth rv))
instBytes = return InstBytes

-- | Get the entire instruction word (useful for exceptions)
instWord :: SemanticsBuilder (InstExpr fmt rv) rv (InstExpr fmt rv (RVWidth rv))
instWord = return InstWord

-- | Read the pc.
readPC :: RVStateExpr expr => expr rv (RVWidth rv)
readPC = stateExpr (LocExpr PCExpr)

-- | Read a value from a register. Register x0 is hardwired to 0.
readReg :: (BVExpr (expr rv), RVStateExpr expr, KnownRV rv) => expr rv 5 -> expr rv (RVWidth rv)
readReg ridE = iteE (ridE `eqE` litBV 0) (litBV 0) (stateExpr (LocExpr (RegExpr ridE)))

-- | Read a value from a floating point register.
readFReg :: (BVExpr (expr rv), RVStateExpr expr, FExt << rv)
         => expr rv 5
         -> expr rv (RVFloatWidth rv)
readFReg ridE = stateExpr (LocExpr (FRegExpr ridE))

-- | Read a variable number of bytes from memory, with an explicit width argument.
readMem :: RVStateExpr expr
        => NatRepr bytes
        -> expr rv (RVWidth rv)
        -> expr rv (8*bytes)
readMem bytes addr = stateExpr (LocExpr (MemExpr bytes addr))

-- | Read a value from a CSR.
readCSR :: (RVStateExpr expr, KnownRV rv) => expr rv 12 -> expr rv (RVWidth rv)
readCSR csr = stateExpr (LocExpr (CSRExpr csr))

-- | Read the current privilege level.
readPriv :: RVStateExpr expr => expr rv 2
readPriv = stateExpr (LocExpr PrivExpr)

-- | Add a statement to the semantics.
addStmt :: Stmt expr rv -> SemanticsBuilder expr rv ()
addStmt stmt = semStmts %= \stmts -> stmts Seq.|> stmt

-- | Add a PC assignment to the semantics.
assignPC :: expr rv (RVWidth rv) -> SemanticsBuilder (expr rv) rv ()
assignPC pc = addStmt (AssignStmt PCExpr pc)

-- | Add a register assignment to the semantics.
assignReg :: BVExpr (expr rv)
          => expr rv 5
          -> expr rv (RVWidth rv)
          -> SemanticsBuilder (expr rv) rv ()
assignReg r e = addStmt $
  BranchStmt (r `eqE` litBV 0)
  $> Seq.empty
  $> Seq.singleton (AssignStmt (RegExpr r) e)

-- | Add a register assignment to the semantics.
assignFReg :: (BVExpr (expr rv), FExt << rv)
           => expr rv 5
           -> expr rv (RVFloatWidth rv)
           -> SemanticsBuilder (expr rv) rv ()
assignFReg r e = addStmt (AssignStmt (FRegExpr r) e)

-- | Add a memory location assignment to the semantics, with an explicit width argument.
assignMem :: NatRepr bytes
          -> expr rv (RVWidth rv)
          -> expr rv (8*bytes)
          -> SemanticsBuilder (expr rv) rv ()
assignMem bytes addr val = addStmt (AssignStmt (MemExpr bytes addr) val)

-- | Add a CSR assignment to the semantics.
assignCSR :: expr rv 12
          -> expr rv (RVWidth rv)
          -> SemanticsBuilder (expr rv) rv ()
assignCSR csr val = addStmt (AssignStmt (CSRExpr csr) val)

-- | Add a privilege assignment to the semantics.
assignPriv :: expr rv 2 -> SemanticsBuilder (expr rv) rv ()
assignPriv priv = addStmt (AssignStmt PrivExpr priv)

-- | Reserve a memory location.
reserve :: BVExpr (expr rv) => expr rv (RVWidth rv) -> SemanticsBuilder (expr rv) rv ()
reserve addr = addStmt (AssignStmt (ResExpr addr) (litBV 1))

-- | Check that a memory location is reserved.
checkReserved :: RVStateExpr expr => expr rv (RVWidth rv) -> expr rv 1
checkReserved addr = stateExpr (LocExpr (ResExpr addr))

-- | Left-associative application (use with 'branch' to avoid parentheses around @do@
-- notation)
($>) :: (a -> b) -> a -> b
($>) = ($)

infixl 1 $>

-- | Add a branch statement to the semantics. Note that comments in the subsemanticss
-- will be ignored.
branch :: expr rv 1
       -> SemanticsBuilder (expr rv) rv ()
       -> SemanticsBuilder (expr rv) rv ()
       -> SemanticsBuilder (expr rv) rv ()
branch e fbTrue fbFalse = do
  let fTrue  = getSemantics fbTrue  ^. semStmts
      fFalse = getSemantics fbFalse ^. semStmts
  addStmt (BranchStmt e fTrue fFalse)

-- Class instances

instance TestEquality expr => TestEquality (LocExpr expr rv) where
  PCExpr `testEquality` PCExpr = Just Refl
  RegExpr e1 `testEquality` RegExpr e2 = case e1 `testEquality` e2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  MemExpr b1 e1 `testEquality` MemExpr b2 e2 = case (b1 `testEquality` b2, e1 `testEquality` e2) of
    (Just Refl, Just Refl) -> Just Refl
    _ -> Nothing
  ResExpr e1 `testEquality` ResExpr e2 = case e1 `testEquality` e2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  CSRExpr e1 `testEquality` CSRExpr e2 = case e1 `testEquality` e2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  PrivExpr `testEquality` PrivExpr = Just Refl
  _ `testEquality` _ = Nothing

instance TestEquality expr => TestEquality (StateExpr expr rv) where
  LocExpr e1 `testEquality` LocExpr e2 = case e1 `testEquality` e2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  AppExpr e1 `testEquality` AppExpr e2 = case e1 `testEquality` e2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  _ `testEquality` _ = Nothing

instance TestEquality (InstExpr fmt rv) where
  OperandExpr oid1 `testEquality` OperandExpr oid2 = case oid1 `testEquality` oid2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  InstBytes `testEquality` InstBytes = Just Refl
  InstWord `testEquality` InstWord = Just Refl
  InstStateExpr e1 `testEquality` InstStateExpr e2 = case e1 `testEquality` e2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  _ `testEquality` _ = Nothing

instance Eq (InstExpr fmt rv w) where
  x == y = isJust (testEquality x y)

instance Pretty (LocExpr (InstExpr fmt rv) rv w) where
  pPrint PCExpr      = text "pc"
  pPrint (RegExpr e) = text "x[" <> pPrint e <> text "]"
  pPrint (FRegExpr e) = text "f[" <> pPrint e <> text "]"
  pPrint (MemExpr bytes e) = text "M[" <> pPrint e <> text "]_" <> pPrint (natValue bytes)
  pPrint (ResExpr e) = text "MReserved[" <> pPrint e <> text "]"
  pPrint (CSRExpr e) = text "CSR[" <> pPrint e <> text "]"
  pPrint PrivExpr    = text "current_priv"

instance Pretty (InstExpr fmt rv w) where
  pPrint = pPrintInstExpr' True

instance Pretty (Stmt (InstExpr fmt rv) rv) where
  pPrint (AssignStmt le e) = pPrint le <+> text ":=" <+> pPrint e
  pPrint (BranchStmt test s1s s2s) =
    text "IF" <+> pPrint test
    $$ nest 2 (text "THEN")
    $$ nest 4 (vcat (pPrint <$> toList s1s))
    $$ nest 2 (text "ELSE")
    $$ nest 4 (vcat (pPrint <$> toList s2s))

instance Pretty (Semantics (InstExpr fmt rv) rv) where
  pPrint semantics = vcat (pPrint <$> toList (semantics ^. semStmts))

pPrintStateExpr' :: Bool -> StateExpr (InstExpr fmt rv) rv w -> Doc
pPrintStateExpr' _ (LocExpr loc) = pPrint loc
pPrintStateExpr' top (AppExpr app) = pPrintApp' top app

pPrintInstExpr' :: Bool -> InstExpr fmt rv w -> Doc
pPrintInstExpr' _ (OperandExpr (OperandID oid)) = text "arg" <> pPrint (indexValue oid)
pPrintInstExpr' _ InstBytes = text "step"
pPrintInstExpr' _ InstWord = text "inst"
pPrintInstExpr' top (InstStateExpr e) = pPrintStateExpr' top e

pPrintApp' :: Bool -> BVApp (InstExpr fmt rv) w -> Doc
pPrintApp' _ (NotApp e) = text "!" <> pPrintInstExpr' False e
pPrintApp' _ (LitBVApp bv) = text $ show bv
pPrintApp' _ (ZExtApp _ e) = text "zext(" <> pPrintInstExpr' True e <> text ")"
pPrintApp' _ (SExtApp _ e) = text "sext(" <> pPrintInstExpr' True e <> text ")"
pPrintApp' top (ExtractApp w ix e) =
  pPrintInstExpr' top e <> text "[" <> pPrint ix <> text ":" <>
  pPrint (ix + fromIntegral (natValue w) - 1) <> text "]"
pPrintApp' False e = parens (pPrintApp' True e)
pPrintApp' _ (AndApp e1 e2) = pPrintInstExpr' False e1 <+> text "&" <+> pPrintInstExpr' False e2
pPrintApp' _ (OrApp  e1 e2) = pPrintInstExpr' False e1 <+> text "|" <+> pPrintInstExpr' False e2
pPrintApp' _ (XorApp e1 e2) = pPrintInstExpr' False e1 <+> text "^" <+> pPrintInstExpr' False e2
pPrintApp' _ (SllApp e1 e2) = pPrintInstExpr' False e1 <+> text "<<" <+> pPrintInstExpr' False e2
pPrintApp' _ (SrlApp e1 e2) = pPrintInstExpr' False e1 <+> text ">l>" <+> pPrintInstExpr' False e2
pPrintApp' _ (SraApp e1 e2) = pPrintInstExpr' False e1 <+> text ">a>" <+> pPrintInstExpr' False e2
pPrintApp' _ (AddApp e1 e2) = pPrintInstExpr' False e1 <+> text "+" <+> pPrintInstExpr' False e2
pPrintApp' _ (SubApp e1 e2) = pPrintInstExpr' False e1 <+> text "-" <+> pPrintInstExpr' False e2
pPrintApp' _ (MulApp e1 e2) = pPrintInstExpr' False e1 <+> text "*" <+> pPrintInstExpr' False e2
pPrintApp' _ (QuotUApp e1 e2) = pPrintInstExpr' False e1 <+> text "/u" <+> pPrintInstExpr' False e2
pPrintApp' _ (QuotSApp e1 e2) = pPrintInstExpr' False e1 <+> text "/s" <+> pPrintInstExpr' False e2
pPrintApp' _ (RemUApp e1 e2) = pPrintInstExpr' False e1 <+> text "%u" <+> pPrintInstExpr' False e2
pPrintApp' _ (RemSApp e1 e2) = pPrintInstExpr' False e1 <+> text "%s" <+> pPrintInstExpr' False e2
pPrintApp' _ (EqApp  e1 e2) = pPrintInstExpr' False e1 <+> text "==" <+> pPrintInstExpr' False e2
pPrintApp' _ (LtuApp e1 e2) = pPrintInstExpr' False e1 <+> text "<u" <+> pPrintInstExpr' False e2
pPrintApp' _ (LtsApp e1 e2) = pPrintInstExpr' False e1 <+> text "<s" <+> pPrintInstExpr' False e2
pPrintApp' _ (ConcatApp e1 e2) =
  text "{" <> pPrintInstExpr' True e1 <> text ", " <> pPrintInstExpr' True e2 <> text "}"
pPrintApp' _ (IteApp e1 e2 e3) =
  text "if" <+> pPrintInstExpr' True e1 <+>
  text "then" <+> pPrintInstExpr' True e2 <+>
  text "else" <+> pPrintInstExpr' True e3
