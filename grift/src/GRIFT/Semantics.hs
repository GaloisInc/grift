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
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

{-|
Module      : GRIFT.Semantics
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

These two expression types are formed from the "open" expression type, 'StateApp',
which encapsulates the notion of compound expressions involving pieces of RISC-V
machine state. The 'StateApp' type has a type parameter @expr@, which allows us to
build compound expressions using sub-expressions of type @expr@. 'PureStateExpr' and
'InstExpr' both contain 'StateApp' as a special case, where the @expr@ parameter is
instantiated as 'PureStateExpr' and 'InstExpr', respectively, thus "tying the knot"
and allowing us to construct arbitrary bitvector expressions involving the RISC-V
machine state and, in the case of 'InstExpr', we can also refer to the instruction
currently being executed.

We also export a typeclass 'StateExpr', implemented by both 'PureStateExpr' and
'InstExpr'. This contains a single method, 'stateExpr', which allows us to embed a
'StateApp' into these types. This class is useful for defining semantic formulas
that could be executed either inside or outside the context of an executing
instruction.

Using these expressions, we can also construct assignments of pieces of state to
expressions. This is encapsulated in the 'Stmt' type, whose constructor 'AssignStmt'
does exactly this. We can also create 'BranchStmt's which conditionally execute one
of two distinct sets of statements based on a test condition.

Finally, we can combine 'Stmt's to create 'Semantics's, which are simply sequences of
statements. We export a monad, 'SemanticsM', to facilitate straightforward
definitions of these assignments, and a number of functions ('assignPC', 'assignGPR',
etc.) that can be used within this monad. To see examples of its use, take a look at
'GRIFT.Extensions.Base', which contains the base RISC-V ISA instruction definitions,
defined using 'SemanticsBuilder'.

-}

module GRIFT.Semantics
  ( -- * 'BitVector' semantic expressions
    module Data.BitVector.Sized.App
  , module Data.BitVector.Sized.Float.App
    -- * RISC-V semantic expressions
  , LocApp(..)
  , StateApp(..)
  , StateExpr(..)
  , PureStateExpr(..)
  , InstExpr(..)
    -- ** Access to state
  , readPC
  , readGPR
  , readFPR
  , readMem
  , rawReadCSR
  , readPriv
  , checkReserved
    -- * RISC-V semantic statements and formulas
  , Stmt(..)
  , Semantics, semComments, semStmts
  , InstSemantics(..)
  , instSemantics
    -- * SemanticsM monad
  , SemanticsM
  , getSemantics
    -- * SemanticsM operations
    -- ** Auxiliary
  , comment
  , operandEs, operandEsWithRepr
  , instBytes
  , instWord
    -- ** State actions
  , assignPC
  , assignGPR
  , assignFPR
  , assignMem
  , assignCSR
  , assignPriv
  , reserve
  , branch
  , ($>)
    -- ** Pretty printing
  , pPrintInstExpr
  , pPrintInstSemantics
  , pPrintOperandName
  ) where

import Control.Lens ( (%=), (^.), Simple, Lens, lens )
import Control.Monad.Fail
import Control.Monad.State
import Data.BitVector.Sized.App
import Data.BitVector.Sized.Float.App
import Data.Foldable
import Data.Parameterized
import Data.Parameterized.List
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import GHC.TypeLits
import qualified GHC.TypeLits as T
import Prelude hiding ((<>), (!!))
import Text.PrettyPrint.HughesPJClass

import GRIFT.Types

----------------------------------------
-- Expressions, statements, and semantics

-- | This type represents an abstract component of the global state. Sub-expressions
-- come from an arbitrary expression language @expr@.
data LocApp (expr :: Nat -> *) (rv :: RV) (w :: Nat) where
  PCExpr   :: LocApp expr rv (RVWidth rv)
  GPRExpr  :: expr 5 -> LocApp expr rv (RVWidth rv)
  FPRExpr :: FExt << rv => expr 5 -> LocApp expr rv (RVFloatWidth rv)
  MemExpr  :: NatRepr bytes -> expr (RVWidth rv) -> LocApp expr rv (8 T.* bytes)
  ResExpr  :: expr (RVWidth rv) -> LocApp expr rv 1
  CSRExpr  :: expr 12 -> LocApp expr rv (RVWidth rv)
  PrivExpr :: LocApp expr rv 2

-- | Expressions for general computations over the RISC-V machine state -- we can
-- access specific locations, and we can also build up compound expressions using the
-- 'BVApp' and 'BVFloatApp' expression language. Sub-expressions come from an
-- arbitrary expression language @expr@.
data StateApp (expr :: Nat -> *) (rv :: RV) (w :: Nat) where
  -- | Accessing state
  LocApp :: !(LocApp expr rv w) -> StateApp expr rv w

  -- | 'BVApp' with 'StateApp' subexpressions
  AppExpr :: !(BVApp expr w) -> StateApp expr rv w

  -- | 'BVFloatApp' with 'StateApp' subexpressions
  FloatAppExpr :: FExt << rv => !(BVFloatApp expr w) -> StateApp expr rv w

-- | Expressions built purely from 'StateExpr's, which are executed outside the
-- context of an executing instruction (for instance, during exception handling).
newtype PureStateExpr (rv :: RV) (w :: Nat) = PureStateExpr (StateApp (PureStateExpr rv) rv w)

instance BVExpr (PureStateExpr rv) where
  appExpr = PureStateExpr . AppExpr

instance FExt << rv => BVFloatExpr (PureStateExpr rv) where
  floatAppExpr = PureStateExpr . FloatAppExpr

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
  InstStateExpr :: !(StateApp (InstExpr fmt rv) rv w) -> InstExpr fmt rv w

instance BVExpr (InstExpr fmt rv) where
  appExpr = InstStateExpr . AppExpr

instance FExt << rv =>  BVFloatExpr (InstExpr fmt rv) where
  floatAppExpr = InstStateExpr . FloatAppExpr

-- TODO: When we get quantified constraints, put a forall arch. BVExpr (expr arch)
-- here
-- | A type class for expression languages that can refer to arbitrary pieces of
-- RISC-V machine state.
class StateExpr (expr :: RV -> Nat -> *) where
  stateExpr :: StateApp (expr rv) rv w -> expr rv w

instance StateExpr PureStateExpr where
  stateExpr = PureStateExpr

instance StateExpr (InstExpr fmt) where
  stateExpr = InstStateExpr

-- | A 'Stmt' represents an atomic state transformation -- typically, an assignment
-- of a state component (register, memory location, etc.) to an expression of the
-- appropriate width ('AssignStmt'). There is also 'BranchStmt', which represents a
-- conditional execution of one of two blocks of 'Stmt's, depending on whether the
-- condition evaluates to true or false.
data Stmt (expr :: Nat -> *) (rv :: RV) where
  -- | Assign a piece of state to a value.
  AssignStmt :: !(LocApp expr rv w) -> !(expr w) -> Stmt expr rv
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
              }

-- | A wrapper for 'Semantics's over 'InstExpr's with the 'Format' type parameter
-- appearing last, for the purposes of associating with an corresponding 'Opcode' of
-- the same format. We also associate the `OperandTypes` of the `Format` with a
-- particular list of `OperandName`s for pretty printing purposes.
data InstSemantics (rv :: RV) (fmt :: Format)
  = InstSemantics { getInstSemantics :: Semantics (InstExpr fmt rv) rv
                  , getOperandNames :: List OperandName (OperandTypes fmt)
                  }

instSemantics :: List OperandName (OperandTypes fmt)
              -> SemanticsM (InstExpr fmt rv) rv ()
              -> InstSemantics rv fmt
instSemantics opNames semM = InstSemantics (getSemantics semM) opNames

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
newtype SemanticsM (expr :: Nat -> *) (rv :: RV) a =
  SemanticsM { unSemanticsM :: State (Semantics expr rv) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadState (Semantics expr rv))

instance MonadFail (SemanticsM expr rv) where
  fail = error

-- | Get the operands for a particular known format.
operandEs :: forall rv fmt . (KnownRepr FormatRepr fmt)
          => SemanticsM (InstExpr fmt rv) rv (List (InstExpr fmt rv) (OperandTypes fmt))
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

-- | Obtain the semantics defined by a 'SemanticsM' action.
getSemantics :: SemanticsM expr rv () -> Semantics expr rv
getSemantics = flip execState emptySemantics . unSemanticsM

-- | Add a comment.
comment :: String -> SemanticsM expr rv ()
comment c = semComments %= \cs -> cs Seq.|> c

-- | Get the width of the instruction word
instBytes :: SemanticsM (InstExpr fmt rv) rv (InstExpr fmt rv (RVWidth rv))
instBytes = return InstBytes

-- | Get the entire instruction word (useful for exceptions)
instWord :: SemanticsM (InstExpr fmt rv) rv (InstExpr fmt rv (RVWidth rv))
instWord = return InstWord

-- | Read the pc.
readPC :: StateExpr expr => expr rv (RVWidth rv)
readPC = stateExpr (LocApp PCExpr)

-- | Read a value from a register. Register x0 is hardwired to 0.
readGPR :: (BVExpr (expr rv), StateExpr expr, KnownRVWidth rv) => expr rv 5 -> expr rv (RVWidth rv)
readGPR ridE = iteE (ridE `eqE` litBV 0) (litBV 0) (stateExpr (LocApp (GPRExpr ridE)))

-- | Read a value from a floating point register.
readFPR :: (BVExpr (expr rv), StateExpr expr, FExt << rv)
         => expr rv 5
         -> expr rv (RVFloatWidth rv)
readFPR ridE = stateExpr (LocApp (FPRExpr ridE))

-- TODO: We need a wrapper around this to handle access faults.
-- | Read a variable number of bytes from memory, with an explicit width argument.
readMem :: StateExpr expr
        => NatRepr bytes
        -> expr rv (RVWidth rv)
        -> expr rv (8 T.* bytes)
readMem bytes addr = stateExpr (LocApp (MemExpr bytes addr))

-- | Read a value from a CSR.
rawReadCSR :: (StateExpr expr, KnownRVWidth rv) => expr rv 12 -> expr rv (RVWidth rv)
rawReadCSR csr = stateExpr (LocApp (CSRExpr csr))

-- | Read the current privilege level.
readPriv :: StateExpr expr => expr rv 2
readPriv = stateExpr (LocApp PrivExpr)

-- | Add a statement to the semantics.
addStmt :: Stmt expr rv -> SemanticsM expr rv ()
addStmt stmt = semStmts %= \stmts -> stmts Seq.|> stmt

-- | Add a PC assignment to the semantics.
assignPC :: expr rv (RVWidth rv) -> SemanticsM (expr rv) rv ()
assignPC pc = addStmt (AssignStmt PCExpr pc)

-- | Add a register assignment to the semantics.
assignGPR :: BVExpr (expr rv)
          => expr rv 5
          -> expr rv (RVWidth rv)
          -> SemanticsM (expr rv) rv ()
assignGPR r e = addStmt $
  BranchStmt (r `eqE` litBV 0)
  $> Seq.empty
  $> Seq.singleton (AssignStmt (GPRExpr r) e)

-- | Add a register assignment to the semantics.
assignFPR :: (BVExpr (expr rv), FExt << rv)
           => expr rv 5
           -> expr rv (RVFloatWidth rv)
           -> SemanticsM (expr rv) rv ()
assignFPR r e = addStmt (AssignStmt (FPRExpr r) e)

-- TODO: We need a wrapper around this to handle access faults.
-- | Add a memory location assignment to the semantics, with an explicit width argument.
assignMem :: NatRepr bytes
          -> expr rv (RVWidth rv)
          -> expr rv (8 T.* bytes)
          -> SemanticsM (expr rv) rv ()
assignMem bytes addr val = addStmt (AssignStmt (MemExpr bytes addr) val)

-- | Add a CSR assignment to the semantics.
assignCSR :: expr rv 12
          -> expr rv (RVWidth rv)
          -> SemanticsM (expr rv) rv ()
assignCSR csr val = addStmt (AssignStmt (CSRExpr csr) val)

-- | Add a privilege assignment to the semantics.
assignPriv :: expr rv 2 -> SemanticsM (expr rv) rv ()
assignPriv priv = addStmt (AssignStmt PrivExpr priv)

-- | Reserve a memory location.
reserve :: BVExpr (expr rv) => expr rv (RVWidth rv) -> SemanticsM (expr rv) rv ()
reserve addr = addStmt (AssignStmt (ResExpr addr) (litBV 1))

-- | Check that a memory location is reserved.
checkReserved :: StateExpr expr => expr rv (RVWidth rv) -> expr rv 1
checkReserved addr = stateExpr (LocApp (ResExpr addr))

-- | Left-associative application (use with 'branch' to avoid parentheses around @do@
-- notation)
($>) :: (a -> b) -> a -> b
($>) = ($)

infixl 1 $>

-- | Add a branch statement to the semantics. Note that comments in the subsemantics
-- will be ignored.
branch :: expr 1 -> SemanticsM expr rv () -> SemanticsM expr rv () -> SemanticsM expr rv ()
branch e fbTrue fbFalse = do
  let fTrue  = getSemantics fbTrue  ^. semStmts
      fFalse = getSemantics fbFalse ^. semStmts
  addStmt (BranchStmt e fTrue fFalse)

-- Class instances

instance TestEquality expr => TestEquality (LocApp expr rv) where
  PCExpr `testEquality` PCExpr = Just Refl
  GPRExpr e1 `testEquality` GPRExpr e2 = case e1 `testEquality` e2 of
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

instance TestEquality expr => TestEquality (StateApp expr rv) where
  LocApp e1 `testEquality` LocApp e2 = case e1 `testEquality` e2 of
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

-- Pretty printing

pPrintLocApp :: (forall w' . Bool -> expr w' -> Doc)
             -> Bool
             -> LocApp expr rv w
             -> Doc
pPrintLocApp _ _ PCExpr = text "pc"
pPrintLocApp ppExpr top (GPRExpr e) = text "x[" <> ppExpr top e <> text "]"
pPrintLocApp ppExpr top (FPRExpr e) = text "f[" <> ppExpr top e <> text "]"
pPrintLocApp ppExpr top (MemExpr bytes e) = text "M[" <> ppExpr top e <> text "]_" <> pPrint (intValue bytes)
pPrintLocApp ppExpr top (ResExpr e) = text "MReserved[" <> ppExpr top e <> text "]"
pPrintLocApp ppExpr top (CSRExpr e) = text "CSR[" <> ppExpr top e <> text "]"
pPrintLocApp _ _ PrivExpr = text "current_priv"

pPrintStateApp :: (forall w' . Bool -> expr w' -> Doc)
               -> Bool
               -> StateApp expr rv w
               -> Doc
pPrintStateApp ppExpr top (LocApp loc) = pPrintLocApp ppExpr top loc
pPrintStateApp ppExpr top (AppExpr app) = pPrintBVApp ppExpr top app
pPrintStateApp ppExpr top (FloatAppExpr app) = pPrintBVFloatApp ppExpr top app

pPrintBVApp :: (forall w' . Bool -> expr w' -> Doc)
            -> Bool
            -> BVApp expr w
            -> Doc
pPrintBVApp ppExpr _ (NotApp e) = text "!" <> ppExpr False e
pPrintBVApp ppExpr _ (NegateApp e) = text "-" <> ppExpr False e
pPrintBVApp _ _ (LitBVApp bv) = text $ show bv
pPrintBVApp ppExpr _ (AbsApp e) = text "|" <> ppExpr True e <> text "|"
pPrintBVApp ppExpr _ (SignumApp e) = text "signum(" <> ppExpr True e <> text ")"
pPrintBVApp ppExpr _ (ZExtApp _ e) = text "zext(" <> ppExpr True e <> text ")"
pPrintBVApp ppExpr _ (SExtApp _ e) = text "sext(" <> ppExpr True e <> text ")"
pPrintBVApp ppExpr top (ExtractApp w ix e) =
  ppExpr top e <> text "[" <> pPrint ix <> text ":" <>
  pPrint (ix + fromIntegral (natValue w) - 1) <> text "]"
pPrintBVApp ppExpr False e = parens (pPrintBVApp ppExpr True e)
pPrintBVApp ppExpr _ (AndApp e1 e2) = ppExpr False e1 <+> text "&" <+> ppExpr False e2
pPrintBVApp ppExpr _ (OrApp  e1 e2) = ppExpr False e1 <+> text "|" <+> ppExpr False e2
pPrintBVApp ppExpr _ (XorApp e1 e2) = ppExpr False e1 <+> text "^" <+> ppExpr False e2
pPrintBVApp ppExpr _ (SllApp e1 e2) = ppExpr False e1 <+> text "<<" <+> ppExpr False e2
pPrintBVApp ppExpr _ (SrlApp e1 e2) = ppExpr False e1 <+> text ">l>" <+> ppExpr False e2
pPrintBVApp ppExpr _ (SraApp e1 e2) = ppExpr False e1 <+> text ">a>" <+> ppExpr False e2
pPrintBVApp ppExpr _ (AddApp e1 e2) = ppExpr False e1 <+> text "+" <+> ppExpr False e2
pPrintBVApp ppExpr _ (SubApp e1 e2) = ppExpr False e1 <+> text "-" <+> ppExpr False e2
pPrintBVApp ppExpr _ (MulApp e1 e2) = ppExpr False e1 <+> text "*" <+> ppExpr False e2
pPrintBVApp ppExpr _ (QuotUApp e1 e2) = ppExpr False e1 <+> text "/u" <+> ppExpr False e2
pPrintBVApp ppExpr _ (QuotSApp e1 e2) = ppExpr False e1 <+> text "/s" <+> ppExpr False e2
pPrintBVApp ppExpr _ (RemUApp e1 e2) = ppExpr False e1 <+> text "%u" <+> ppExpr False e2
pPrintBVApp ppExpr _ (RemSApp e1 e2) = ppExpr False e1 <+> text "%s" <+> ppExpr False e2
pPrintBVApp ppExpr _ (EqApp  e1 e2) = ppExpr False e1 <+> text "==" <+> ppExpr False e2
pPrintBVApp ppExpr _ (LtuApp e1 e2) = ppExpr False e1 <+> text "<u" <+> ppExpr False e2
pPrintBVApp ppExpr _ (LtsApp e1 e2) = ppExpr False e1 <+> text "<s" <+> ppExpr False e2
pPrintBVApp ppExpr _ (ConcatApp e1 e2) =
  text "{" <> ppExpr True e1 <> text ", " <> ppExpr True e2 <> text "}"
pPrintBVApp ppExpr _ (IteApp e1 e2 e3) =
  text "if" <+> ppExpr True e1 <+>
  text "then" <+> ppExpr True e2 <+>
  text "else" <+> ppExpr True e3

pPrintBVFloatApp :: (forall w' . Bool -> expr w' -> Doc)
            -> Bool
            -> BVFloatApp expr w
            -> Doc

pPrintBVFloatApp ppExpr _ (Ui32ToF16App rm e) =
  text "ui32ToF16(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (Ui32ToF32App rm e) =
  text "ui32ToF32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (Ui32ToF64App rm e) =
  text "ui32ToF64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (I32ToF16App rm e) =
  text "i32ToF16(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (I32ToF32App rm e) =
  text "i32ToF32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (I32ToF64App rm e) =
  text "i32ToF64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (Ui64ToF16App rm e) =
  text "ui64ToF16(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (Ui64ToF32App rm e) =
  text "ui64ToF32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (Ui64ToF64App rm e) =
  text "ui64ToF64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (I64ToF16App rm e) =
  text "i64ToF16(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (I64ToF32App rm e) =
  text "i64ToF32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (I64ToF64App rm e) =
  text "i64ToF64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"

pPrintBVFloatApp ppExpr _ (F16ToUi32App rm e) =
  text "f16ToUi32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16ToUi64App rm e) =
  text "f16ToUi64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16ToI32App rm e) =
  text "f16ToI32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16ToI64App rm e) =
  text "f16ToI64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32ToUi32App rm e) =
  text "f32ToUi32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32ToUi64App rm e) =
  text "f32ToUi64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32ToI32App rm e) =
  text "f32ToI32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32ToI64App rm e) =
  text "f32ToI64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64ToUi32App rm e) =
  text "f64ToUi32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64ToUi64App rm e) =
  text "f64ToUi64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64ToI32App rm e) =
  text "f64ToI32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64ToI64App rm e) =
  text "f64ToI64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"

pPrintBVFloatApp ppExpr _ (F16ToF32App rm e) =
  text "f16ToF32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16ToF64App rm e) =
  text "f16ToF64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32ToF16App rm e) =
  text "f32ToF16(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32ToF64App rm e) =
  text "f32ToF64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64ToF16App rm e) =
  text "f64ToF16(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64ToF32App rm e) =
  text "f64ToF32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"

pPrintBVFloatApp ppExpr _ (F16RoundToIntApp rm e) =
  text "f16RoundToInt(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16AddApp rm x y) =
  text "f16Add(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16SubApp rm x y) =
  text "f16Sub(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16MulApp rm x y) =
  text "f16Mul(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16MulAddApp rm x y z) =
  text "f16MulAdd(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", " <> ppExpr True z <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16DivApp rm x y) =
  text "f16Div(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16RemApp rm x y) =
  text "f16Rem(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16SqrtApp rm e) =
  text "f16Sqrt(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16EqApp x y) =
  text "f16Eq(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F16LeApp x y) =
  text "f16Le(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F16LtApp x y) =
  text "f16Lt(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F16EqSignalingApp x y) =
  text "f16EqSignaling(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F16LeQuietApp x y) =
  text "f16LeQuiet(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F16LtQuietApp x y) =
  text "f16LtQuiet(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F16IsSignalingNaNApp x) =
  text "f16IsSignalingNaN(" <> ppExpr True x <>  text ")"

pPrintBVFloatApp ppExpr _ (F32RoundToIntApp rm e) =
  text "f32RoundToInt(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32AddApp rm x y) =
  text "f32Add(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32SubApp rm x y) =
  text "f32Sub(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32MulApp rm x y) =
  text "f32Mul(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32MulAddApp rm x y z) =
  text "f32MulAdd(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", " <> ppExpr True z <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32DivApp rm x y) =
  text "f32Div(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32RemApp rm x y) =
  text "f32Rem(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32SqrtApp rm e) =
  text "f32Sqrt(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32EqApp x y) =
  text "f32Eq(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F32LeApp x y) =
  text "f32Le(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F32LtApp x y) =
  text "f32Lt(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F32EqSignalingApp x y) =
  text "f32EqSignaling(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F32LeQuietApp x y) =
  text "f32LeQuiet(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F32LtQuietApp x y) =
  text "f32LtQuiet(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F32IsSignalingNaNApp x) =
  text "f32IsSignalingNaN(" <> ppExpr True x <>  text ")"

pPrintBVFloatApp ppExpr _ (F64RoundToIntApp rm e) =
  text "f64RoundToInt(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64AddApp rm x y) =
  text "f64Add(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64SubApp rm x y) =
  text "f64Sub(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64MulApp rm x y) =
  text "f64Mul(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64MulAddApp rm x y z) =
  text "f64MulAdd(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", " <> ppExpr True z <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64DivApp rm x y) =
  text "f64Div(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64RemApp rm x y) =
  text "f64Rem(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64SqrtApp rm e) =
  text "f64Sqrt(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64EqApp x y) =
  text "f64Eq(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F64LeApp x y) =
  text "f64Le(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F64LtApp x y) =
  text "f64Lt(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F64EqSignalingApp x y) =
  text "f64EqSignaling(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F64LeQuietApp x y) =
  text "f64LeQuiet(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F64LtQuietApp x y) =
  text "f64LtQuiet(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F64IsSignalingNaNApp x) =
  text "f64IsSignalingNaN(" <> ppExpr True x <>  text ")"

pPrintStmt :: (forall w' . Bool -> expr w' -> Doc)
       -> Stmt expr rv
       -> Doc
pPrintStmt ppExpr (AssignStmt le e) = pPrintLocApp ppExpr True le <+> text ":=" <+> ppExpr True e
pPrintStmt ppExpr (BranchStmt test s1s s2s) =
  text "IF" <+> ppExpr True test
  $$ nest 2 (text "THEN")
  $$ nest 4 (vcat (pPrintStmt ppExpr <$> toList s1s))
  $$ nest 2 (text "ELSE")
  $$ nest 4 (vcat (pPrintStmt ppExpr <$> toList s2s))

pPrintSemantics :: (forall w' . Bool -> expr w' -> Doc)
                -> Semantics expr rv
                -> Doc
pPrintSemantics ppExpr semantics = (vcat $ text <$> toList (semantics ^. semComments)) $$
                                   (vcat $ pPrintStmt ppExpr <$> toList (semantics ^. semStmts))

pPrintOperandName :: OperandName w -> Doc
pPrintOperandName Aq = text "aq"
pPrintOperandName Rl = text "rl"
pPrintOperandName Rm = text "rm"
pPrintOperandName Rd = text "rd"
pPrintOperandName Rs1 = text "rs1"
pPrintOperandName Rs2 = text "rs2"
pPrintOperandName Rs3 = text "rs3"
pPrintOperandName Imm5 = text "imm5"
pPrintOperandName Shamt5 = text "shamt5"
pPrintOperandName Shamt7 = text "shamt7"
pPrintOperandName Imm12 = text "imm12"
pPrintOperandName Csr = text "csr"
pPrintOperandName Imm20 = text "imm20"
pPrintOperandName Imm32 = text "imm32"

pPrintInstExpr :: List OperandName (OperandTypes fmt)
               -> Bool
               -> InstExpr fmt rv w
               -> Doc
pPrintInstExpr opNames _ (OperandExpr (OperandID oid)) = pPrintOperandName (opNames !! oid)
pPrintInstExpr _ _ InstBytes = text "step"
pPrintInstExpr _ _ InstWord = text "inst"
pPrintInstExpr opNames top (InstStateExpr e) = pPrintStateApp (pPrintInstExpr opNames) top e

pPrintInstSemantics :: InstSemantics rv fmt -> Doc
pPrintInstSemantics (InstSemantics semantics opNames) =
  pPrintSemantics (pPrintInstExpr opNames) semantics
