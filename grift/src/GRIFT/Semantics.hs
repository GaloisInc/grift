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
    module Data.BitVector.Sized
  , module Data.BitVector.Sized.App
  , module Data.BitVector.Sized.Float.App
    -- * RISC-V semantic expressions
  , LocApp(..)
  , StateApp(..)
  , AbbrevApp(..)
  , AbbrevExpr(..)
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
  , AbbrevStmt(..)
  , Stmt(..)
  , Semantics, semComments, semStmts
  , InstSemantics(..)
  , instSemantics
    -- * SemanticsM monad
  , SemanticsM
  , getSemantics
    -- * SemanticsM operations
    -- ** Auxiliary
  , addStmt
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
import Data.BitVector.Sized
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
  PCApp   :: NatRepr (RVWidth rv) -> LocApp expr rv (RVWidth rv)
  GPRApp  :: NatRepr (RVWidth rv) -> expr 5 -> LocApp expr rv (RVWidth rv)
  FPRApp  :: FExt << rv => NatRepr (RVFloatWidth rv) -> expr 5 -> LocApp expr rv (RVFloatWidth rv)
  MemApp  :: NatRepr bytes -> expr (RVWidth rv) -> LocApp expr rv (8 T.* bytes)
  ResApp  :: AExt << rv => expr (RVWidth rv) -> LocApp expr rv 1
  CSRApp  :: NatRepr (RVWidth rv) -> expr 12 -> LocApp expr rv (RVWidth rv)
  PrivApp :: LocApp expr rv 2

-- | Get the width of a 'LocApp'.
locAppWidth :: LocApp expr rv w -> NatRepr w
locAppWidth (PCApp wRepr) = wRepr
locAppWidth (GPRApp wRepr _) = wRepr
locAppWidth (FPRApp wRepr _) = wRepr
locAppWidth (MemApp bytesRepr _) = (knownNat @8) `natMultiply` bytesRepr
locAppWidth (ResApp _) = knownNat
locAppWidth (CSRApp wRepr _) = wRepr
locAppWidth PrivApp = knownNat

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

-- | Get the width of a 'StateApp'.
stateAppWidth :: StateApp expr rv w -> NatRepr w
stateAppWidth (LocApp locApp) = locAppWidth locApp
stateAppWidth (AppExpr bvApp) = bvAppWidth bvApp
stateAppWidth (FloatAppExpr bvFloatApp) = bvFloatAppWidth bvFloatApp

-- TODO: When we get quantified constraints, put a forall arch. BVExpr (expr arch)
-- here
-- | A type class for expression languages that can refer to arbitrary pieces of
-- RISC-V machine state.
class StateExpr (expr :: RV -> Nat -> *) where
  stateExpr :: StateApp (expr rv) rv w -> expr rv w

-- | Abbreviated expressions. These expand into a concrete semantic meaning, but can
-- be pretty-printed in a nice way without full expansion.
data AbbrevApp (expr :: RV -> Nat -> *) (rv :: RV) (w :: Nat) where
  SafeGPRApp :: ( BVExpr (expr rv), StateExpr expr ) =>
                NatRepr (RVWidth rv) -> expr rv 5 -> AbbrevApp expr rv (RVWidth rv)

-- | A type class for expression languages that support 'AbbrevApp' embedding.
class AbbrevExpr expr where
  abbrevExpr :: AbbrevApp expr rv w -> expr rv w

-- | Get the width of an 'AbbrevApp'.
abbrevAppWidth :: AbbrevApp expr rv w -> NatRepr w
abbrevAppWidth (SafeGPRApp wRepr _) = wRepr

-- | Expressions built purely from 'StateExpr's, which are executed outside the
-- context of an executing instruction (for instance, during exception handling).
data PureStateExpr (rv :: RV) (w :: Nat)
  = PureStateLitBV (BitVector w)
  | PureAbbrevApp (AbbrevApp PureStateExpr rv w)
  | PureStateApp (StateApp (PureStateExpr rv) rv w)

instance BVExpr (PureStateExpr rv) where
  litBV = PureStateLitBV

  exprWidth (PureStateLitBV (BitVector wRepr _)) = wRepr
  exprWidth (PureAbbrevApp abbrevApp) = abbrevAppWidth abbrevApp
  exprWidth (PureStateApp bvApp) = stateAppWidth bvApp

  appExpr bvApp = PureStateApp (AppExpr bvApp)

instance StateExpr PureStateExpr where
  stateExpr = PureStateApp

instance AbbrevExpr PureStateExpr where
  abbrevExpr = PureAbbrevApp

instance FExt << rv => BVFloatExpr (PureStateExpr rv) where
  floatAppExpr = PureStateApp . FloatAppExpr

-- | Expressions for computations over the RISC-V machine state, in the context of
-- an executing instruction.
data InstExpr (fmt :: Format) (rv :: RV) (w :: Nat) where
  -- | Literal BitVector
  InstLitBV :: !(BitVector w) -> InstExpr fmt rv w
  -- | Abbreviation
  InstAbbrevApp :: !(AbbrevApp (InstExpr fmt) rv w) -> InstExpr fmt rv w
  -- | Accessing the instruction operands
  OperandExpr :: !(NatRepr w) -> !(OperandID fmt w) -> InstExpr fmt rv w
  -- | Accessing the instruction width, in number of bytes
  InstBytes :: !(NatRepr (RVWidth rv)) -> InstExpr fmt rv (RVWidth rv)
  -- | Accessing the entire instruction word itself
  InstWord :: !(NatRepr (RVWidth rv)) -> InstExpr fmt rv (RVWidth rv)

  -- | Accessing the machine state
  InstStateApp :: !(StateApp (InstExpr fmt rv) rv w) -> InstExpr fmt rv w

instance BVExpr (InstExpr fmt rv) where
  litBV = InstLitBV

  exprWidth (InstLitBV (BitVector wRepr _)) = wRepr
  exprWidth (InstAbbrevApp abbrevApp) = abbrevAppWidth abbrevApp
  exprWidth (OperandExpr wRepr _) = wRepr
  exprWidth (InstBytes wRepr) = wRepr
  exprWidth (InstWord wRepr) = wRepr
  exprWidth (InstStateApp stateApp) = stateAppWidth stateApp

  appExpr = InstStateApp . AppExpr

instance FExt << rv =>  BVFloatExpr (InstExpr fmt rv) where
  floatAppExpr = InstStateApp . FloatAppExpr

instance StateExpr (InstExpr fmt) where
  stateExpr = InstStateApp

instance AbbrevExpr (InstExpr fmt) where
  abbrevExpr = InstAbbrevApp

-- | A 'Stmt' represents an atomic state transformation -- typically, an assignment
-- of a state component (register, memory location, etc.) to an expression of the
-- appropriate width ('AssignStmt'). There is also 'BranchStmt', which represents a
-- conditional execution of one of two blocks of 'Stmt's, depending on whether the
-- condition evaluates to true or false.
data Stmt (expr :: RV -> Nat -> *) (rv :: RV) where
  -- | Assign a piece of state to a value.
  AssignStmt :: !(LocApp (expr rv) rv w) -> !(expr rv w) -> Stmt expr rv
  -- | Abbreviated statement
  AbbrevStmt :: !(AbbrevStmt expr rv) -> Stmt expr rv
  -- | If-then-else branch statement.
  BranchStmt :: !(expr rv 1)
             -> !(Seq (Stmt expr rv))
             -> !(Seq (Stmt expr rv))
             -> Stmt expr rv

-- | An abbreviated statement. Expands into a larger 'Stmt' for simulation, but is a
-- nice abstraction for pretty printing.
data AbbrevStmt expr rv where
  SafeGPRAssign :: BVExpr (expr rv)
                => !(expr rv 5)
                -> !(expr rv (RVWidth rv))
                -> AbbrevStmt expr rv
  RaiseException :: (BVExpr (expr rv), StateExpr expr)
                 => !(BitVector 12)
                 -> !(expr rv (RVWidth rv))
                 -> AbbrevStmt expr rv

-- | A 'Semantics' is simply a set of simultaneous 'Stmt's.
data Semantics (expr :: RV -> Nat -> *) (rv :: RV)
  = Semantics { _semComments :: !(Seq String)
                -- ^ multiline comment
              , _semStmts    :: !(Seq (Stmt expr rv))
              }

-- | A wrapper for 'Semantics's over 'InstExpr's with the 'Format' type parameter
-- appearing last, for the purposes of associating with an corresponding 'Opcode' of
-- the same format. We also associate the `OperandTypes` of the `Format` with a
-- particular list of `OperandName`s for pretty printing purposes.
data InstSemantics (rv :: RV) (fmt :: Format)
  = InstSemantics { getInstSemantics :: Semantics (InstExpr fmt) rv
                  , getOperandNames :: List OperandName (OperandTypes fmt)
                  }

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
newtype SemanticsM (expr :: RV -> Nat -> *) (rv :: RV) a =
  SemanticsM { unSemanticsM :: State (Semantics expr rv) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadState (Semantics expr rv))

instance MonadFail (SemanticsM expr rv) where
  fail = error

instSemantics :: List OperandName (OperandTypes fmt)
              -> SemanticsM (InstExpr fmt) rv ()
              -> InstSemantics rv fmt
instSemantics opNames semM = InstSemantics (getSemantics semM) opNames

-- | Get the operands for a particular known format.
operandEs :: forall rv fmt . (KnownRepr FormatRepr fmt)
          => SemanticsM (InstExpr fmt) rv (List (InstExpr fmt rv) (OperandTypes fmt))
operandEs = return (operandEsWithRepr knownRepr)

-- | Get the operands for a particular format, where the format is supplied as an
-- explicit argument.
operandEsWithRepr :: FormatRepr fmt -> (List (InstExpr fmt rv) (OperandTypes fmt))
operandEsWithRepr repr = case repr of
  RRepr -> (OperandExpr knownNat (OperandID index0) :<
            OperandExpr knownNat (OperandID index1) :<
            OperandExpr knownNat (OperandID index2) :< Nil)
  IRepr -> (OperandExpr knownNat (OperandID index0) :<
            OperandExpr knownNat (OperandID index1) :<
            OperandExpr knownNat (OperandID index2) :< Nil)
  SRepr -> (OperandExpr knownNat (OperandID index0) :<
            OperandExpr knownNat (OperandID index1) :<
            OperandExpr knownNat (OperandID index2) :< Nil)
  BRepr -> (OperandExpr knownNat (OperandID index0) :<
            OperandExpr knownNat (OperandID index1) :<
            OperandExpr knownNat (OperandID index2) :< Nil)
  URepr -> (OperandExpr knownNat (OperandID index0) :<
            OperandExpr knownNat (OperandID index1) :< Nil)
  JRepr -> (OperandExpr knownNat (OperandID index0) :<
            OperandExpr knownNat (OperandID index1) :< Nil)
  HRepr -> (OperandExpr knownNat (OperandID index0) :<
            OperandExpr knownNat (OperandID index1) :<
            OperandExpr knownNat (OperandID index2) :< Nil)
  PRepr -> Nil
  ARepr -> (OperandExpr knownNat (OperandID index0) :<
            OperandExpr knownNat (OperandID index1) :<
            OperandExpr knownNat (OperandID index2) :<
            OperandExpr knownNat (OperandID index3) :<
            OperandExpr knownNat (OperandID index4) :< Nil)
  R2Repr -> (OperandExpr knownNat (OperandID index0) :<
             OperandExpr knownNat (OperandID index1) :<
             OperandExpr knownNat (OperandID index2) :< Nil)
  R3Repr -> (OperandExpr knownNat (OperandID index0) :<
             OperandExpr knownNat (OperandID index1) :<
             OperandExpr knownNat (OperandID index2) :<
             OperandExpr knownNat (OperandID index3) :< Nil)
  R4Repr -> (OperandExpr knownNat (OperandID index0) :<
             OperandExpr knownNat (OperandID index1) :<
             OperandExpr knownNat (OperandID index2) :<
             OperandExpr knownNat (OperandID index3) :<
             OperandExpr knownNat (OperandID index4) :< Nil)
  RXRepr -> (OperandExpr knownNat (OperandID index0) :<
             OperandExpr knownNat (OperandID index1) :< Nil)
  XRepr -> (OperandExpr knownNat (OperandID index0) :< Nil)
  where index4 = IndexThere index3

-- | Obtain the semantics defined by a 'SemanticsM' action.
getSemantics :: SemanticsM expr rv () -> Semantics expr rv
getSemantics = flip execState emptySemantics . unSemanticsM

-- | Add a comment.
comment :: String -> SemanticsM expr rv ()
comment c = semComments %= \cs -> cs Seq.|> c

-- | Get the width of the instruction word
instBytes :: KnownRV rv => SemanticsM (InstExpr fmt) rv (InstExpr fmt rv (RVWidth rv))
instBytes = return (InstBytes knownNat)

-- | Get the entire instruction word (useful for exceptions)
instWord :: KnownRV rv => SemanticsM (InstExpr fmt) rv (InstExpr fmt rv (RVWidth rv))
instWord = return (InstWord knownNat)

-- | Read the pc.
readPC :: (StateExpr expr, KnownRV rv) => expr rv (RVWidth rv)
readPC = stateExpr (LocApp (PCApp knownNat))

-- | Read a value from a register. Register x0 is hardwired to 0.
readGPR :: (AbbrevExpr expr, BVExpr (expr rv), StateExpr expr, KnownRV rv) => expr rv 5 -> expr rv (RVWidth rv)
readGPR ridE = abbrevExpr (SafeGPRApp knownNat ridE)
  -- iteE (ridE `eqE` litBV 0) (litBV 0) (stateExpr (LocApp (GPRApp knownNat ridE)))

-- | Read a value from a floating point register.
readFPR :: (BVExpr (expr rv), StateExpr expr, FExt << rv, KnownRV rv)
         => expr rv 5
         -> expr rv (RVFloatWidth rv)
readFPR ridE = stateExpr (LocApp (FPRApp knownNat ridE))

-- TODO: We need a wrapper around this to handle access faults.
-- | Read a variable number of bytes from memory, with an explicit width argument.
readMem :: StateExpr expr
        => NatRepr bytes
        -> expr rv (RVWidth rv)
        -> expr rv (8 T.* bytes)
readMem bytes addr = stateExpr (LocApp (MemApp bytes addr))

-- | Read a value from a CSR.
rawReadCSR :: (StateExpr expr, KnownRV rv) => expr rv 12 -> expr rv (RVWidth rv)
rawReadCSR csr = stateExpr (LocApp (CSRApp knownNat csr))

-- | Read the current privilege level.
readPriv :: StateExpr expr => expr rv 2
readPriv = stateExpr (LocApp PrivApp)

-- | Add a statement to the semantics.
addStmt :: Stmt expr rv -> SemanticsM expr rv ()
addStmt stmt = semStmts %= \stmts -> stmts Seq.|> stmt

-- | Add a PC assignment to the semantics.
assignPC :: KnownRV rv => expr rv (RVWidth rv) -> SemanticsM expr rv ()
assignPC pc = addStmt (AssignStmt (PCApp knownNat) pc)

-- | Add a register assignment to the semantics.
assignGPR :: KnownRV rv
          => BVExpr (expr rv)
          => expr rv 5
          -> expr rv (RVWidth rv)
          -> SemanticsM expr rv ()
assignGPR r e = addStmt $ AbbrevStmt (SafeGPRAssign r e)

-- | Add a register assignment to the semantics.
assignFPR :: (KnownRV rv, BVExpr (expr rv), FExt << rv)
           => expr rv 5
           -> expr rv (RVFloatWidth rv)
           -> SemanticsM expr rv ()
assignFPR r e = addStmt (AssignStmt (FPRApp knownNat r) e)

-- TODO: We need a wrapper around this to handle access faults.
-- | Add a memory location assignment to the semantics, with an explicit width argument.
assignMem :: NatRepr bytes
          -> expr rv (RVWidth rv)
          -> expr rv (8 T.* bytes)
          -> SemanticsM expr rv ()
assignMem bytes addr val = addStmt (AssignStmt (MemApp bytes addr) val)

-- | Add a CSR assignment to the semantics.
assignCSR :: KnownRV rv
          => expr rv 12
          -> expr rv (RVWidth rv)
          -> SemanticsM expr rv ()
assignCSR csr val = addStmt (AssignStmt (CSRApp knownNat csr) val)

-- | Add a privilege assignment to the semantics.
assignPriv :: expr rv 2 -> SemanticsM expr rv ()
assignPriv priv = addStmt (AssignStmt PrivApp priv)

-- | Reserve a memory location.
reserve :: (AExt << rv, BVExpr (expr rv)) => expr rv (RVWidth rv) -> SemanticsM expr rv ()
reserve addr = addStmt (AssignStmt (ResApp addr) (litBV 1))

-- | Check that a memory location is reserved.
checkReserved :: (AExt << rv, StateExpr expr) => expr rv (RVWidth rv) -> expr rv 1
checkReserved addr = stateExpr (LocApp (ResApp addr))

-- | Left-associative application (use with 'branch' to avoid parentheses around @do@
-- notation)
($>) :: (a -> b) -> a -> b
($>) = ($)

infixl 1 $>

-- | Add a branch statement to the semantics. Note that comments in the subsemantics
-- will be ignored.
branch :: expr rv 1 -> SemanticsM expr rv () -> SemanticsM expr rv () -> SemanticsM expr rv ()
branch e fbTrue fbFalse = do
  let fTrue  = getSemantics fbTrue  ^. semStmts
      fFalse = getSemantics fbFalse ^. semStmts
  addStmt (BranchStmt e fTrue fFalse)

-- Class instances

instance TestEquality expr => TestEquality (LocApp expr rv) where
  PCApp _ `testEquality` PCApp _ = Just Refl
  GPRApp _ e1 `testEquality` GPRApp _ e2 = case e1 `testEquality` e2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  MemApp b1 e1 `testEquality` MemApp b2 e2 = case (b1 `testEquality` b2, e1 `testEquality` e2) of
    (Just Refl, Just Refl) -> Just Refl
    _ -> Nothing
  ResApp e1 `testEquality` ResApp e2 = case e1 `testEquality` e2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  CSRApp _ e1 `testEquality` CSRApp _ e2 = case e1 `testEquality` e2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  PrivApp `testEquality` PrivApp = Just Refl
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
  InstLitBV bv1 `testEquality` InstLitBV bv2 = bv1 `testEquality` bv2
  OperandExpr _ oid1 `testEquality` OperandExpr _ oid2 = case oid1 `testEquality` oid2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  InstBytes _ `testEquality` InstBytes _ = Just Refl
  InstWord _ `testEquality` InstWord _ = Just Refl
  InstStateApp e1 `testEquality` InstStateApp e2 = case e1 `testEquality` e2 of
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
pPrintLocApp _ _ (PCApp _) = text "pc"
pPrintLocApp ppExpr top (GPRApp _ e) = text "xRaw[" <> ppExpr top e <> text "]" -- this should not happen
pPrintLocApp ppExpr top (FPRApp _ e) = text "f[" <> ppExpr top e <> text "]"
pPrintLocApp ppExpr top (MemApp bytes e) = text "M[" <> ppExpr top e <> text "]_" <> pPrint (intValue bytes)
pPrintLocApp ppExpr top (ResApp e) = text "MReserved[" <> ppExpr top e <> text "]"
pPrintLocApp ppExpr top (CSRApp _ e) = text "CSR[" <> ppExpr top e <> text "]"
pPrintLocApp _ _ PrivApp = text "current_priv"

pPrintAbbrevApp :: (forall w' . Bool -> expr rv w' -> Doc)
                -> Bool
                -> AbbrevApp expr rv w
                -> Doc
pPrintAbbrevApp ppExpr top (SafeGPRApp _ e) = text "x[" <> ppExpr top e <> text "]"

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
pPrintBVApp ppExpr _ (NotApp _ e) = text "!" <> ppExpr False e
pPrintBVApp ppExpr _ (NegateApp _ e) = text "-" <> ppExpr False e
pPrintBVApp ppExpr _ (AbsApp _ e) = text "|" <> ppExpr True e <> text "|"
pPrintBVApp ppExpr _ (SignumApp _ e) = text "signum(" <> ppExpr True e <> text ")"
pPrintBVApp ppExpr _ (ZExtApp _ e) = text "zext(" <> ppExpr True e <> text ")"
pPrintBVApp ppExpr _ (SExtApp _ e) = text "sext(" <> ppExpr True e <> text ")"
pPrintBVApp ppExpr top (ExtractApp w ix e) =
  ppExpr top e <> text "[" <> pPrint ix <> text ":" <>
  pPrint (ix + fromIntegral (natValue w) - 1) <> text "]"
pPrintBVApp ppExpr False e = parens (pPrintBVApp ppExpr True e)
pPrintBVApp ppExpr _ (AndApp _ e1 e2) = ppExpr False e1 <+> text "&" <+> ppExpr False e2
pPrintBVApp ppExpr _ (OrApp _  e1 e2) = ppExpr False e1 <+> text "|" <+> ppExpr False e2
pPrintBVApp ppExpr _ (XorApp _ e1 e2) = ppExpr False e1 <+> text "^" <+> ppExpr False e2
pPrintBVApp ppExpr _ (SllApp _ e1 e2) = ppExpr False e1 <+> text "<<" <+> ppExpr False e2
pPrintBVApp ppExpr _ (SrlApp _ e1 e2) = ppExpr False e1 <+> text ">l>" <+> ppExpr False e2
pPrintBVApp ppExpr _ (SraApp _ e1 e2) = ppExpr False e1 <+> text ">a>" <+> ppExpr False e2
pPrintBVApp ppExpr _ (AddApp _ e1 e2) = ppExpr False e1 <+> text "+" <+> ppExpr False e2
pPrintBVApp ppExpr _ (SubApp _ e1 e2) = ppExpr False e1 <+> text "-" <+> ppExpr False e2
pPrintBVApp ppExpr _ (MulApp _ e1 e2) = ppExpr False e1 <+> text "*" <+> ppExpr False e2
pPrintBVApp ppExpr _ (QuotUApp _ e1 e2) = ppExpr False e1 <+> text "/u" <+> ppExpr False e2
pPrintBVApp ppExpr _ (QuotSApp _ e1 e2) = ppExpr False e1 <+> text "/s" <+> ppExpr False e2
pPrintBVApp ppExpr _ (RemUApp _ e1 e2) = ppExpr False e1 <+> text "%u" <+> ppExpr False e2
pPrintBVApp ppExpr _ (RemSApp _ e1 e2) = ppExpr False e1 <+> text "%s" <+> ppExpr False e2
pPrintBVApp ppExpr _ (EqApp  e1 e2) = ppExpr False e1 <+> text "==" <+> ppExpr False e2
pPrintBVApp ppExpr _ (LtuApp e1 e2) = ppExpr False e1 <+> text "<u" <+> ppExpr False e2
pPrintBVApp ppExpr _ (LtsApp e1 e2) = ppExpr False e1 <+> text "<s" <+> ppExpr False e2
pPrintBVApp ppExpr _ (ConcatApp _ e1 e2) =
  text "{" <> ppExpr True e1 <> text ", " <> ppExpr True e2 <> text "}"
pPrintBVApp ppExpr _ (IteApp _ e1 e2 e3) =
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

pPrintAbbrevStmt :: (forall w' . Bool -> expr rv w' -> Doc)
                 -> AbbrevStmt expr rv
                 -> Doc
pPrintAbbrevStmt ppExpr (SafeGPRAssign ridE e) =
  text "x[" <> ppExpr True ridE <> text "] :=" <+> ppExpr True e
pPrintAbbrevStmt ppExpr (RaiseException code info) =
  text "raiseException(code = " <> pPrint code <> text ", info = " <> ppExpr True info <> text")"

pPrintStmt :: (forall w' . Bool -> expr rv w' -> Doc)
           -> Stmt expr rv
           -> Doc
pPrintStmt ppExpr (AssignStmt le e) = pPrintLocApp ppExpr True le <+> text ":=" <+> ppExpr True e
pPrintStmt ppExpr (AbbrevStmt abbrevStmt) = pPrintAbbrevStmt ppExpr abbrevStmt
pPrintStmt ppExpr (BranchStmt test s1s s2s) =
  text "IF" <+> ppExpr True test
  $$ nest 2 (text "THEN")
  $$ nest 4 (vcat (pPrintStmt ppExpr <$> toList s1s))
  $$ nest 2 (text "ELSE")
  $$ nest 4 (vcat (pPrintStmt ppExpr <$> toList s2s))

pPrintSemantics :: (forall w' . Bool -> expr rv w' -> Doc)
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
pPrintInstExpr opNames _ (OperandExpr _ (OperandID oid)) = pPrintOperandName (opNames !! oid)
pPrintInstExpr _ _ (InstLitBV bv) = pPrint bv
pPrintInstExpr opNames top (InstAbbrevApp abbrevApp) = pPrintAbbrevApp (pPrintInstExpr opNames) top abbrevApp
pPrintInstExpr _ _ (InstBytes _) = text "step"
pPrintInstExpr _ _ (InstWord _) = text "inst"
pPrintInstExpr opNames top (InstStateApp e) = pPrintStateApp (pPrintInstExpr opNames) top e

pPrintInstSemantics :: InstSemantics rv fmt -> Doc
pPrintInstSemantics (InstSemantics semantics opNames) =
  pPrintSemantics (pPrintInstExpr opNames) semantics
