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
License     : None (yet)
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

Finally, we can combine 'Stmt's to create 'Formula's, which are simply sequences of
statements. We export a monad, 'FormulaBuilder,' to facilitate straightforward
definitions of these assignments, and a number of functions ('assignPC', 'assignReg',
etc.) that can be used within this monad. To see examples of its use, take a look at
RISCV.Extensions.Base, which contains the base RISC-V ISA instruction definitions,
defined using 'FormulaBuilder'.

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
  , readMem
  , readCSR
  , readPriv
  , checkReserved
    -- * RISC-V semantic statements and formulas
  , Stmt(..)
  , Formula, fComments, fDefs
  , InstFormula(..)
    -- * FormulaBuilder monad
  , FormulaBuilder
  , getFormula
    -- * FormulaBuilder operations
    -- ** Auxiliary
  , comment
  , operandEs, operandEsWithRepr
  , instBytes
  , instWord
  , litBV
    -- ** State actions
  , assignPC
  , assignReg
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
import Text.PrettyPrint.HughesPJClass

import Data.BitVector.Sized.App
import RISCV.Types

----------------------------------------
-- Expressions, statements, and formulas

-- | This type represents an abstract component of the global state. Sub-expressions
-- come from an arbitrary expression language @expr@.
data LocExpr expr arch w where
  PCExpr   :: LocExpr expr arch (ArchWidth arch)
  RegExpr  :: expr 5 -> LocExpr expr arch (ArchWidth arch)
  MemExpr  :: NatRepr bytes -> expr (ArchWidth arch) -> LocExpr expr arch (8*bytes)
  ResExpr  :: expr (ArchWidth arch) -> LocExpr expr arch 1
  CSRExpr  :: expr 12 -> LocExpr expr arch (ArchWidth arch)
  PrivExpr :: LocExpr expr arch 2

-- | Expressions for general computations over the RISC-V machine state -- we can
-- access specific locations, and we can also build up compound expressions using the
-- 'BVApp' expression language. Sub-expressions come from an arbitrary expression
-- language @expr@.
data StateExpr expr arch w where
  -- Accessing state
  LocExpr :: !(LocExpr expr arch w) -> StateExpr expr arch w

  -- BVApp with Expr subexpressions
  AppExpr :: !(BVApp expr w) -> StateExpr expr arch w

-- | Expressions built purely from 'StateExpr's, which are executed outside the
-- context of an executing instruction (for instance, during exception handling).
newtype PureStateExpr arch w = PureStateExpr (StateExpr (PureStateExpr arch) arch w)

instance BVExpr (PureStateExpr arch) where
  appExpr = PureStateExpr . AppExpr

-- | Expressions for computations over the RISC-V machine state, in the context of
-- an executing instruction.
data InstExpr fmt arch w where
  -- | Accessing the instruction operands
  OperandExpr :: !(OperandID fmt w) -> InstExpr fmt arch w
  -- | Accessing the instruction width, in number of bytes
  InstBytes :: InstExpr fmt arch (ArchWidth arch)
  -- | Accessing the entire instruction word itself
  InstWord :: InstExpr fmt arch (ArchWidth arch)

  -- | Accessing the machine state
  InstStateExpr :: !(StateExpr (InstExpr fmt arch) arch w) -> InstExpr fmt arch w

instance BVExpr (InstExpr fmt arch) where
  appExpr = InstStateExpr . AppExpr

-- TODO: When we get quantified constraints, put a forall arch. BVExpr (expr arch)
-- here
-- | A type class for expression languages that can refer to arbitrary pieces of
-- RISC-V machine state.
class RVStateExpr (expr :: BaseArch -> Nat -> *) where
  stateExpr :: StateExpr (expr arch) arch w -> expr arch w

instance RVStateExpr PureStateExpr where
  stateExpr = PureStateExpr

instance RVStateExpr (InstExpr fmt) where
  stateExpr = InstStateExpr

-- | A 'Stmt' represents an atomic state transformation -- typically, an assignment
-- of a state component (register, memory location, etc.) to an expression of the
-- appropriate width.
data Stmt (expr :: Nat -> *) (arch :: BaseArch) where
  -- | Assign a piece of state to a value.
  AssignStmt :: !(LocExpr expr arch w) -> !(expr w) -> Stmt expr arch
  -- | If-then-else branch statement.
  BranchStmt :: !(expr 1)
             -> !(Seq (Stmt expr arch))
             -> !(Seq (Stmt expr arch))
             -> Stmt expr arch

-- | A 'Formula' is simply a set of simultaneous 'Stmt's.
data Formula expr arch
  = Formula { _fComments :: !(Seq String)
              -- ^ multiline comment
            , _fDefs    :: !(Seq (Stmt expr arch))
              -- ^ sequence of statements defining the formula
            }

-- | A wrapper for 'Formula's over 'InstExpr's with the 'Format' type parameter
-- appearing last, for the purposes of associating with an corresponding 'Opcode' of
-- the same format.
newtype InstFormula arch fmt = InstFormula { getInstFormula :: Formula (InstExpr fmt arch) arch }

-- | Lens for 'Formula' comments.
fComments :: Simple Lens (Formula expr arch) (Seq String)
fComments = lens _fComments (\(Formula _ d) c -> Formula c d)

-- | Lens for 'Formula' statements.
fDefs :: Simple Lens (Formula expr arch) (Seq (Stmt expr arch))
fDefs = lens _fDefs (\(Formula c _) d -> Formula c d)

-- | Every definition begins with the empty formula.
emptyFormula :: Formula expr arch
emptyFormula = Formula Seq.empty Seq.empty

-- | State monad for defining semantics over the RISC-V machine state. We allow the
-- expression language to vary, because sometimes we are in the context of an
-- executing instruction, and sometimes we are not (for instance, when we are
-- handling an exception).
newtype FormulaBuilder expr arch a =
  FormulaBuilder { unFormulaBuilder :: State (Formula expr arch) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadState (Formula expr arch))

-- | Get the operands for a particular known format
operandEs :: forall arch fmt . (KnownRepr FormatRepr fmt)
          => FormulaBuilder (InstExpr fmt arch) arch (List (InstExpr fmt arch) (OperandTypes fmt))
operandEs = return (operandEsWithRepr knownRepr)

operandEsWithRepr :: FormatRepr fmt
                  -> (List (InstExpr fmt arch) (OperandTypes fmt))
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
  XRepr -> (OperandExpr (OperandID index0) :< Nil)
  where index4 = IndexThere index3

-- | Obtain the formula defined by a 'FormulaBuilder' action.
getFormula :: FormulaBuilder expr arch () -> Formula expr arch
getFormula = flip execState emptyFormula . unFormulaBuilder

-- | Add a comment.
comment :: String -> FormulaBuilder expr arch ()
comment c = fComments %= \cs -> cs Seq.|> c

-- | Get the width of the instruction word
instBytes :: FormulaBuilder (InstExpr fmt arch) arch (InstExpr fmt arch (ArchWidth arch))
instBytes = return InstBytes

-- | Get the entire instruction word (useful for exceptions)
instWord :: FormulaBuilder (InstExpr fmt arch) arch (InstExpr fmt arch (ArchWidth arch))
instWord = return InstWord

-- | Read the pc.
readPC :: RVStateExpr expr => expr arch (ArchWidth arch)
readPC = stateExpr (LocExpr PCExpr)

-- | Read a value from a register. Register x0 is hardwired to 0.
readReg :: (BVExpr (expr arch), RVStateExpr expr, KnownArch arch) => expr arch 5 -> expr arch (ArchWidth arch)
readReg ridE = iteE (ridE `eqE` litBV 0) (litBV 0) (stateExpr (LocExpr (RegExpr ridE)))

-- | Read a variable number of bytes from memory, with an explicit width argument.
readMem :: RVStateExpr expr
        => NatRepr bytes
        -> expr arch (ArchWidth arch)
        -> expr arch (8*bytes)
readMem bytes addr = stateExpr (LocExpr (MemExpr bytes addr))

-- | Read a value from a CSR.
readCSR :: (RVStateExpr expr, KnownArch arch) => expr arch 12 -> expr arch (ArchWidth arch)
readCSR csr = stateExpr (LocExpr (CSRExpr csr))

-- | Read the current privilege level.
readPriv :: RVStateExpr expr => expr arch 2
readPriv = stateExpr (LocExpr PrivExpr)

-- | Add a statement to the formula.
addStmt :: Stmt expr arch -> FormulaBuilder expr arch ()
addStmt stmt = fDefs %= \stmts -> stmts Seq.|> stmt

-- | Add a PC assignment to the formula.
assignPC :: expr arch (ArchWidth arch) -> FormulaBuilder (expr arch) arch ()
assignPC pc = addStmt (AssignStmt PCExpr pc)

-- | Add a register assignment to the formula.
assignReg :: BVExpr (expr arch)
          => expr arch 5
          -> expr arch (ArchWidth arch)
          -> FormulaBuilder (expr arch) arch ()
assignReg r e = addStmt $
  BranchStmt (r `eqE` litBV 0)
  $> Seq.empty
  $> Seq.singleton (AssignStmt (RegExpr r) e)

-- | Add a memory location assignment to the formula, with an explicit width argument.
assignMem :: NatRepr bytes
          -> expr arch (ArchWidth arch)
          -> expr arch (8*bytes)
          -> FormulaBuilder (expr arch) arch ()
assignMem bytes addr val = addStmt (AssignStmt (MemExpr bytes addr) val)

-- | Add a CSR assignment to the formula.
assignCSR :: expr arch 12
          -> expr arch (ArchWidth arch)
          -> FormulaBuilder (expr arch) arch ()
assignCSR csr val = addStmt (AssignStmt (CSRExpr csr) val)

-- | Add a privilege assignment to the formula.
assignPriv :: expr arch 2 -> FormulaBuilder (expr arch) arch ()
assignPriv priv = addStmt (AssignStmt PrivExpr priv)

-- | Reserve a memory location.
reserve :: BVExpr (expr arch) => expr arch (ArchWidth arch) -> FormulaBuilder (expr arch) arch ()
reserve addr = addStmt (AssignStmt (ResExpr addr) (litBV 1))

-- | Check that a memory location is reserved.
checkReserved :: RVStateExpr expr => expr arch (ArchWidth arch) -> expr arch 1
checkReserved addr = stateExpr (LocExpr (ResExpr addr))

-- | Left-associative application (use with 'branch' to avoid parentheses around @do@
-- notation)
($>) :: (a -> b) -> a -> b
($>) = ($)

infixl 1 $>

-- | Add a branch statement to the formula. Note that comments in the subformulas
-- will be ignored.
branch :: expr arch 1
       -> FormulaBuilder (expr arch) arch ()
       -> FormulaBuilder (expr arch) arch ()
       -> FormulaBuilder (expr arch) arch ()
branch e fbTrue fbFalse = do
  let fTrue  = getFormula fbTrue  ^. fDefs
      fFalse = getFormula fbFalse ^. fDefs
  addStmt (BranchStmt e fTrue fFalse)

-- Class instances

instance TestEquality expr => TestEquality (LocExpr expr arch) where
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

instance TestEquality expr => TestEquality (StateExpr expr arch) where
  LocExpr e1 `testEquality` LocExpr e2 = case e1 `testEquality` e2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  AppExpr e1 `testEquality` AppExpr e2 = case e1 `testEquality` e2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  _ `testEquality` _ = Nothing

instance TestEquality (InstExpr fmt arch) where
  OperandExpr oid1 `testEquality` OperandExpr oid2 = case oid1 `testEquality` oid2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  InstBytes `testEquality` InstBytes = Just Refl
  InstWord `testEquality` InstWord = Just Refl
  InstStateExpr e1 `testEquality` InstStateExpr e2 = case e1 `testEquality` e2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  _ `testEquality` _ = Nothing

instance Eq (InstExpr fmt arch w) where
  x == y = isJust (testEquality x y)

instance Pretty (LocExpr (InstExpr fmt arch) arch w) where
  pPrint PCExpr      = text "pc"
  pPrint (RegExpr e) = text "x[" <> pPrint e <> text "]"
  pPrint (MemExpr bytes e) = text "M[" <> pPrint e <> text "]_" <> pPrint (natValue bytes)
  pPrint (ResExpr e) = text "MReserved[" <> pPrint e <> text "]"
  pPrint (CSRExpr e) = text "CSR[" <> pPrint e <> text "]"
  pPrint PrivExpr    = text "current_priv"

instance Pretty (InstExpr fmt arch w) where
  pPrint = pPrintInstExpr' True

instance Pretty (Stmt (InstExpr fmt arch) arch) where
  pPrint (AssignStmt le e) = pPrint le <+> text ":=" <+> pPrint e
  pPrint (BranchStmt test s1s s2s) =
    text "IF" <+> pPrint test
    $$ nest 2 (text "THEN")
    $$ nest 4 (vcat (pPrint <$> toList s1s))
    $$ nest 2 (text "ELSE")
    $$ nest 4 (vcat (pPrint <$> toList s2s))

instance Pretty (Formula (InstExpr fmt arch) arch) where
  pPrint formula = vcat (pPrint <$> toList (formula ^. fDefs))

pPrintStateExpr' :: Bool -> StateExpr (InstExpr fmt arch) arch w -> Doc
pPrintStateExpr' _ (LocExpr loc) = pPrint loc
pPrintStateExpr' top (AppExpr app) = pPrintApp' top app

pPrintInstExpr' :: Bool -> InstExpr fmt arch w -> Doc
pPrintInstExpr' _ (OperandExpr (OperandID oid)) = text "arg" <> pPrint (indexValue oid)
pPrintInstExpr' _ InstBytes = text "step"
pPrintInstExpr' _ InstWord = text "inst"
pPrintInstExpr' top (InstStateExpr e) = pPrintStateExpr' top e

pPrintApp' :: Bool -> BVApp (InstExpr fmt arch) w -> Doc
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
