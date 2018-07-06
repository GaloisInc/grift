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

Several types and functions enabling specification of semantics for RISC-V
instructions.
-}

module RISCV.Semantics
  ( -- * Types for semantic formulas
    LocExpr(..)
  , StateExpr(..)
  , InstExpr(..)
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
  , checkReserved
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
import Data.Parameterized.TH.GADT
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import GHC.TypeLits
import Text.PrettyPrint.HughesPJClass

import Data.BitVector.Sized.App
import RISCV.Types

----------------------------------------
-- Expressions, statements, and formulas

-- | This type represents an abstract component of the global state, and should be
-- used both for building expressions in our expression language and for interpreting
-- those expressions.
data LocExpr expr arch w where
  PCExpr   ::                                   LocExpr expr arch (ArchWidth arch)
  RegExpr  :: expr 5                         -> LocExpr expr arch (ArchWidth arch)
  MemExpr  :: NatRepr bytes -> expr (ArchWidth arch) -> LocExpr expr arch (8*bytes)
  ResExpr  :: expr (ArchWidth arch)          -> LocExpr expr arch 1
  CSRExpr  :: expr 12                        -> LocExpr expr arch (ArchWidth arch)
  PrivExpr ::                                   LocExpr expr arch 2

instance Pretty (LocExpr (InstExpr arch fmt) arch w) where
  pPrint PCExpr      = text "pc"
  pPrint (RegExpr e) = text "x[" <> pPrint e <> text "]"
  pPrint (MemExpr bytes e) = text "M[" <> pPrint e <> text "]_" <> pPrint (natValue bytes)
  pPrint (ResExpr e) = text "MReserved[" <> pPrint e <> text "]"
  pPrint (CSRExpr e) = text "CSR[" <> pPrint e <> text "]"
  pPrint PrivExpr    = text "current_priv"

-- | Expressions for general computations over the RISC-V machine state.
data StateExpr (expr :: Nat -> *) arch w where
  -- Accessing state
  LocExpr :: LocExpr expr arch w -> StateExpr expr arch w

  -- BVApp with Expr subexpressions
  AppExpr :: !(BVApp expr w) -> StateExpr expr arch w

instance Pretty (StateExpr (InstExpr arch fmt) arch w) where

-- | Expressions for computations over the RISC-V machine state, in the context of
-- executing an instruction.
data InstExpr (arch :: BaseArch) (fmt :: Format) (w :: Nat) where
  -- Accessing the instruction
  OperandExpr :: !(OperandID fmt w) -> InstExpr arch fmt w
  InstBytes :: InstExpr arch fmt (ArchWidth arch)

  -- Accessing the machine state
  StateExpr :: !(StateExpr (InstExpr arch fmt) arch w) -> InstExpr arch fmt w

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

instance TestEquality expr =>  TestEquality (StateExpr expr arch) where
  LocExpr e1 `testEquality` LocExpr e2 = case e1 `testEquality` e2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  AppExpr e1 `testEquality` AppExpr e2 = case e1 `testEquality` e2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  _ `testEquality` _ = Nothing

instance TestEquality (InstExpr arch fmt) where
  OperandExpr oid1 `testEquality` OperandExpr oid2 = case oid1 `testEquality` oid2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  InstBytes `testEquality` InstBytes = Just Refl
  StateExpr e1 `testEquality` StateExpr e2 = case e1 `testEquality` e2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  _ `testEquality` _ = Nothing

instance Eq (InstExpr arch fmt w) where
  x == y = isJust (testEquality x y)

instance Pretty (InstExpr arch fmt w) where
  pPrint = pPrintInstExpr' True

pPrintStateExpr' :: Bool -> StateExpr (InstExpr arch fmt) arch w -> Doc
pPrintStateExpr' _ (LocExpr loc) = pPrint loc
pPrintStateExpr' top (AppExpr app) = pPrintApp' top app

pPrintInstExpr' :: Bool -> InstExpr arch fmt w -> Doc
pPrintInstExpr' _ (OperandExpr (OperandID oid)) = text "arg" <> pPrint (indexValue oid)
pPrintInstExpr' _ InstBytes = text "step"
pPrintInstExpr' top (StateExpr e) = pPrintStateExpr' top e

pPrintApp' :: Bool -> BVApp (InstExpr arch fmt) w -> Doc
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

-- | A 'Stmt' represents an atomic state transformation -- typically, an assignment
-- of a state component (register, memory location, etc.) to a 'InstExpr' of the
-- appropriate width.
data Stmt (arch :: BaseArch) (fmt :: Format) where
  -- | Assign a piece of state to a value.
  AssignStmt :: !(LocExpr (InstExpr arch fmt) arch w) -> !(InstExpr arch fmt w) -> Stmt arch fmt
  -- | If-then-else branch statement.
  BranchStmt :: !(InstExpr arch fmt 1)
             -> !(Seq (Stmt arch fmt))
             -> !(Seq (Stmt arch fmt))
             -> Stmt arch fmt

instance Pretty (Stmt arch fmt) where
  pPrint (AssignStmt le e) = pPrint le <+> text ":=" <+> pPrint e
  pPrint (BranchStmt test s1s s2s) =
    text "IF" <+> pPrint test
    $$ nest 2 (text "THEN")
    $$ nest 4 (vcat (pPrint <$> toList s1s))
    $$ nest 2 (text "ELSE")
    $$ nest 4 (vcat (pPrint <$> toList s2s))

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

instance Pretty (Formula arch fmt) where
  pPrint formula = vcat (pPrint <$> toList (formula ^. fDefs))

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

instance BVExpr (InstExpr arch fmt) where
  appExpr = StateExpr . AppExpr

-- | Get the operands for a particular known format
operandEs :: forall arch fmt . (KnownRepr FormatRepr fmt)
          => FormulaBuilder arch fmt (List (InstExpr arch fmt) (OperandTypes fmt))
operandEs = case knownRepr :: FormatRepr fmt of
  RRepr -> return (OperandExpr (OperandID index0) :<
                   OperandExpr (OperandID index1) :<
                   OperandExpr (OperandID index2) :< Nil)
  IRepr -> return (OperandExpr (OperandID index0) :<
                   OperandExpr (OperandID index1) :<
                   OperandExpr (OperandID index2) :< Nil)
  SRepr -> return (OperandExpr (OperandID index0) :<
                   OperandExpr (OperandID index1) :<
                   OperandExpr (OperandID index2) :< Nil)
  BRepr -> return (OperandExpr (OperandID index0) :<
                   OperandExpr (OperandID index1) :<
                   OperandExpr (OperandID index2) :< Nil)
  URepr -> return (OperandExpr (OperandID index0) :<
                   OperandExpr (OperandID index1) :< Nil)
  JRepr -> return (OperandExpr (OperandID index0) :<
                   OperandExpr (OperandID index1) :< Nil)
  ARepr -> return (OperandExpr (OperandID index0) :<
                   OperandExpr (OperandID index1) :<
                   OperandExpr (OperandID index2) :<
                   OperandExpr (OperandID index3) :<
                   OperandExpr (OperandID index4) :< Nil)
  XRepr -> return (OperandExpr (OperandID index0) :< Nil)
  where index4 = IndexThere index3

-- | Obtain the formula defined by a 'FormulaBuilder' action.
getFormula :: FormulaBuilder arch fmt () -> Formula arch fmt
getFormula = flip execState emptyFormula . unFormulaBuilder

-- | Add a comment.
comment :: String -> FormulaBuilder arch fmt ()
comment c = fComments %= \cs -> cs Seq.|> c

-- | Get the width of the instruction word
instBytes :: FormulaBuilder arch fmt (InstExpr arch fmt (ArchWidth arch))
instBytes = return InstBytes

-- | Read the pc.
readPC :: FormulaBuilder arch fmt (InstExpr arch fmt (ArchWidth arch))
readPC = return (StateExpr (LocExpr PCExpr))

-- | Read a value from a register. Register x0 is hardwired to 0.
readReg :: KnownArch arch
        => InstExpr arch fmt 5
        -> FormulaBuilder arch fmt (InstExpr arch fmt (ArchWidth arch))
readReg ridE = return $ iteE (ridE `eqE` litBV 0) (litBV 0) (StateExpr (LocExpr (RegExpr ridE)))

-- | Read a variable number of bytes from memory, with an explicit width argument.
readMem :: NatRepr bytes
                -> InstExpr arch fmt (ArchWidth arch)
                -> FormulaBuilder arch fmt (InstExpr arch fmt (8*bytes))
readMem bytes addr = return (StateExpr (LocExpr (MemExpr bytes addr)))

-- | Read a value from a CSR.
readCSR :: KnownArch arch
        => InstExpr arch fmt 12
        -> FormulaBuilder arch fmt (InstExpr arch fmt (ArchWidth arch))
readCSR csr = return (StateExpr (LocExpr (CSRExpr csr)))

-- | Read the current privilege level.
readPriv :: FormulaBuilder arch fmt (InstExpr arch fmt 2)
readPriv = return (StateExpr (LocExpr PrivExpr))

-- | Add a statement to the formula.
addStmt :: Stmt arch fmt -> FormulaBuilder arch fmt ()
addStmt stmt = fDefs %= \stmts -> stmts Seq.|> stmt

-- | Add a PC assignment to the formula.
assignPC :: InstExpr arch fmt (ArchWidth arch) -> FormulaBuilder arch fmt ()
assignPC pc = addStmt (AssignStmt PCExpr pc)

-- | Add a register assignment to the formula.
assignReg :: InstExpr arch fmt 5
          -> InstExpr arch fmt (ArchWidth arch)
          -> FormulaBuilder arch fmt ()
assignReg r e = addStmt $
  BranchStmt (r `eqE` litBV 0)
  $> Seq.empty
  $> Seq.singleton (AssignStmt (RegExpr r) e)

-- | Add a memory location assignment to the formula, with an explicit width argument.
assignMem :: NatRepr bytes
          -> InstExpr arch fmt (ArchWidth arch)
          -> InstExpr arch fmt (8*bytes)
          -> FormulaBuilder arch fmt ()
assignMem bytes addr val = addStmt (AssignStmt (MemExpr bytes addr) val)

-- | Add a CSR assignment to the formula.
assignCSR :: InstExpr arch fmt 12
          -> InstExpr arch fmt (ArchWidth arch)
          -> FormulaBuilder arch fmt ()
assignCSR csr val = addStmt (AssignStmt (CSRExpr csr) val)

-- | Add a privilege assignment to the formula.
assignPriv :: InstExpr arch fmt 2 -> FormulaBuilder arch fmt ()
assignPriv priv = addStmt (AssignStmt PrivExpr priv)

-- | Reserve a memory location.
reserve :: InstExpr arch fmt (ArchWidth arch) -> FormulaBuilder arch fmt ()
reserve addr = addStmt (AssignStmt (ResExpr addr) (litBV 1))

-- | Check that a memory location is reserved.
checkReserved :: InstExpr arch fmt (ArchWidth arch) -> FormulaBuilder arch fmt (InstExpr arch fmt 1)
checkReserved addr = return (StateExpr (LocExpr (ResExpr addr)))

-- | Left-associative application (use with 'branch' to avoid parentheses around @do@
-- notation)
($>) :: (a -> b) -> a -> b
($>) = ($)

infixl 1 $>

-- | Add a branch statement to the formula. Note that comments in the subformulas
-- will be ignored.
branch :: InstExpr arch fmt 1
       -> FormulaBuilder arch fmt ()
       -> FormulaBuilder arch fmt ()
       -> FormulaBuilder arch fmt ()
branch e fbTrue fbFalse = do
  let fTrue  = getFormula fbTrue  ^. fDefs
      fFalse = getFormula fbFalse ^. fDefs
  addStmt (BranchStmt e fTrue fFalse)
