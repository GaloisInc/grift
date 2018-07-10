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

Several types and functions enabling specification of semantics for RISC-V
instructions.
-}

module RISCV.Semantics
  ( -- * Types for semantic formulas
    LocExpr(..)
  , StateExpr(..)
  , RVStateExpr(..)
  , PureStateExpr(..)
  , InstExpr(..)
  , Stmt(..)
  , Formula, fComments, fDefs
  , InstFormula(..)
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

-- | This type represents an abstract component of the global state.
data LocExpr expr arch w where
  PCExpr   ::                                   LocExpr expr arch (ArchWidth arch)
  RegExpr  :: expr 5                         -> LocExpr expr arch (ArchWidth arch)
  MemExpr  :: NatRepr bytes -> expr (ArchWidth arch) -> LocExpr expr arch (8*bytes)
  ResExpr  :: expr (ArchWidth arch)          -> LocExpr expr arch 1
  CSRExpr  :: expr 12                        -> LocExpr expr arch (ArchWidth arch)
  PrivExpr ::                                   LocExpr expr arch 2

-- | Expressions for general computations over the RISC-V machine state.
data StateExpr (expr :: Nat -> *) arch w where
  -- Accessing state
  LocExpr :: !(LocExpr expr arch w) -> StateExpr expr arch w

  -- BVApp with Expr subexpressions
  AppExpr :: !(BVApp expr w) -> StateExpr expr arch w

-- TODO: When we get quantified constraints, put a forall arch. BVExpr (expr arch) here
class RVStateExpr (expr :: BaseArch -> Nat -> *) where
  stateExpr :: StateExpr (expr arch) arch w -> expr arch w

newtype PureStateExpr arch w = PureStateExpr (StateExpr (PureStateExpr arch) arch w)

instance BVExpr (PureStateExpr arch) where
  appExpr = PureStateExpr . AppExpr

instance RVStateExpr PureStateExpr where
  stateExpr = PureStateExpr

-- | Expressions for computations over the RISC-V machine state, in the context of
-- executing an instruction.
data InstExpr (fmt :: Format) (arch :: BaseArch) (w :: Nat) where
  -- Accessing the instruction
  OperandExpr :: !(OperandID fmt w) -> InstExpr fmt arch w
  InstBytes :: InstExpr fmt arch (ArchWidth arch)

  -- Accessing the machine state
  StateExpr :: !(StateExpr (InstExpr fmt arch) arch w) -> InstExpr fmt arch w

instance BVExpr (InstExpr fmt arch) where
  appExpr = StateExpr . AppExpr

instance RVStateExpr (InstExpr fmt) where
  stateExpr = StateExpr

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

data Formula expr arch
  = Formula { _fComments :: !(Seq String)
              -- ^ multiline comment
            , _fDefs    :: !(Seq (Stmt expr arch))
              -- ^ sequence of statements defining the formula
            }

data InstFormula arch fmt = InstFormula { getInstFormula :: Formula (InstExpr fmt arch) arch }

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
getFormula :: FormulaBuilder expr arch () -> Formula expr arch
getFormula = flip execState emptyFormula . unFormulaBuilder

-- | Add a comment.
comment :: String -> FormulaBuilder expr arch ()
comment c = fComments %= \cs -> cs Seq.|> c

-- | Get the width of the instruction word
instBytes :: FormulaBuilder (InstExpr fmt arch) arch (InstExpr fmt arch (ArchWidth arch))
instBytes = return InstBytes

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

instance TestEquality expr =>  TestEquality (StateExpr expr arch) where
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
  StateExpr e1 `testEquality` StateExpr e2 = case e1 `testEquality` e2 of
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
pPrintInstExpr' top (StateExpr e) = pPrintStateExpr' top e

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
