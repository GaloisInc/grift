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

-- TODO: get pretty printing into a separate entity rather than using Show.
-- TODO: Pretty printing needs to parenthesize expressions appropriately.
-- TODO: It might make sense to remove arch as a type parameter to Formula and
-- Semantics, and add a BVExpr constructor, XLen, which queries the environment for
-- the register width.
-- TODO: It might make sense to parameterize BVExpr and Stmt over a Format, since
-- Formula is as well. If we do this, we might want to change how we're dealing with
-- OperandParams.
-- TODO: This is a variable-width ISA, but we have separated the semantics out from
-- the decoding so cleanly that we actually don't know how big the instruction word
-- is here, and therefore we don't know how much to increment the PC by after most
-- instructions. We could either handle that externally (yuck), or we could include
-- an additional field in each instruction. Then the params function could provide
-- that information along with the operands. Something to think about.
module RISCV.Semantics
  ( -- * Types
    OperandType(..)
  , type RegID, type Imm12, type Imm20, type Imm32
  , OperandID(..)
  , OperandIDWidth
  , OperandParam(..)
  , BVExpr(..)
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
  , xlen
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
import Data.Parameterized.TH.GADT
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import GHC.TypeLits

import RISCV.Instruction
import RISCV.Types

-- | Operand identifiers.
-- data OperandType = Rd | Rs1 | Rs2 | Imm12 | Imm20 | Imm32
data OperandType = RegID | Imm12 | Imm20 | Imm32

type RegID = 'RegID
type Imm12 = 'Imm12
type Imm20 = 'Imm20
type Imm32 = 'Imm32

-- | Type-level representative for 'OperandID'.
data OperandID :: Format -> OperandType -> * where
  RRd    :: OperandID R RegID
  RRs1   :: OperandID R RegID
  RRs2   :: OperandID R RegID

  IRd    :: OperandID I RegID
  IRs1   :: OperandID I RegID
  IImm12 :: OperandID I Imm12

  SRs1   :: OperandID S RegID
  SRs2   :: OperandID S RegID
  SImm12 :: OperandID S Imm12

  BRs1   :: OperandID B RegID
  BRs2   :: OperandID B RegID
  BImm12 :: OperandID B Imm12

  URd    :: OperandID U RegID
  UImm20 :: OperandID U Imm20

  JRd    :: OperandID J RegID
  JImm20 :: OperandID J Imm20

  XImm32 :: OperandID X Imm32

-- | Maps an 'OperandID' to its length as a 'BitVector'.
type family OperandIDWidth (ot :: OperandType) :: Nat where
  OperandIDWidth RegID = 5
  OperandIDWidth Imm12 = 12
  OperandIDWidth Imm20 = 20
  OperandIDWidth Imm32 = 32

-- Instances
$(return [])
deriving instance Show (OperandID fmt ot)

instance ShowF (OperandID fmt)
deriving instance Eq (OperandID fmt ot)
instance EqF (OperandID fmt) where
  eqF = (==)
instance TestEquality (OperandID fmt) where
  testEquality = $(structuralTypeEquality [t|OperandID|] [])
instance OrdF (OperandID fmt) where
  compareF = $(structuralTypeOrd [t|OperandID|] [])

----------------------------------------
-- Expressions, statements, and formulas

-- | Formula parameter (represents unknown operands)
data OperandParam (arch :: BaseArch) (fmt :: Format) (oid :: OperandType)
  = OperandParam (OperandID fmt oid)

deriving instance Show (OperandParam arch fmt oid)
instance ShowF (OperandParam arch fmt)

-- TODO: Parameterize by fmt?
-- | BitVector expressions. These are the building blocks for semantic formulas for
-- an instruction.
data BVExpr (arch :: BaseArch) (fmt :: Format) (w :: Nat) where
  -- Basic constructors
  LitBV :: BitVector w -> BVExpr arch fmt w
  ParamBV :: OperandParam arch fmt oid -> BVExpr arch fmt (OperandIDWidth oid)
  InstBytes :: BVExpr arch fmt (ArchWidth arch)

  -- Accessing state
  PCRead  :: BVExpr arch fmt (ArchWidth arch)
  RegRead :: BVExpr arch fmt 5 -> BVExpr arch fmt (ArchWidth arch)
  MemRead :: BVExpr arch fmt (ArchWidth arch)
          -> BVExpr arch fmt 8
  -- | This is temporary; when we have CSRs this will change.
  XLen :: BVExpr arch fmt (ArchWidth arch)

  -- Bitwise operations
  AndE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt w
  OrE  :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt w
  XorE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt w
  NotE :: BVExpr arch fmt w -> BVExpr arch fmt w

  -- Arithmetic operations
  AddE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt w
  SubE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt w
  MulSE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt (w+w)
  MulUE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt (w+w)
  MulSUE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt (w+w)
  DivUE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt w
  DivSE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt w
  RemUE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt w
  RemSE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt w
  SllE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt w
  SrlE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt w
  SraE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt w

  -- Comparisons
  EqE  :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt 1
  LtuE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt 1
  LtsE :: BVExpr arch fmt w -> BVExpr arch fmt w -> BVExpr arch fmt 1

  -- Width-changing
  ZExtE :: NatRepr w' -> BVExpr arch fmt w -> BVExpr arch fmt w'
  SExtE :: NatRepr w' -> BVExpr arch fmt w -> BVExpr arch fmt w'
  -- TODO: Extraction should take a BVExpr arch fmt w' rather than an actual Int.
  ExtractE :: NatRepr w' -> Int -> BVExpr arch fmt w -> BVExpr arch fmt w'
  ConcatE :: BVExpr arch fmt w -> BVExpr arch fmt w' -> BVExpr arch fmt (w+w')

  -- Other operations
  IteE :: BVExpr arch fmt 1
       -> BVExpr arch fmt w
       -> BVExpr arch fmt w
       -> BVExpr arch fmt w

-- TODO: Fix this horrible pretty printing.
deriving instance Show (BVExpr arch fmt w)

-- | Runtime exception.
data Exception = EnvironmentCall
               | Breakpoint
               | IllegalInstruction
               | MemoryAccessError
  deriving (Show)

-- | A 'Stmt' represents an atomic state transformation -- typically, an assignment
-- of a state component (register, memory location, etc.) to a 'BVExpr' of the
-- appropriate width.
data Stmt (arch :: BaseArch) (fmt :: Format) where
  AssignReg :: BVExpr arch fmt 5 -> BVExpr arch fmt (ArchWidth arch) -> Stmt arch fmt
  AssignMem :: BVExpr arch fmt (ArchWidth arch)
            -> BVExpr arch fmt 8
            -> Stmt arch fmt
  AssignPC  :: BVExpr arch fmt (ArchWidth arch) -> Stmt arch fmt
  RaiseException :: BVExpr arch fmt 1 -> Exception -> Stmt arch fmt

instance Show (Stmt arch fmt) where
  show (AssignReg r e) = "x[" ++ show r ++ "]' := " ++ show e
  show (AssignMem addr e) = "M[" ++ show addr ++ "]' := " ++ show e
  show (AssignPC pc) = "pc' := " ++ show pc
  show (RaiseException cond e) = "if (" ++ show cond ++ ") then RaiseException(" ++ show e ++ ")"

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

instance Show (Formula arch fmt) where
  show (Formula comments defs) =
    showComments ++
    showDefs
    where showComments = concat (toList ((++ "\n") <$> comments))
          showDefs = concat (toList ((\d -> "  " ++ show d ++ "\n") <$> defs))
instance ShowF (Formula arch)

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

-- | Obtain the formula defined by a 'FormulaBuilder' action.
getFormula :: FormulaBuilder arch fmt () -> Formula arch fmt
getFormula = flip execState emptyFormula . unFormulaBuilder

-- | Add a comment.
comment :: String -> FormulaBuilder arch fmt ()
comment c = fComments %= \cs -> cs Seq.|> c

-- | Literal bit vector.
litBV :: BitVector w -> BVExpr arch fmt w
litBV = LitBV

-- | Get the width of the instruction word
instBytes :: FormulaBuilder arch fmt (BVExpr arch fmt (ArchWidth arch))
instBytes = return InstBytes

-- | Read the pc.
pcRead :: FormulaBuilder arch fmt (BVExpr arch fmt (ArchWidth arch))
pcRead = return PCRead

-- | Read a register.
regRead :: BVExpr arch fmt 5 -> FormulaBuilder arch fmt (BVExpr arch fmt (ArchWidth arch))
regRead = return . RegRead

-- | Read a byte from memory.
memRead :: BVExpr arch fmt (ArchWidth arch)
        -> FormulaBuilder arch fmt (BVExpr arch fmt 8)
memRead addr = return (MemRead addr)

-- | Get XLEN.
xlen :: BVExpr arch fmt (ArchWidth arch)
xlen = XLen

-- | Bitwise and.
andE :: BVExpr arch fmt w
     -> BVExpr arch fmt w
     -> FormulaBuilder arch fmt (BVExpr arch fmt w)
andE e1 e2 = return (AndE e1 e2)

-- | Bitwise or.
orE :: BVExpr arch fmt w
    -> BVExpr arch fmt w
    -> FormulaBuilder arch fmt (BVExpr arch fmt w)
orE e1 e2 = return (OrE e1 e2)

-- | Bitwise xor.
xorE :: BVExpr arch fmt w
     -> BVExpr arch fmt w
     -> FormulaBuilder arch fmt (BVExpr arch fmt w)
xorE e1 e2 = return (XorE e1 e2)

-- | Bitwise not.
notE :: BVExpr arch fmt w -> FormulaBuilder arch fmt (BVExpr arch fmt w)
notE e = return (NotE e)

-- | Add two expressions.
addE :: BVExpr arch fmt w
     -> BVExpr arch fmt w
     -> FormulaBuilder arch fmt (BVExpr arch fmt w)
addE e1 e2 = return (AddE e1 e2)

-- | Subtract the second expression from the first.
subE :: BVExpr arch fmt w
     -> BVExpr arch fmt w
     -> FormulaBuilder arch fmt (BVExpr arch fmt w)
subE e1 e2 = return (SubE e1 e2)

-- | Signed multiply two 'BitVectors', doubling the width of the result to hold all
-- arithmetic overflow bits.
mulsE :: BVExpr arch fmt w
      -> BVExpr arch fmt w
      -> FormulaBuilder arch fmt (BVExpr arch fmt (w+w))
mulsE e1 e2 = return (MulSE e1 e2)

-- | Unsigned multiply two 'BitVectors', doubling the width of the result to hold
-- all arithmetic overflow bits.
muluE :: BVExpr arch fmt w
      -> BVExpr arch fmt w
      -> FormulaBuilder arch fmt (BVExpr arch fmt (w+w))
muluE e1 e2 = return (MulUE e1 e2)

-- | Multiply two 'BitVectors', treating the first as a signed number and the second
-- as an unsigned number, doubling the width of the result to hold all arithmetic
-- overflow bits.
mulsuE :: BVExpr arch fmt w
       -> BVExpr arch fmt w
       -> FormulaBuilder arch fmt (BVExpr arch fmt (w+w))
mulsuE e1 e2 = return (MulSUE e1 e2)

-- | Signed divide two 'BitVectors', rounding to zero.
divsE :: BVExpr arch fmt w
      -> BVExpr arch fmt w
      -> FormulaBuilder arch fmt (BVExpr arch fmt w)
divsE e1 e2 = return (DivSE e1 e2)

-- | Unsigned divide two 'BitVectors', rounding to zero.
divuE :: BVExpr arch fmt w
     -> BVExpr arch fmt w
     -> FormulaBuilder arch fmt (BVExpr arch fmt w)
divuE e1 e2 = return (DivUE e1 e2)

-- | Remainder after signed division of two 'BitVectors', when rounded to zero.
remsE :: BVExpr arch fmt w
     -> BVExpr arch fmt w
     -> FormulaBuilder arch fmt (BVExpr arch fmt w)
remsE e1 e2 = return (RemSE e1 e2)

-- | Remainder after unsigned division of two 'BitVectors', when rounded to zero.
remuE :: BVExpr arch fmt w
     -> BVExpr arch fmt w
     -> FormulaBuilder arch fmt (BVExpr arch fmt w)
remuE e1 e2 = return (RemUE e1 e2)

-- | Left logical shift the first expression by the second.
sllE :: BVExpr arch fmt w
     -> BVExpr arch fmt w
     -> FormulaBuilder arch fmt (BVExpr arch fmt w)
sllE e1 e2 = return (SllE e1 e2)

-- | Left logical shift the first expression by the second.
srlE :: BVExpr arch fmt w
     -> BVExpr arch fmt w
     -> FormulaBuilder arch fmt (BVExpr arch fmt w)
srlE e1 e2 = return (SrlE e1 e2)

-- | Left logical shift the first expression by the second.
sraE :: BVExpr arch fmt w
     -> BVExpr arch fmt w
     -> FormulaBuilder arch fmt (BVExpr arch fmt w)
sraE e1 e2 = return (SraE e1 e2)

-- | Test for equality of two expressions.
eqE :: BVExpr arch fmt w
    -> BVExpr arch fmt w
    -> FormulaBuilder arch fmt (BVExpr arch fmt 1)
eqE e1 e2 = return (EqE e1 e2)

-- | Signed less than
ltsE :: BVExpr arch fmt w
     -> BVExpr arch fmt w
     -> FormulaBuilder arch fmt (BVExpr arch fmt 1)
ltsE e1 e2 = return (LtsE e1 e2)

-- | Unsigned less than
ltuE :: BVExpr arch fmt w
     -> BVExpr arch fmt w
     -> FormulaBuilder arch fmt (BVExpr arch fmt 1)
ltuE e1 e2 = return (LtuE e1 e2)

-- | Zero-extension
-- zextE :: KnownNat w' => BVExpr arch fmt w -> FormulaBuilder arch fmt (BVExpr arch fmt w')
zextE :: KnownNat w' => BVExpr arch fmt w -> FormulaBuilder arch fmt (BVExpr arch fmt w')
zextE e = return (ZExtE knownNat e)

-- | Sign-extension
sextE :: KnownNat w' => BVExpr arch fmt w -> FormulaBuilder arch fmt (BVExpr arch fmt w')
sextE e = return (SExtE knownNat e)

-- | Extract bits
extractE :: KnownNat w' => Int -> BVExpr arch fmt w -> FormulaBuilder arch fmt (BVExpr arch fmt w')
extractE base e = return (ExtractE knownNat base e)

-- | Extract bits with an explicit width argument
extractEWithRepr :: NatRepr w'
                 -> Int
                 -> BVExpr arch fmt w
                 -> FormulaBuilder arch fmt (BVExpr arch fmt w')
extractEWithRepr wRepr base e = return (ExtractE wRepr base e)

-- | Concatenation
concatE :: BVExpr arch fmt w -> BVExpr arch fmt w' -> FormulaBuilder arch fmt (BVExpr arch fmt (w+w'))
concatE e1 e2 = return (ConcatE e1 e2)

-- | Conditional branch.
iteE :: BVExpr arch fmt 1
     -> BVExpr arch fmt w
     -> BVExpr arch fmt w
     -> FormulaBuilder arch fmt (BVExpr arch fmt w)
iteE t e1 e2 = return (IteE t e1 e2)

-- | Add a statement to the formula.
addStmt :: Stmt arch fmt -> FormulaBuilder arch fmt ()
addStmt stmt = fDefs %= \stmts -> stmts Seq.|> stmt

-- TODO: protect against multiple assignments? (for all of the assign* functions)
-- | Add a register assignment to the formula.
assignReg :: BVExpr arch fmt 5
          -> BVExpr arch fmt (ArchWidth arch)
          -> FormulaBuilder arch fmt ()
assignReg r e = addStmt (AssignReg r e)

-- TODO: Should we allow arbitrary width assignments?
-- | Add a memory location assignment to the formula.
assignMem :: BVExpr arch fmt (ArchWidth arch)
          -> BVExpr arch fmt 8
          -> FormulaBuilder arch fmt ()
assignMem addr val = addStmt (AssignMem addr val)

-- | Add a PC assignment to the formula.
assignPC :: BVExpr arch fmt (ArchWidth arch) -> FormulaBuilder arch fmt ()
assignPC pc = addStmt (AssignPC pc)

-- | Conditionally raise an exception.
raiseException :: BVExpr arch fmt 1 -> Exception -> FormulaBuilder arch fmt ()
raiseException cond e = addStmt (RaiseException cond e)

-- | Maps each format to the parameter types for its operands.
-- We include an extra parameter indicating the size of the instruction word for pc
-- incrementing.
type family FormatParams (arch :: BaseArch) (fmt :: Format) :: * where
  FormatParams arch R = (BVExpr arch R 5, BVExpr arch R 5, BVExpr arch R 5)
  FormatParams arch I = (BVExpr arch I 5, BVExpr arch I 5, BVExpr arch I 12)
  FormatParams arch S = (BVExpr arch S 5, BVExpr arch S 5, BVExpr arch S 12)
  FormatParams arch B = (BVExpr arch B 5, BVExpr arch B 5, BVExpr arch B 12)
  FormatParams arch U = (BVExpr arch U 5, BVExpr arch U 20)
  FormatParams arch J = (BVExpr arch J 5, BVExpr arch J 20)
  FormatParams arch X = (BVExpr arch X 32)

params' :: FormatRepr fmt
        -> FormulaBuilder arch fmt (FormatParams arch fmt)
params' repr = case repr of
    RRepr -> do
      let rd  = OperandParam RRd
          rs1 = OperandParam RRs1
          rs2 = OperandParam RRs2
      return (ParamBV rd, ParamBV rs1, ParamBV rs2)
    IRepr -> do
      let rd    = OperandParam IRd
          rs1   = OperandParam IRs1
          imm12 = OperandParam IImm12
      return (ParamBV rd, ParamBV rs1, ParamBV imm12)
    SRepr -> do
      let rs1   = OperandParam SRs1
          rs2   = OperandParam SRs2
          imm12 = OperandParam SImm12
      return (ParamBV rs1, ParamBV rs2, ParamBV imm12)
    BRepr -> do
      let rs1   = OperandParam BRs1
          rs2   = OperandParam BRs2
          imm12 = OperandParam BImm12
      return (ParamBV rs1, ParamBV rs2, ParamBV imm12)
    URepr -> do
      let rd    = OperandParam URd
          imm20 = OperandParam UImm20
      return (ParamBV rd, ParamBV imm20)
    JRepr -> do
      let rd    = OperandParam JRd
          imm20 = OperandParam JImm20
      return (ParamBV rd, ParamBV imm20)
    XRepr -> do
      let ill = OperandParam XImm32
      return (ParamBV ill)

-- | Get the parameters for a particular known format
params :: (KnownRepr FormatRepr fmt) => FormulaBuilder arch fmt (FormatParams arch fmt)
params = params' knownRepr
