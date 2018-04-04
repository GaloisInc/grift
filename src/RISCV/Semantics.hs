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
    OperandID(..)
  , OperandIDRepr(..)
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
  , memReadWithRepr
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
  -- ** Control
  , iteE
  -- ** State actions
  , assignReg
  , assignMem
  , assignMemWithRepr
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
data OperandID = Rd | Rs1 | Rs2 | Imm12 | Imm20 | Imm32

type Rd    = 'Rd
type Rs1   = 'Rs1
type Rs2   = 'Rs2
type Imm12 = 'Imm12
type Imm20 = 'Imm20
type Imm32 = 'Imm32

-- | Type-level representative for 'OperandID'.
data OperandIDRepr :: OperandID -> * where
  RdRepr    :: OperandIDRepr Rd
  Rs1Repr   :: OperandIDRepr Rs1
  Rs2Repr   :: OperandIDRepr Rs2
  Imm12Repr :: OperandIDRepr Imm12
  Imm20Repr :: OperandIDRepr Imm20
  Imm32Repr :: OperandIDRepr Imm32

-- | Maps an 'OperandID' to its length as a 'BitVector'.
type family OperandIDWidth (oi :: OperandID) :: Nat where
  OperandIDWidth Rd    = 5
  OperandIDWidth Rs1   = 5
  OperandIDWidth Rs2   = 5
  OperandIDWidth Imm12 = 12
  OperandIDWidth Imm20 = 20
  OperandIDWidth Imm32 = 32

-- Instances
$(return [])
instance Show (OperandIDRepr k) where
  show RdRepr = "rd"
  show Rs1Repr = "rs1"
  show Rs2Repr = "rs2"
  show Imm12Repr = "imm12"
  show Imm20Repr = "imm20"
  show Imm32Repr = "imm32"

instance ShowF OperandIDRepr
deriving instance Eq (OperandIDRepr k)
instance EqF OperandIDRepr where
  eqF = (==)
instance TestEquality OperandIDRepr where
  testEquality = $(structuralTypeEquality [t|OperandIDRepr|] [])
instance OrdF OperandIDRepr where
  compareF = $(structuralTypeOrd [t|OperandIDRepr|] [])
instance KnownRepr OperandIDRepr Rd    where knownRepr = RdRepr
instance KnownRepr OperandIDRepr Rs1   where knownRepr = Rs1Repr
instance KnownRepr OperandIDRepr Rs2   where knownRepr = Rs2Repr
instance KnownRepr OperandIDRepr Imm12 where knownRepr = Imm12Repr
instance KnownRepr OperandIDRepr Imm20 where knownRepr = Imm20Repr
instance KnownRepr OperandIDRepr Imm32 where knownRepr = Imm32Repr

----------------------------------------
-- Expressions, statements, and formulas

-- | Formula parameter (represents unknown operands)
data OperandParam (arch :: BaseArch) (oid :: OperandID)
  = OperandParam (OperandIDRepr oid)

instance Show (OperandParam arch oid) where
  show (OperandParam repr) = show repr
instance ShowF (OperandParam arch)

-- | BitVector expressions. These are the building blocks for semantic formulas for
-- an instruction.
data BVExpr (arch :: BaseArch) (w :: Nat) where
  -- Basic constructors
  LitBV :: BitVector w -> BVExpr arch w
  ParamBV :: OperandParam arch oid -> BVExpr arch (OperandIDWidth oid)
  InstBytes :: BVExpr arch (ArchWidth arch)

  -- Accessing state
  PCRead  :: BVExpr arch (ArchWidth arch)
  RegRead :: BVExpr arch 5 -> BVExpr arch (ArchWidth arch)
  MemRead :: NatRepr bytes
          -> BVExpr arch (ArchWidth arch)
          -> BVExpr arch (8*bytes)
  -- | This is temporary; when we have CSRs this will change.
  XLen :: BVExpr arch (ArchWidth arch)

  -- Bitwise operations
  AndE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  OrE  :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  XorE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  NotE :: BVExpr arch w -> BVExpr arch w

  -- Arithmetic operations
  AddE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  SubE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  MulSE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch (w+w)
  MulUE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch (w+w)
  MulSUE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch (w+w)
  DivUE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  DivSE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  RemUE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  RemSE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  -- TODO: does the shift amount have to be the same width as the shiftee?
  SllE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  SrlE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  SraE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w

  -- Comparisons
  EqE  :: BVExpr arch w -> BVExpr arch w -> BVExpr arch 1
  LtuE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch 1
  LtsE :: BVExpr arch w -> BVExpr arch w -> BVExpr arch 1

  -- Width-changing
  ZExtE :: NatRepr w' -> BVExpr arch w -> BVExpr arch w'
  SExtE :: NatRepr w' -> BVExpr arch w -> BVExpr arch w'
  ExtractE :: NatRepr w' -> Int -> BVExpr arch w -> BVExpr arch w'

  -- Other operations
  IteE :: BVExpr arch 1
       -> BVExpr arch w
       -> BVExpr arch w
       -> BVExpr arch w

-- TODO: Fix this horrible pretty printing.
instance Show (BVExpr arch w) where
  show (LitBV bv) = show bv
  show (ParamBV p) = show p
  show InstBytes = "ib"
  show PCRead = "pc"
  show (RegRead r) = "x[" ++ show r ++ "]"
  show (MemRead bRepr addr) =
    "M[" ++ show addr ++ "][" ++ show (8 * natValue bRepr - 1) ++ ":0]"
  show (AndE e1 e2) = show e1 ++ " & " ++ show e2
  show (OrE  e1 e2) = show e1 ++ " | " ++ show e2
  show (XorE e1 e2) = show e1 ++ " ^ " ++ show e2
  show (NotE e) = "~" ++ show e
  show (AddE e1 e2) = show e1 ++ " + " ++ show e2
  show (SubE e1 e2) = show e1 ++ " - " ++ show e2
  show (MulUE e1 e2) = show e1 ++ " u*u " ++ show e2
  show (MulSE e1 e2) = show e1 ++ " s*s " ++ show e2
  show (MulSUE e1 e2) = show e1 ++ " s*u " ++ show e2
  show (DivUE e1 e2) = show e1 ++ " u/ " ++ show e2
  show (DivSE e1 e2) = show e1 ++ " s/ " ++ show e2
  show (RemUE e1 e2) = show e1 ++ " u% " ++ show e2
  show (RemSE e1 e2) = show e1 ++ " s% " ++ show e2
  show (SllE e1 e2) = show e1 ++ " << " ++ show e2
  show (SrlE e1 e2) = show e1 ++ " >>_l " ++ show e2
  show (SraE e1 e2) = show e1 ++ " >>_a " ++ show e2
  show (EqE  e1 e2) = show e1 ++ " = " ++ show e2
  show (LtuE e1 e2) = show e1 ++ " u< " ++ show e2
  show (LtsE e1 e2) = show e1 ++ " s< " ++ show e2
  show (ZExtE _ e) = "zext(" ++ show e ++ ")"
  show (SExtE _ e) = "sext(" ++ show e ++ ")"
  show (ExtractE wRepr base e) =
    show e ++ "[" ++ show (base + fromIntegral (natValue wRepr)) ++ ":" ++ show base ++ "]"
  show (IteE t e1 e2) =
    "if (" ++ show t ++ ") then " ++ show e1 ++ " else " ++ show e2
instance ShowF (BVExpr arch)

-- | Runtime exception.
data Exception = EnvironmentCall
               | Breakpoint
               | IllegalInstruction
  deriving (Show)

-- | A 'Stmt' represents an atomic state transformation -- typically, an assignment
-- of a state component (register, memory location, etc.) to a 'BVExpr' of the
-- appropriate width.
data Stmt (arch :: BaseArch) where
  AssignReg :: BVExpr arch 5 -> BVExpr arch (ArchWidth arch) -> Stmt arch
  AssignMem :: NatRepr bytes
            -> BVExpr arch (ArchWidth arch)
            -> BVExpr arch (8*bytes)
            -> Stmt arch
  AssignPC  :: BVExpr arch (ArchWidth arch) -> Stmt arch
  RaiseException :: BVExpr arch 1 -> Exception -> Stmt arch

instance Show (Stmt arch) where
  show (AssignReg r e) = "x[" ++ show r ++ "]' = " ++ show e
  -- TODO: Should we indicate how many bytes are being written? Or will that always
  -- be inferrable from the right-hand side?
  show (AssignMem _ addr e) = "M[" ++ show addr ++ "]' = " ++ show e
  show (AssignPC pc) = "pc' = " ++ show pc
  show (RaiseException cond e) = "if " ++ show cond ++ " then RaiseException(" ++ show e ++ ")"

-- TODO: OperandParamize Formula and FormulaBuilder by instruction format. Have
-- special-purpose param functions for each format, that return the parameters as a
-- tuple.
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
            , _fDefs    :: Seq (Stmt arch)
              -- ^ sequence of statements defining the formula
            }

-- | Lens for 'Formula' comments.
fComments :: Simple Lens (Formula arch fmt) (Seq String)
fComments = lens _fComments (\(Formula _ d) c -> Formula c d)

-- | Lens for 'Formula' statements.
fDefs :: Simple Lens (Formula arch fmt) (Seq (Stmt arch))
fDefs = lens _fDefs (\(Formula c _) d -> Formula c d)

instance Show (Formula arch fmt) where
  show (Formula comments defs) =
    showComments ++
    -- "OperandParams: " ++ showParams ++ "\n" ++
    showDefs
    where showComments = concat (toList ((++ "\n") <$> comments))
--          showParams = intercalate ", " (toList (show <$> params))
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

-- TODO: Not all of these need to be in FormulaBuilder, right? Maybe change these
-- functions to return pure BVExprs when I can, or just remove them altogether and go
-- with the BVExpr constructors.

-- | Obtain the formula defined by a 'FormulaBuilder' action.
getFormula :: FormulaBuilder arch fmt () -> Formula arch fmt
getFormula = flip execState emptyFormula . unFormulaBuilder

-- | Add a comment.
comment :: String -> FormulaBuilder arch fmt ()
comment c = fComments %= \cs -> cs Seq.|> c

-- | Literal bit vector.
litBV :: BitVector w -> BVExpr arch w
litBV = LitBV

-- | Get the width of the instruction word
instBytes :: FormulaBuilder arch fmt (BVExpr arch (ArchWidth arch))
instBytes = return InstBytes

-- | Read the pc.
pcRead :: FormulaBuilder arch fmt (BVExpr arch (ArchWidth arch))
pcRead = return PCRead

-- | Read a register.
regRead :: BVExpr arch 5 -> FormulaBuilder arch fmt (BVExpr arch (ArchWidth arch))
regRead = return . RegRead

-- | Read a memory location.
memRead :: KnownNat bytes
        => BVExpr arch (ArchWidth arch)
        -> FormulaBuilder arch fmt (BVExpr arch (8*bytes))
memRead addr = return (MemRead knownNat addr)

-- | Read a memory location with an explicit width argument.
memReadWithRepr :: NatRepr bytes
                -> BVExpr arch (ArchWidth arch)
                -> FormulaBuilder arch fmt (BVExpr arch (8*bytes))
memReadWithRepr bRepr addr = return (MemRead bRepr addr)

-- | Get XLEN.
xlen :: BVExpr arch (ArchWidth arch)
xlen = XLen

-- | Bitwise and.
andE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
andE e1 e2 = return (AndE e1 e2)

-- | Bitwise or.
orE :: BVExpr arch w
    -> BVExpr arch w
    -> FormulaBuilder arch fmt (BVExpr arch w)
orE e1 e2 = return (OrE e1 e2)

-- | Bitwise xor.
xorE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
xorE e1 e2 = return (XorE e1 e2)

-- | Bitwise not.
notE :: BVExpr arch w -> FormulaBuilder arch fmt (BVExpr arch w)
notE e = return (NotE e)

-- | Add two expressions.
addE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
addE e1 e2 = return (AddE e1 e2)

-- | Subtract the second expression from the first.
subE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
subE e1 e2 = return (SubE e1 e2)

-- | Signed multiply two 'BitVectors', doubling the width of the result to hold all
-- arithmetic overflow bits.
mulsE :: BVExpr arch w
      -> BVExpr arch w
      -> FormulaBuilder arch fmt (BVExpr arch (w+w))
mulsE e1 e2 = return (MulSE e1 e2)

-- | Unsigned multiply two 'BitVectors', doubling the width of the result to hold
-- all arithmetic overflow bits.
muluE :: BVExpr arch w
      -> BVExpr arch w
      -> FormulaBuilder arch fmt (BVExpr arch (w+w))
muluE e1 e2 = return (MulUE e1 e2)

-- | Multiply two 'BitVectors', treating the first as a signed number and the second
-- as an unsigned number, doubling the width of the result to hold all arithmetic
-- overflow bits.
mulsuE :: BVExpr arch w
       -> BVExpr arch w
       -> FormulaBuilder arch fmt (BVExpr arch (w+w))
mulsuE e1 e2 = return (MulSUE e1 e2)

-- | Signed divide two 'BitVectors', rounding to zero.
divsE :: BVExpr arch w
      -> BVExpr arch w
      -> FormulaBuilder arch fmt (BVExpr arch w)
divsE e1 e2 = return (DivSE e1 e2)

-- | Unsigned divide two 'BitVectors', rounding to zero.
divuE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
divuE e1 e2 = return (DivUE e1 e2)

-- | Remainder after signed division of two 'BitVectors', when rounded to zero.
remsE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
remsE e1 e2 = return (RemSE e1 e2)

-- | Remainder after unsigned division of two 'BitVectors', when rounded to zero.
remuE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
remuE e1 e2 = return (RemUE e1 e2)

-- | Left logical shift the first expression by the second.
sllE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
sllE e1 e2 = return (SllE e1 e2)

-- | Left logical shift the first expression by the second.
srlE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
srlE e1 e2 = return (SrlE e1 e2)

-- | Left logical shift the first expression by the second.
sraE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
sraE e1 e2 = return (SraE e1 e2)

-- | Test for equality of two expressions.
eqE :: BVExpr arch w
    -> BVExpr arch w
    -> FormulaBuilder arch fmt (BVExpr arch 1)
eqE e1 e2 = return (EqE e1 e2)

-- | Signed less than
ltsE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch 1)
ltsE e1 e2 = return (LtsE e1 e2)

-- | Unsigned less than
ltuE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch 1)
ltuE e1 e2 = return (LtuE e1 e2)

-- | Zero-extension
-- zextE :: KnownNat w' => BVExpr arch w -> FormulaBuilder arch fmt (BVExpr arch w')
zextE :: KnownNat w' => BVExpr arch w -> FormulaBuilder arch fmt (BVExpr arch w')
zextE e = return (ZExtE knownNat e)

-- | Sign-extension
sextE :: KnownNat w' => BVExpr arch w -> FormulaBuilder arch fmt (BVExpr arch w')
sextE e = return (SExtE knownNat e)

-- | Extract bits
extractE :: KnownNat w' => Int -> BVExpr arch w -> FormulaBuilder arch fmt (BVExpr arch w')
extractE base e = return (ExtractE knownNat base e)

-- | Extract bits with an explicit width argument
extractEWithRepr :: NatRepr w'
                 -> Int
                 -> BVExpr arch w
                 -> FormulaBuilder arch fmt (BVExpr arch w')
extractEWithRepr wRepr base e = return (ExtractE wRepr base e)

-- | Conditional branch.
iteE :: BVExpr arch 1
     -> BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
iteE t e1 e2 = return (IteE t e1 e2)

-- | Add a statement to the formula.
addStmt :: Stmt arch -> FormulaBuilder arch fmt ()
addStmt stmt = fDefs %= \stmts -> stmts Seq.|> stmt

-- TODO: protect against multiple assignments? (for all of the assign* functions)
-- | Add a register assignment to the formula.
assignReg :: BVExpr arch 5
          -> BVExpr arch (ArchWidth arch)
          -> FormulaBuilder arch fmt ()
assignReg r e = addStmt (AssignReg r e)

-- TODO: Should we allow arbitrary width assignments?
-- | Add a memory location assignment to the formula.
assignMem :: KnownNat bytes
          => BVExpr arch (ArchWidth arch)
          -> BVExpr arch (8*bytes)
          -> FormulaBuilder arch fmt ()
assignMem addr val = addStmt (AssignMem knownNat addr val)

-- | 'assignMem' with explicit 'NatRepr' indicating number of bytes to be read.
assignMemWithRepr :: NatRepr bytes
                  -> BVExpr arch (ArchWidth arch)
                  -> BVExpr arch (8*bytes)
                  -> FormulaBuilder arch fmt ()
assignMemWithRepr bRepr addr val = addStmt (AssignMem bRepr addr val)

-- | Add a PC assignment to the formula.
assignPC :: BVExpr arch (ArchWidth arch) -> FormulaBuilder arch fmt ()
assignPC pc = addStmt (AssignPC pc)

-- | Conditionally raise an exception.
raiseException :: BVExpr arch 1 -> Exception -> FormulaBuilder arch fmt ()
raiseException cond e = addStmt (RaiseException cond e)

-- | Maps each format to the parameter types for its operands.
-- We include an extra parameter indicating the size of the instruction word for pc
-- incrementing.
type family FormatParams (arch :: BaseArch) (fmt :: Format) :: * where
  FormatParams arch R = (BVExpr arch 5, BVExpr arch 5, BVExpr arch 5)
  FormatParams arch I = (BVExpr arch 5, BVExpr arch 5, BVExpr arch 12)
  FormatParams arch S = (BVExpr arch 5, BVExpr arch 5, BVExpr arch 12)
  FormatParams arch B = (BVExpr arch 5, BVExpr arch 5, BVExpr arch 12)
  FormatParams arch U = (BVExpr arch 5, BVExpr arch 20)
  FormatParams arch J = (BVExpr arch 5, BVExpr arch 20)
  FormatParams arch X = (BVExpr arch 32)

params' :: FormatRepr fmt
        -> FormulaBuilder arch fmt (FormatParams arch fmt)
params' repr = case repr of
    RRepr -> do
      let rd  = OperandParam RdRepr
          rs1 = OperandParam Rs1Repr
          rs2 = OperandParam Rs2Repr
      return (ParamBV rd, ParamBV rs1, ParamBV rs2)
    IRepr -> do
      let rd    = OperandParam RdRepr
          rs1   = OperandParam Rs1Repr
          imm12 = OperandParam Imm12Repr
      return (ParamBV rd, ParamBV rs1, ParamBV imm12)
    SRepr -> do
      let rs1   = OperandParam Rs1Repr
          rs2   = OperandParam Rs2Repr
          imm12 = OperandParam Imm12Repr
      return (ParamBV rs1, ParamBV rs2, ParamBV imm12)
    BRepr -> do
      let rs1   = OperandParam Rs1Repr
          rs2   = OperandParam Rs2Repr
          imm12 = OperandParam Imm12Repr
      return (ParamBV rs1, ParamBV rs2, ParamBV imm12)
    URepr -> do
      let rd    = OperandParam RdRepr
          imm20 = OperandParam Imm20Repr
      return (ParamBV rd, ParamBV imm20)
    JRepr -> do
      let rd    = OperandParam RdRepr
          imm20 = OperandParam Imm20Repr
      return (ParamBV rd, ParamBV imm20)
    XRepr -> do
      let ill = OperandParam Imm32Repr
      return (ParamBV ill)

-- | Get the parameters for a particular known format
params :: (KnownRepr FormatRepr fmt) => FormulaBuilder arch fmt (FormatParams arch fmt)
params = params' knownRepr
