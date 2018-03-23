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
-- TODO: This is a variable-width ISA, but we have separated the semantics out from
-- the decoding so cleanly that we actually don't know how big the instruction word
-- is here, and therefore we don't know how much to increment the PC by after most
-- instructions. We could either handle that externally (yuck), or we could include
-- an additional field in each instruction. Then the params function could provide
-- that information along with the operands. Something to think about.
module RISCV.Semantics
  ( -- * Types
    OperandParam
  , BVExpr
  , Exception(..)
  , Stmt
  , Formula
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
  -- ** Bitwise
  , andE
  , orE
  , xorE
  , notE
  -- ** Arithmetic
  , addE
  , subE
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

import Control.Lens ( (%=) )
import Control.Lens.TH (makeLenses)
import Control.Monad.State
import Data.BitVector.Sized
import Data.Foldable (toList)
import Data.Parameterized
import Data.Parameterized.TH.GADT
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import GHC.TypeLits

import RISCV.Format

-- Operand identifiers

data OperandID = Rd | Rs1 | Rs2 | Imm12 | Imm20 | Imm32

data OperandIDRepr :: OperandID -> * where
  RdRepr    :: OperandIDRepr 'Rd
  Rs1Repr   :: OperandIDRepr 'Rs1
  Rs2Repr   :: OperandIDRepr 'Rs2
  Imm12Repr :: OperandIDRepr 'Imm12
  Imm20Repr :: OperandIDRepr 'Imm20
  Imm32Repr :: OperandIDRepr 'Imm32

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
instance KnownRepr OperandIDRepr 'Rd    where knownRepr = RdRepr
instance KnownRepr OperandIDRepr 'Rs1   where knownRepr = Rs1Repr
instance KnownRepr OperandIDRepr 'Rs2   where knownRepr = Rs2Repr
instance KnownRepr OperandIDRepr 'Imm12 where knownRepr = Imm12Repr
instance KnownRepr OperandIDRepr 'Imm20 where knownRepr = Imm20Repr
instance KnownRepr OperandIDRepr 'Imm32 where knownRepr = Imm32Repr

----------------------------------------
-- Expressions, statements, and formulas

-- | Formula parameter (represents unknown operands)
data OperandParam (arch :: Arch) (oid :: OperandID)
  = OperandParam (OperandIDRepr oid)

instance Show (OperandParam arch oid) where
  show (OperandParam repr) = show repr
instance ShowF (OperandParam arch)

-- | BitVector expressions. These are the building blocks for semantic formulas for
-- an instruction.
data BVExpr (arch :: Arch) (w :: Nat) where
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

  -- Bitwise operations
  And :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  Or  :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  Xor :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  Not :: BVExpr arch w -> BVExpr arch w

  -- Arithmetic operations
  Add :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  Sub :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  -- TODO: does the shift amount have to be the same width as the shiftee?
  Sll :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  Srl :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w
  Sra :: BVExpr arch w -> BVExpr arch w -> BVExpr arch w

  -- Comparisons
  Eq :: BVExpr arch w -> BVExpr arch w -> BVExpr arch 1
  Ltu :: BVExpr arch w -> BVExpr arch w -> BVExpr arch 1
  Lts :: BVExpr arch w -> BVExpr arch w -> BVExpr arch 1

  -- Width-changing
  ZExt :: NatRepr w' -> BVExpr arch w -> BVExpr arch w'
  SExt :: NatRepr w' -> BVExpr arch w -> BVExpr arch w'
  Extract :: NatRepr w' -> Int -> BVExpr arch w -> BVExpr arch w'

  -- Other operations
  Ite :: BVExpr arch 1
      -> BVExpr arch w
      -> BVExpr arch w
      -> BVExpr arch w

instance Show (BVExpr arch w) where
  show (LitBV bv) = show bv
  show (ParamBV p) = show p
  show (InstBytes) = "ib"
  show PCRead = "pc"
  show (RegRead r) = "x[" ++ show r ++ "]"
  show (MemRead bRepr addr) =
    "M[" ++ show addr ++ "][" ++ show (8 * (natValue bRepr) - 1) ++ ":0]"
  show (And e1 e2) = show e1 ++ " & " ++ show e2
  show (Or  e1 e2) = show e1 ++ " | " ++ show e2
  show (Xor e1 e2) = show e1 ++ " ^ " ++ show e2
  show (Not e) = "~" ++ show e
  show (Add e1 e2) = show e1 ++ " + " ++ show e2
  show (Sub e1 e2) = show e1 ++ " - " ++ show e2
  show (Sll e1 e2) = show e1 ++ " << " ++ show e2
  show (Srl e1 e2) = show e1 ++ " >>_l " ++ show e2
  show (Sra e1 e2) = show e1 ++ " >>_a " ++ show e2
  show (Eq  e1 e2) = show e1 ++ " = " ++ show e2
  show (Ltu  e1 e2) = show e1 ++ " <_u " ++ show e2
  show (Lts  e1 e2) = show e1 ++ " <_s " ++ show e2
  show (ZExt _ e) = "zext(" ++ show e ++ ")"
  show (SExt _ e) = "sext(" ++ show e ++ ")"
  show (Extract wRepr base e) =
    show e ++ "[" ++ show (base + fromIntegral (natValue wRepr)) ++ ":" ++ show base ++ "]"
  show (Ite t e1 e2) =
    "if (" ++ show t ++ ") then " ++ show e1 ++ " else " ++ show e2
instance ShowF (BVExpr arch)

data Exception = EnvironmentCall
               | Breakpoint
               | IllegalInstruction
  deriving (Show)

-- | A 'Stmt' represents an atomic state transformation -- typically, an assignment
-- of a state component (register, memory location, etc.) to a 'BVExpr' of the
-- appropriate width.
data Stmt (arch :: Arch) where
  AssignReg :: BVExpr arch 5 -> BVExpr arch (ArchWidth arch) -> Stmt arch
  AssignMem :: NatRepr bytes
            -> BVExpr arch (ArchWidth arch)
            -> BVExpr arch (8*bytes)
            -> Stmt arch
  AssignPC  :: BVExpr arch (ArchWidth arch) -> Stmt arch
  RaiseException :: Exception -> Stmt arch

instance Show (Stmt arch) where
  show (AssignReg r e) = "x[" ++ show r ++ "]' = " ++ show e
  -- TODO: Should we indicate how many bytes are being written? Or will that always
  -- be inferrable from the right-hand side?
  show (AssignMem _ addr e) = "M[" ++ show addr ++ "]' = " ++ show e
  show (AssignPC pc) = "pc' = " ++ show pc
  show (RaiseException e) = "RaiseException(" ++ show e ++ ")"

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
  = Formula { _fComment :: Seq String
              -- ^ multiline comment
            , _fDef     :: Seq (Stmt arch)
              -- ^ sequence of statements defining the formula
            }
makeLenses ''Formula

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

-- | Obtain the formula defined by a 'FormulaBuilder' action.
getFormula :: FormulaBuilder arch fmt () -> Formula arch fmt
getFormula = flip execState emptyFormula . unFormulaBuilder

-- | Add a comment.
comment :: String -> FormulaBuilder arch fmt ()
comment c = fComment %= \cs -> cs Seq.|> c

-- | Literal bit vector.
litBV :: BitVector w -> BVExpr arch w
litBV = LitBV

-- | Get the width of the instruction word
instBytes :: KnownNat (ArchWidth arch)
          => FormulaBuilder arch fmt (BVExpr arch (ArchWidth arch))
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

-- | Bitwise and.
andE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
andE e1 e2 = return (And e1 e2)

-- | Bitwise or.
orE :: BVExpr arch w
    -> BVExpr arch w
    -> FormulaBuilder arch fmt (BVExpr arch w)
orE e1 e2 = return (Or e1 e2)

-- | Bitwise xor.
xorE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
xorE e1 e2 = return (Xor e1 e2)

-- | Bitwise not.
notE :: BVExpr arch w -> FormulaBuilder arch fmt (BVExpr arch w)
notE e = return (Not e)

-- | Add two expressions.
addE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
addE e1 e2 = return (Add e1 e2)

-- | Subtract the second expression from the first.
subE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
subE e1 e2 = return (Sub e1 e2)

-- | Left logical shift the first expression by the second.
sllE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
sllE e1 e2 = return (Sll e1 e2)

-- | Left logical shift the first expression by the second.
srlE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
srlE e1 e2 = return (Srl e1 e2)

-- | Left logical shift the first expression by the second.
sraE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
sraE e1 e2 = return (Sra e1 e2)

-- | Test for equality of two expressions.
eqE :: BVExpr arch w
    -> BVExpr arch w
    -> FormulaBuilder arch fmt (BVExpr arch 1)
eqE e1 e2 = return (Eq e1 e2)

-- | Signed less than
ltsE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch 1)
ltsE e1 e2 = return (Lts e1 e2)

-- | Unsigned less than
ltuE :: BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch 1)
ltuE e1 e2 = return (Ltu e1 e2)

-- | Zero-extension
zextE :: KnownNat w' => BVExpr arch w -> FormulaBuilder arch fmt (BVExpr arch w')
zextE e = return (ZExt knownNat e)

-- | Sign-extension
sextE :: KnownNat w' => BVExpr arch w -> FormulaBuilder arch fmt (BVExpr arch w')
sextE e = return (SExt knownNat e)

-- | Extract bits
extractE :: KnownNat w' => Int -> BVExpr arch w -> FormulaBuilder arch fmt (BVExpr arch w')
extractE base e = return (Extract knownNat base e)

-- | Extract bits with an explicit width argument
extractEWithRepr :: NatRepr w'
                 -> Int
                 -> BVExpr arch w
                 -> FormulaBuilder arch fmt (BVExpr arch w')
extractEWithRepr wRepr base e = return (Extract wRepr base e)

-- | Conditional branch.
iteE :: BVExpr arch 1
     -> BVExpr arch w
     -> BVExpr arch w
     -> FormulaBuilder arch fmt (BVExpr arch w)
iteE t e1 e2 = return (Ite t e1 e2)

-- | Add a statement to the formula.
addStmt :: Stmt arch -> FormulaBuilder arch fmt ()
addStmt stmt = fDef %= \stmts -> stmts Seq.|> stmt

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

assignMemWithRepr :: NatRepr bytes
                  -> BVExpr arch (ArchWidth arch)
                  -> BVExpr arch (8*bytes)
                  -> FormulaBuilder arch fmt ()
assignMemWithRepr bRepr addr val = addStmt (AssignMem bRepr addr val)

-- | Add a PC assignment to the formula.
assignPC :: BVExpr arch (ArchWidth arch) -> FormulaBuilder arch fmt ()
assignPC pc = addStmt (AssignPC pc)

-- | Raise an exception.
raiseException :: Exception -> FormulaBuilder arch fmt ()
raiseException e = addStmt (RaiseException e)

-- | Maps each format to the parameter types for its operands.
-- We include an extra parameter indicating the size of the instruction word for pc
-- incrementing.
type family FormatParams (arch :: Arch) (fmt :: Format) :: * where
  FormatParams arch 'R = (BVExpr arch 5, BVExpr arch 5, BVExpr arch 5)
  FormatParams arch 'I = (BVExpr arch 5, BVExpr arch 5, BVExpr arch 12)
  FormatParams arch 'S = (BVExpr arch 5, BVExpr arch 5, BVExpr arch 12)
  FormatParams arch 'B = (BVExpr arch 5, BVExpr arch 5, BVExpr arch 12)
  FormatParams arch 'U = (BVExpr arch 5, BVExpr arch 20)
  FormatParams arch 'J = (BVExpr arch 5, BVExpr arch 20)
  FormatParams arch 'E = ()
  FormatParams arch 'X = (BVExpr arch 32)

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
    _ -> undefined

-- | Get the parameters for a particular known format
params :: (KnownRepr FormatRepr fmt) => FormulaBuilder arch fmt (FormatParams arch fmt)
params = params' knownRepr

----------------------------------------
