{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
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
    Parameter
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
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import GHC.TypeLits

import RISCV.Format

----------------------------------------
-- Expressions, statements, and formulas

-- | Formula parameter (represents unknown operands)
data Parameter (arch :: Arch) (w :: Nat)
  = Parameter (NatRepr w) String

instance Show (Parameter arch w) where
  show (Parameter _repr name) = name
instance ShowF (Parameter arch)

-- | BitVector expressions. These are the building blocks for semantic formulas for
-- an instruction.
data BVExpr (arch :: Arch) (w :: Nat) where
  -- Basic constructors
  LitBV :: BitVector w -> BVExpr arch w
  ParamBV :: Parameter arch w -> BVExpr arch w

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

-- TODO: Parameterize Formula and FormulaBuilder by instruction format. Have
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
    -- "Parameters: " ++ showParams ++ "\n" ++
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
      let rd  = Parameter knownNat "rd"
          rs1 = Parameter knownNat "rs1"
          rs2 = Parameter knownNat "rs2"
      return (ParamBV rd, ParamBV rs1, ParamBV rs2)
    IRepr -> do
      let rd    = Parameter knownNat "rd"
          rs1   = Parameter knownNat "rs1"
          imm12 = Parameter knownNat "imm12"
      return (ParamBV rd, ParamBV rs1, ParamBV imm12)
    SRepr -> do
      let rs1   = Parameter knownNat "rs1"
          rs2   = Parameter knownNat "rs2"
          imm12 = Parameter knownNat "imm12"
      return (ParamBV rs1, ParamBV rs2, ParamBV imm12)
    BRepr -> do
      let rs1   = Parameter knownNat "rs1"
          rs2   = Parameter knownNat "rs2"
          imm12 = Parameter knownNat "imm12"
      return (ParamBV rs1, ParamBV rs2, ParamBV imm12)
    URepr -> do
      let rd    = Parameter knownNat "rd"
          imm20 = Parameter knownNat "imm20"
      return (ParamBV rd, ParamBV imm20)
    JRepr -> do
      let rd    = Parameter knownNat "rd"
          imm20 = Parameter knownNat "imm20"
      return (ParamBV rd, ParamBV imm20)
    _ -> undefined

-- | Get the parameters for a particular known format
params :: (KnownRepr FormatRepr fmt) => FormulaBuilder arch fmt (FormatParams arch fmt)
params = params' knownRepr

-- | Get the width of the instruction word
instBytes :: KnownNat (ArchWidth arch)
          => FormulaBuilder arch fmt (BVExpr arch (ArchWidth arch))
instBytes = return $ ParamBV (Parameter knownNat "instBytes")
