{-# LANGUAGE BinaryLiterals        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module      : RISCV.Instruction
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

AST for Instruction data type, parameterized by instruction format (R, I, S, ...).
-}

-- TODO: Consider getting rid of the InstWord type and just doing everything by
-- number of bits. It's confusing to have two kinds of NatRepr's floating around.

module RISCV.Instruction
  ( -- * Instructions
    Instruction(..)
  , InstructionSet(..)
  , instructionSet
  , EncodeMap, DecodeMap, SemanticsMap
  , InstWord
    -- * Architecture types
  , Arch(..)
  , ArchRepr(..)
    -- * Instruction formats
  , Format(..)
  , FormatRepr(..)
    -- * Opcodes
  , Opcode(..)
  , OpBits(..)
    -- * Operands
  , Operands(..)
    -- * Opcode / OpBits conversion
  , opcodeFromOpBits
  , opBitsFromOpcode
  , semanticsFromOpcode
  ) where

import Data.BitVector.Sized
import Data.Parameterized
import qualified Data.Parameterized.Map as Map
import Data.Parameterized.Map (MapF)
import Data.Parameterized.TH.GADT
import Foreign.Marshal.Utils (fromBool)
import GHC.TypeLits

import RISCV.Format
import RISCV.Semantics

----------------------------------------
-- Operands

-- TODO: Consider making the operand arguments lists of bit vector types???
-- | RISC-V Operand lists, parameterized by format. There is exactly one constructor
-- per format.
data Operands :: Format -> * where
  ROperands :: BitVector 5 -> BitVector 5  -> BitVector 5  -> Operands 'R
  IOperands :: BitVector 5 -> BitVector 5  -> BitVector 12 -> Operands 'I
  SOperands :: BitVector 5 -> BitVector 5  -> BitVector 12 -> Operands 'S
  BOperands :: BitVector 5 -> BitVector 5  -> BitVector 12 -> Operands 'B
  UOperands :: BitVector 5 -> BitVector 20                 -> Operands 'U
  JOperands :: BitVector 5 -> BitVector 20                 -> Operands 'J
  EOperands ::                                                Operands 'E
  XOperands :: BitVector 32                                -> Operands 'X

-- Instances
$(return [])
deriving instance Show (Operands k)
instance ShowF Operands
deriving instance Eq (Operands k)
instance EqF Operands where
  eqF = (==)
instance TestEquality Operands where
  testEquality = $(structuralTypeEquality [t|Operands|] [])
instance OrdF Operands where
  compareF = $(structuralTypeOrd [t|Operands|] [])

----------------------------------------
-- Opcodes

-- | RISC-V Opcodes, parameterized by format.
data Opcode :: Format -> * where

  -- R type
  Add    :: Opcode 'R -- RV32I
  Sub    :: Opcode 'R
  Sll    :: Opcode 'R
  Slt    :: Opcode 'R
  Sltu   :: Opcode 'R
  Xor    :: Opcode 'R
  Srl    :: Opcode 'R
  Sra    :: Opcode 'R
  Or     :: Opcode 'R
  And    :: Opcode 'R
  Mul    :: Opcode 'R -- RV32M
  Mulh   :: Opcode 'R
  Mulhsu :: Opcode 'R
  Mulhu  :: Opcode 'R
  Div    :: Opcode 'R
  Divu   :: Opcode 'R
  Rem    :: Opcode 'R
  Remu   :: Opcode 'R

  -- I type
  Jalr    :: Opcode 'I -- RV32I
  Lb      :: Opcode 'I
  Lh      :: Opcode 'I
  Lw      :: Opcode 'I
  Lbu     :: Opcode 'I
  Lhu     :: Opcode 'I
  Addi    :: Opcode 'I
  Slti    :: Opcode 'I
  Sltiu   :: Opcode 'I
  Xori    :: Opcode 'I
  Ori     :: Opcode 'I
  Andi    :: Opcode 'I
  -- TODO: the shift instructions are also a slightly different format, we accept
  -- that for the time being.
  Slli    :: Opcode 'I
  Srli    :: Opcode 'I
  Srai    :: Opcode 'I
  -- TODO: Fence and Fence_i are both slightly wonky; we might need to separate them
  -- out into separate formats like we did with Ecall and Ebreak. Fence uses the
  -- immediate bits to encode additional operands and Fence_i requires them to be 0,
  -- so ideally we'd capture that in the type. It's still possible to fit them into
  -- the I format for now, but it's actually the case (just like with shifts) only
  -- certain operands are allowed (in the case of Fence.i, all the operands *must* be
  -- 0).
  Fence   :: Opcode 'I
  Fence_i :: Opcode 'I
  Csrrw   :: Opcode 'I
  Csrrs   :: Opcode 'I
  Csrrc   :: Opcode 'I
  Csrrwi  :: Opcode 'I
  Csrrsi  :: Opcode 'I
  Csrrci  :: Opcode 'I

  -- S type
  Sb :: Opcode 'S -- RV32I
  Sh :: Opcode 'S
  Sw :: Opcode 'S

  -- B type
  Beq  :: Opcode 'B -- RV32I
  Bne  :: Opcode 'B
  Blt  :: Opcode 'B
  Bge  :: Opcode 'B
  Bltu :: Opcode 'B
  Bgeu :: Opcode 'B

  -- U type
  Lui   :: Opcode 'U -- RV32I
  Auipc :: Opcode 'U

  -- J type
  Jal :: Opcode 'J -- RV32I

  -- E type
  Ecall   :: Opcode 'E -- RV32I
  Ebreak  :: Opcode 'E

  -- X type (illegal instruction)
  Illegal :: Opcode 'X -- RV32I

-- Instances
$(return [])
deriving instance Show (Opcode k)
instance ShowF Opcode
deriving instance Eq (Opcode k)
instance EqF Opcode where
  eqF = (==)
instance TestEquality Opcode where
  testEquality = $(structuralTypeEquality [t|Opcode|] [])
instance OrdF Opcode where
  compareF = $(structuralTypeOrd [t|Opcode|] [])

----------------------------------------
-- OpBits

-- | Bits fixed by an opcode.
-- Holds all the bits that are fixed by a particular opcode. Each format maps to a
-- potentially different set of bits.
data OpBits :: Format -> * where
  ROpBits :: BitVector 7 -> BitVector 3 -> BitVector 7 -> OpBits 'R
  IOpBits :: BitVector 7 -> BitVector 3                -> OpBits 'I
  SOpBits :: BitVector 7 -> BitVector 3                -> OpBits 'S
  BOpBits :: BitVector 7 -> BitVector 3                -> OpBits 'B
  UOpBits :: BitVector 7                               -> OpBits 'U
  JOpBits :: BitVector 7                               -> OpBits 'J
  EOpBits :: BitVector 7 -> BitVector 25               -> OpBits 'E
  XOpBits ::                                              OpBits 'X

-- Instances
$(return [])
deriving instance Show (OpBits k)
instance ShowF OpBits
deriving instance Eq (OpBits k)
instance EqF OpBits where
  eqF = (==)
instance TestEquality OpBits where
  testEquality = $(structuralTypeEquality [t|OpBits|] [])
instance OrdF OpBits where
  compareF = $(structuralTypeOrd [t|OpBits|] [])

----------------------------------------
-- Instructions

-- | RISC-V Instruction, parameterized by format.
data Instruction (fmt :: Format) = Inst { instOpcode   :: Opcode fmt
                                        , instOperands :: Operands fmt
                                        }

-- Instances
$(return [])
instance Show (Instruction k) where
  show (Inst opcode operands) = show opcode ++ " " ++ show operands
instance ShowF Instruction
instance Eq (Instruction k) where
  Inst opcode operands == Inst opcode' operands' =
    opcode == opcode' && operands == operands'
instance EqF Instruction where
  eqF = (==)
instance TestEquality Instruction where
  (Inst opcode operands) `testEquality` (Inst opcode' operands') =
    case (opcode   `testEquality` opcode',
          operands `testEquality` operands') of
      (Just Refl, Just Refl) -> Just Refl
      _ -> Nothing
instance OrdF Instruction where
  Inst opcode operands `compareF` Inst opcode' operands' =
    case opcode `compareF` opcode' of
      EQF -> operands `compareF` operands'
      cmp -> cmp

-- | Instruction encoding, mapping each opcode to its associated 'OpBits', the bits
-- it fixes in an instruction word.
type EncodeMap = MapF Opcode OpBits

-- | Reverse of 'EncodeMap'
type DecodeMap = MapF OpBits Opcode

type SemanticsMap arch = MapF Opcode (Formula arch)

-- | A set of RISC-V instructions. We use this type to group the various instructions
-- into categories based on extension and register width.
data InstructionSet arch
  = InstructionSet { isEncodeMap    :: EncodeMap
                   , isDecodeMap    :: DecodeMap
                   , isSemanticsMap :: SemanticsMap arch
                   }

instance Monoid (InstructionSet arch) where
  -- RV32 is the default/minimum, so that should be mempty.
  mempty = InstructionSet Map.empty Map.empty Map.empty


  InstructionSet em1 dm1 sm1 `mappend` InstructionSet em2 dm2 sm2
    = InstructionSet (em1 `Map.union` em2) (dm1 `Map.union` dm2) (sm1 `Map.union` sm2)

-- | Construction an instructionSet from only an EncodeMap
instructionSet :: EncodeMap -> SemanticsMap arch -> InstructionSet arch
instructionSet em sm = InstructionSet em (transMap em) sm
  where swap :: Pair (k :: t -> *) (v :: t -> *) -> Pair v k
        swap (Pair k v) = Pair v k
        transMap :: OrdF v => MapF (k :: t -> *) (v :: t -> *) -> MapF v k
        transMap = Map.fromList . map swap . Map.toList

-- | Given an instruction set, obtain the fixed bits of an opcode (encoding)
opBitsFromOpcode :: InstructionSet arch -> Opcode fmt -> OpBits fmt
opBitsFromOpcode is opcode = case Map.lookup opcode (isEncodeMap is) of
  Just opBits -> opBits
  Nothing     -> error $ "Opcode " ++ show opcode ++
                 " does not have corresponding OpBits defined."

-- | Given an instruction set, obtain the semantics of an opcode
semanticsFromOpcode :: InstructionSet arch -> Opcode fmt -> Formula arch fmt
semanticsFromOpcode is opcode = case Map.lookup opcode (isSemanticsMap is) of
  Just formula -> formula
  Nothing      -> error $ "Opcode " ++ show opcode ++
                  " does not have corresponding semantics defined."

-- | Given an instruction set, obtain the opcode from its fixed bits (decoding)
opcodeFromOpBits :: InstructionSet arch -> OpBits fmt -> Either (Opcode 'X) (Opcode fmt)
opcodeFromOpBits is opBits =
  maybe (Left Illegal) Right (Map.lookup opBits (isDecodeMap is))

-- | Type for instruction words. These can be any multiple of 16.
type InstWord (n :: Nat) = BitVector (16 * n)

----------------------------------------


class (Monad m) =>  MState m arch | m -> arch where
  getPC  :: m (BitVector (ArchWidth arch))
  getReg :: BitVector 5 -> m (BitVector (ArchWidth arch))
  getMem :: NatRepr bytes
         -> BitVector (ArchWidth arch)
         -> m (BitVector (8*bytes))

  setPC :: BitVector (ArchWidth arch) -> m ()
  setReg :: BitVector 5 -> BitVector (ArchWidth arch) -> m ()
  setMem :: NatRepr bytes
         -> BitVector (ArchWidth arch)
         -> BitVector (8*bytes)
         -> m ()

evalParam :: MState m arch
          => OperandParam arch oid
          -> Operands fmt
          -> m (BitVector (OperandIDWidth oid))
evalParam (OperandParam RdRepr)    (ROperands  rd   _   _) = return rd
evalParam (OperandParam Rs1Repr)   (ROperands   _ rs1   _) = return rs1
evalParam (OperandParam Rs2Repr)   (ROperands   _   _ rs2) = return rs2
evalParam (OperandParam RdRepr)    (IOperands  rd   _   _) = return rd
evalParam (OperandParam Rs1Repr)   (IOperands   _ rs1   _) = return rs1
evalParam (OperandParam Imm12Repr) (IOperands   _   _ imm) = return imm
evalParam (OperandParam Rs1Repr)   (SOperands rs1   _   _) = return rs1
evalParam (OperandParam Rs2Repr)   (SOperands   _ rs2   _) = return rs2
evalParam (OperandParam Imm12Repr) (SOperands   _   _ imm) = return imm
evalParam (OperandParam Rs1Repr)   (BOperands rs1   _   _) = return rs1
evalParam (OperandParam Rs2Repr)   (BOperands   _ rs2   _) = return rs2
evalParam (OperandParam Imm12Repr) (BOperands   _   _ imm) = return imm
evalParam (OperandParam RdRepr)    (UOperands  rd       _) = return rd
evalParam (OperandParam Imm20Repr) (UOperands   _     imm) = return imm
evalParam (OperandParam RdRepr)    (JOperands  rd       _) = return rd
evalParam (OperandParam Imm20Repr) (JOperands   _     imm) = return imm
evalParam (OperandParam Imm32Repr) (XOperands         imm) = return imm
evalParam oidRepr operands = error $
  "No operand " ++ show oidRepr ++ " in operands " ++ show operands

evalExpr :: (MState m arch, KnownNat (ArchWidth arch))
         => BVExpr arch w
         -> Operands fmt
         -> Integer
         -> m (BitVector w)
evalExpr (LitBV bv) _ _ = return bv
evalExpr (ParamBV p) operands _ = evalParam p operands
evalExpr PCRead _ _ = getPC
evalExpr InstBytes _ ib = return $ bitVector ib
evalExpr (RegRead ridE) operands ib =
  evalExpr ridE operands ib >>= getReg
evalExpr (MemRead bRepr addrE) operands ib =
  evalExpr addrE operands ib >>= getMem bRepr
evalExpr (AndE e1 e2) operands ib = do
  e1Val <- evalExpr e1 operands ib
  e2Val <- evalExpr e2 operands ib
  return $ e1Val `bvAnd` e2Val
evalExpr (OrE e1 e2) operands ib = do
  e1Val <- evalExpr e1 operands ib
  e2Val <- evalExpr e2 operands ib
  return $ e1Val `bvOr` e2Val
evalExpr (XorE e1 e2) operands ib = do
  e1Val <- evalExpr e1 operands ib
  e2Val <- evalExpr e2 operands ib
  return $ e1Val `bvXor` e2Val
evalExpr (NotE e) operands ib = do
  evalExpr e operands ib >>= return . bvComplement
evalExpr (AddE e1 e2) operands ib = do
  e1Val <- evalExpr e1 operands ib
  e2Val <- evalExpr e2 operands ib
  return $ e1Val `bvAdd` e2Val
evalExpr (SubE e1 e2) operands ib = do
  e1Val <- evalExpr e1 operands ib
  e2Val <- evalExpr e2 operands ib
  return $ e1Val `bvAdd` (bvNegate e2Val)
-- TODO: throw some kind of exception if the shifter operand is larger than the
-- architecture width.
evalExpr (SllE e1 e2) operands ib = do
  e1Val <- evalExpr e1 operands ib
  e2Val <- evalExpr e2 operands ib
  return $ e1Val `bvShiftL` fromIntegral (bvIntegerU e2Val)
evalExpr (SrlE e1 e2) operands ib = do
  e1Val <- evalExpr e1 operands ib
  e2Val <- evalExpr e2 operands ib
  return $ e1Val `bvShiftRL` fromIntegral (bvIntegerU e2Val)
evalExpr (SraE e1 e2) operands ib = do
  e1Val <- evalExpr e1 operands ib
  e2Val <- evalExpr e2 operands ib
  return $ e1Val `bvShiftRA` fromIntegral (bvIntegerU e2Val)
evalExpr (EqE e1 e2) operands ib = do
  e1Val <- evalExpr e1 operands ib
  e2Val <- evalExpr e2 operands ib
  return $ fromBool (e1Val == e2Val)
evalExpr (LtuE e1 e2) operands ib = do
  e1Val <- evalExpr e1 operands ib
  e2Val <- evalExpr e2 operands ib
  return $ fromBool (e1Val `bvLTU` e2Val)
evalExpr (LtsE e1 e2) operands ib = do
  e1Val <- evalExpr e1 operands ib
  e2Val <- evalExpr e2 operands ib
  return $ fromBool (e1Val `bvLTS` e2Val)
evalExpr (ZExtE wRepr e) operands ib =
  evalExpr e operands ib >>= return . bvZextWithRepr wRepr
evalExpr (SExtE wRepr e) operands ib =
  evalExpr e operands ib >>= return . bvSextWithRepr wRepr
evalExpr (ExtractE wRepr base e) operands ib =
  evalExpr e operands ib >>= return . bvExtractWithRepr wRepr base
evalExpr (IteE testE tE fE) operands ib = do
  testVal <- evalExpr testE operands ib
  tVal <- evalExpr tE operands ib
  fVal <- evalExpr fE operands ib
  return $ if (testVal == 1) then tVal else fVal

-- stepMState :: MState m arch
--            => Formula arch fmt
--            -> Operands fmt
--            -> m ()
-- stepMState = undefined





----------------------------------------
-- TODO: Add compressed instructions
--
-- This task is a little different from just adding an extension. In fact, it's
-- really orthogonal to it, because we aren't adding *any* new instructions! Instead,
-- we define a separate opcodeOpBits mapping for those special instructions. Since
-- those mappings will under-specify the instruction word in a sense, we will need to
-- encode a predicate that determines whether a particular instruction instance is
-- compressable or not. The encode function will have to take some kind of flag, and
-- check both the flag and the compressability of the instruction before determining
-- how to encode it.
--
-- Another approach would be to use bit lenses to, instead of creating lenses mapping
-- the operands into a 16-bit instruction word, map the 16-bit instruction word into
-- the 32-bit one. Then we could create a separate function, maybe called compress,
-- that would compress those instructions that could be compressed. This might be a
-- slicker approach. We'd still need the compressibility predicate; I'm not sure if
-- there is a clever way to weave that predicate in with the BitLayout stuff. Have to
-- think about it. My feeling is that it makes more sense to just define one
-- "compressible" function and see if that looks adequate.
--
-- Do I want the decoder to know whether it is "allowed" to decode compressed
-- instructions, or any extension for that matter? My current feeling is that we
-- might as well not bother making a distinction between the various extensions in
-- the encoding/decoding end of things if we can help it. It would be nice to somehow
-- tag instructions with the extensions they belong to, though, if only for error
-- reporting purposes.
--
-- On the decoding end of things, it's a little trickier. Right now the decoder takes
-- a 32-bit word. I'm thinking maybe I should have a separate
--
--   decodeC :: BitVector 16 -> Maybe (BitVector 32)
--
-- which returns the full 32-bit word that the compressed instruction would expand
-- to. This is assuming I use the bit lens approach rather than creating an entirely
-- separate mapping for compressed instructions into the Instruction type.
--
-- Now that I'm really thinking about it, I believe the above is exactly the right
-- approach. It captures the fact that these compressed guys are being embedded into
-- a larger instruction word.
