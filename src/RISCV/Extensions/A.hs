{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-|
Module      : RISCV.Extensions.A
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

RV32A/RV64A, memory atomics extension
-}

module RISCV.Extensions.A
  ( a32
  , a64
  ) where

import Data.BitVector.Sized.App
import Data.Monoid
import qualified Data.Parameterized.Map as Map
import Data.Parameterized
import Data.Parameterized.List

import RISCV.Extensions.Helpers
import RISCV.InstructionSet
import RISCV.Semantics
import RISCV.Semantics.Exceptions
import RISCV.Types

-- | A extension (RV32)
a32 :: (KnownArch arch, AExt << exts) => InstructionSet arch exts
a32 = instructionSet aEncode aSemantics

-- | A extension (RV64)
a64 :: (KnownArch arch, 64 <= ArchWidth arch, AExt << exts) => InstructionSet arch exts
a64 = a32 <> instructionSet a64Encode a64Semantics

aEncode :: AExt << exts => EncodeMap arch exts
aEncode = Map.fromList
  [ Pair Lrw      (OpBits ARepr (0b0101111 :< 0b010 :< 0b00010 :< Nil))
  , Pair Scw      (OpBits ARepr (0b0101111 :< 0b010 :< 0b00011 :< Nil))
  , Pair Amoswapw (OpBits ARepr (0b0101111 :< 0b010 :< 0b00001 :< Nil))
  , Pair Amoaddw  (OpBits ARepr (0b0101111 :< 0b010 :< 0b00000 :< Nil))
  , Pair Amoxorw  (OpBits ARepr (0b0101111 :< 0b010 :< 0b00100 :< Nil))
  , Pair Amoandw  (OpBits ARepr (0b0101111 :< 0b010 :< 0b01100 :< Nil))
  , Pair Amoorw   (OpBits ARepr (0b0101111 :< 0b010 :< 0b01000 :< Nil))
  , Pair Amominw  (OpBits ARepr (0b0101111 :< 0b010 :< 0b10000 :< Nil))
  , Pair Amomaxw  (OpBits ARepr (0b0101111 :< 0b010 :< 0b10100 :< Nil))
  , Pair Amominuw (OpBits ARepr (0b0101111 :< 0b010 :< 0b11000 :< Nil))
  , Pair Amomaxuw (OpBits ARepr (0b0101111 :< 0b010 :< 0b11100 :< Nil))
  ]

a64Encode :: (64 <= ArchWidth arch, AExt << exts) => EncodeMap arch exts
a64Encode = Map.fromList
  [ Pair Lrd      (OpBits ARepr (0b0101111 :< 0b011 :< 0b00010 :< Nil))
  , Pair Scd      (OpBits ARepr (0b0101111 :< 0b011 :< 0b00011 :< Nil))
  , Pair Amoswapd (OpBits ARepr (0b0101111 :< 0b011 :< 0b00001 :< Nil))
  , Pair Amoaddd  (OpBits ARepr (0b0101111 :< 0b011 :< 0b00000 :< Nil))
  , Pair Amoxord  (OpBits ARepr (0b0101111 :< 0b011 :< 0b00100 :< Nil))
  , Pair Amoandd  (OpBits ARepr (0b0101111 :< 0b011 :< 0b01100 :< Nil))
  , Pair Amoord   (OpBits ARepr (0b0101111 :< 0b011 :< 0b01000 :< Nil))
  , Pair Amomind  (OpBits ARepr (0b0101111 :< 0b011 :< 0b10000 :< Nil))
  , Pair Amomaxd  (OpBits ARepr (0b0101111 :< 0b011 :< 0b10100 :< Nil))
  , Pair Amominud (OpBits ARepr (0b0101111 :< 0b011 :< 0b11000 :< Nil))
  , Pair Amomaxud (OpBits ARepr (0b0101111 :< 0b011 :< 0b11100 :< Nil))
  ]

aSemantics :: forall arch exts . (KnownArch arch, AExt << exts) => SemanticsMap arch exts
aSemantics = Map.fromList
  [ Pair Lrw $ InstFormula $ getFormula $ do
      comment "Loads the four bytes from memory at address x[rs1]."
      comment "Writes them to x[rd], sign-extending the result."
      comment "Registers a reservation on that memory word."

      rd :< rs1 :< rs2 :< _rl :< _aq :< Nil <- operandEs

      -- Check that rs2 is zero
      let illegal = notE (rs2 `eqE` litBV 0)

      let x_rs1 = readReg rs1
      let mVal  = readMem (knownNat @4) x_rs1

      branch illegal
        $> raiseException IllegalInstruction
        $> do assignReg rd (sextE mVal)
              reserve x_rs1
              incrPC

  , Pair Scw $ InstFormula $ getFormula $ do
      comment "Checks that there exists a load reservation on address x[rs1]."
      comment "If so, stores the four bytes in register x[rs2] at that address."
      comment "Writes 0 to x[rd] if the store succeeded, or a nonzero error code otherwise."

      rd :< rs1 :< rs2 :< _rl :< _aq :< Nil <- operandEs

      let x_rs1 = readReg rs1
      let x_rs2 = readReg rs2

      let reserved = checkReserved x_rs1

      branch reserved
        $> do assignMem (knownNat @4) x_rs1 (extractE 0 x_rs2)
              assignReg rd (litBV 0)
              incrPC
        $> do assignReg rd (litBV 1) -- TODO: this could be any nonzero value.
              incrPC
  , Pair Amoswapw $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to x[rs2]. Set x[rd] to the sign extension of t."

      amoOp32 const
  , Pair Amoaddw $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t + x[rs2]. Set x[rd] to the sign extension of t."

      amoOp32 addE
  , Pair Amoxorw $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t ^ x[rs2]. Set x[rd] to the sign extension of t."

      amoOp32 xorE
  , Pair Amoandw $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t & x[rs2]. Set x[rd] to the sign extension of t."

      amoOp32 andE
  , Pair Amoorw $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t | x[rs2]. Set x[rd] to the sign extension of t."

      amoOp32 orE
  , Pair Amominw $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to min_s(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp32 $ \e1 e2 -> iteE (e1 `ltsE` e2) e1 e2
  , Pair Amomaxw $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to max_s(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp32 $ \e1 e2 -> iteE (e1 `ltsE` e2) e2 e1
  , Pair Amominuw $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to min_u(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp32 $ \e1 e2 -> iteE (e1 `ltuE` e2) e1 e2
  , Pair Amomaxuw $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to max_u(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp32 $ \e1 e2 -> iteE (e1 `ltuE` e2) e2 e1
  ]

amoOp32 :: KnownArch arch
        => (InstExpr A arch 32 -> InstExpr A arch 32 -> InstExpr A arch 32)
        -> FormulaBuilder (InstExpr A arch) arch ()
amoOp32 op = do
      rd :< rs1 :< rs2 :< _rl :< _aq :< Nil <- operandEs

      let x_rs1 = readReg rs1
      let x_rs2 = readReg rs2
      let mVal  = readMem (knownNat @4) x_rs1

      assignMem (knownNat @4) x_rs1 (extractE 0 x_rs2 `op` mVal)
      assignReg rd (sextE mVal)

      incrPC


a64Semantics :: forall arch exts . (KnownArch arch, 64 <= ArchWidth arch, AExt << exts) => SemanticsMap arch exts
a64Semantics = Map.fromList
  [ Pair Lrd $ InstFormula $ getFormula $ do
      comment "Loads the eight bytes from memory at address x[rs1]."
      comment "Writes them to x[rd], sign-extending the result."
      comment "Registers a reservation on that memory word."

      rd :< rs1 :< rs2 :< _rl :< _aq :< Nil <- operandEs

      -- Check that rs2 is zero
      let illegal = notE (rs2 `eqE` litBV 0)

      let x_rs1 = readReg rs1
      let mVal  = readMem (knownNat @8) x_rs1

      branch illegal
        $> raiseException IllegalInstruction
        $> do assignReg rd (sextE mVal)
              reserve x_rs1

  , Pair Scd $ InstFormula $ getFormula $ do
      comment "Checks that there exists a load reservation on address x[rs1]."
      comment "If so, stores the eight bytes in register x[rs2] at that address."
      comment "Writes 0 to x[rd] if the store succeeded, or a nonzero error code otherwise."

      rd :< rs1 :< rs2 :< _rl :< _aq :< Nil <- operandEs

      let x_rs1 = readReg rs1
      let x_rs2 = readReg rs2

      let reserved = checkReserved x_rs1

      branch reserved
        $> do assignMem (knownNat @8) x_rs1 (extractE 0 x_rs2)
              assignReg rd (litBV 0)
        $> assignReg rd (litBV 1) -- TODO: this could be any nonzero value.
  , Pair Amoswapd $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to x[rs2]. Set x[rd] to the sign extension of t."

      amoOp64 const
  , Pair Amoaddd $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t + x[rs2]. Set x[rd] to the sign extension of t."

      amoOp64 addE
  , Pair Amoxord $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t ^ x[rs2]. Set x[rd] to the sign extension of t."

      amoOp64 xorE
  , Pair Amoandd $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t & x[rs2]. Set x[rd] to the sign extension of t."

      amoOp64 andE
  , Pair Amoord $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t | x[rs2]. Set x[rd] to the sign extension of t."

      amoOp64 orE
  , Pair Amomind $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to min_s(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp64 $ \e1 e2 -> iteE (e1 `ltsE` e2) e1 e2
  , Pair Amomaxd $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to max_s(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp64 $ \e1 e2 -> iteE (e1 `ltsE` e2) e2 e1
  , Pair Amominud $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to min_u(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp64 $ \e1 e2 -> iteE (e1 `ltuE` e2) e1 e2
  , Pair Amomaxud $ InstFormula $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to max_u(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp64 $ \e1 e2 -> iteE (e1 `ltuE` e2) e2 e1
  ]

amoOp64 :: KnownArch arch
        => (InstExpr A arch 64 -> InstExpr A arch 64 -> InstExpr A arch 64)
        -> FormulaBuilder (InstExpr A arch) arch ()
amoOp64 op = do
      rd :< rs1 :< rs2 :< _rl :< _aq :< Nil <- operandEs

      let x_rs1 = readReg rs1
      let x_rs2 = readReg rs2
      let mVal  = readMem (knownNat @8) x_rs1

      assignMem (knownNat @8) x_rs1 (extractE 0 x_rs2 `op` mVal)
      assignReg rd (sextE mVal)

      incrPC
