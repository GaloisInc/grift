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

A, memory atomics extension
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

aEncode :: EncodeMap arch
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

a64Encode :: 64 <= ArchWidth arch => EncodeMap arch
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

aSemantics :: forall arch . KnownArch arch => SemanticsMap arch
aSemantics = Map.fromList
  [ Pair Lrw $ getFormula $ do
      comment "Loads the four bytes from memory at address x[rs1]."
      comment "Writes them to x[rd], sign-extending the result."
      comment "Registers a reservation on that memory word."

      rd :< rs1 :< rs2 :< _rl :< _aq :< Nil <- operandEs

      -- Check that rs2 is zero
      let illegal = notE (rs2 `eqE` litBV 0)

      x_rs1 <- readReg rs1
      mVal  <- readMem32 x_rs1

      branch illegal
        $> raiseException IllegalInstruction
        $> do assignReg rd (sextE mVal)
              reserve x_rs1

  , Pair Scw $ getFormula $ do
      comment "Checks that there exists a load reservation on address x[rs1]."
      comment "If so, stores the four bytes in register x[rs2] at that address."
      comment "Writes 0 to x[rd] if the store succeeded, or a nonzero error code otherwise."

      rd :< rs1 :< rs2 :< _rl :< _aq :< Nil <- operandEs

      x_rs1 <- readReg rs1
      x_rs2 <- readReg rs2

      reserved <- checkReserved x_rs1

      branch reserved
        $> do assignMem32 x_rs1 (extractE 0 x_rs2)
              assignReg rd (litBV 0)
        $> assignReg rd (litBV 1) -- TODO: this could be any nonzero value.
  , Pair Amoswapw $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to x[rs2]. Set x[rd] to the sign extension of t."

      amoOp32 const
  , Pair Amoaddw $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t + x[rs2]. Set x[rd] to the sign extension of t."

      amoOp32 addE
  , Pair Amoxorw $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t ^ x[rs2]. Set x[rd] to the sign extension of t."

      amoOp32 xorE
  , Pair Amoandw $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t & x[rs2]. Set x[rd] to the sign extension of t."

      amoOp32 andE
  , Pair Amoorw $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t | x[rs2]. Set x[rd] to the sign extension of t."

      amoOp32 orE
  , Pair Amominw $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to min_s(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp32 $ \e1 e2 -> iteE (e1 `ltsE` e2) e1 e2
  , Pair Amomaxw $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to max_s(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp32 $ \e1 e2 -> iteE (e1 `ltsE` e2) e2 e1
  , Pair Amominuw $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to min_u(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp32 $ \e1 e2 -> iteE (e1 `ltuE` e2) e1 e2
  , Pair Amomaxuw $ getFormula $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to max_u(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp32 $ \e1 e2 -> iteE (e1 `ltuE` e2) e2 e1
  ]

amoOp32 :: KnownArch arch
        => (Expr arch A 32 -> Expr arch A 32 -> Expr arch A 32)
        -> FormulaBuilder arch A ()
amoOp32 op = do
      rd :< rs1 :< rs2 :< _rl :< _aq :< Nil <- operandEs

      x_rs1 <- readReg rs1
      x_rs2 <- readReg rs2
      mVal  <- readMem32 x_rs1

      assignMem32 x_rs1 (extractE 0 x_rs2 `op` mVal)
      assignReg rd (sextE mVal)

a64Semantics :: forall arch . (KnownArch arch, 64 <= ArchWidth arch) => SemanticsMap arch
a64Semantics = Map.fromList
  [ Pair Lrd      undefined
  , Pair Scd      undefined
  , Pair Amoswapd undefined
  , Pair Amoaddd  undefined
  , Pair Amoxord  undefined
  , Pair Amoandd  undefined
  , Pair Amoord   undefined
  , Pair Amomind  undefined
  , Pair Amomaxd  undefined
  , Pair Amominud undefined
  , Pair Amomaxud undefined
  ]
