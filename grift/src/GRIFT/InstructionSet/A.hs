{-
This file is part of GRIFT (Galois RISC-V ISA Formal Tools).

GRIFT is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GRIFT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero Public License for more details.

You should have received a copy of the GNU Affero Public License
along with GRIFT.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# Language BinaryLiterals   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-|
Module      : GRIFT.InstructionSet.A
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

RV32A/RV64A, memory atomics extension
-}

module GRIFT.InstructionSet.A
  ( aFromRepr
  ) where

import Data.BitVector.Sized.App
import qualified Data.Parameterized.Map as Map
import Data.Parameterized
import Data.Parameterized.List

import GRIFT.InstructionSet
import GRIFT.Semantics
import GRIFT.Semantics.Utils
import GRIFT.Types

-- | Get the A instruction set from an explicit 'RVRepr'.
aFromRepr :: RVRepr rv -> InstructionSet rv
aFromRepr rv@(RVRepr RV32Repr (ExtensionsRepr _ _ AYesRepr _ _)) = withRV rv a32
aFromRepr rv@(RVRepr RV64Repr (ExtensionsRepr _ _ AYesRepr _ _)) = withRV rv a64
aFromRepr _ = mempty

-- | A extension (RV32)
a32 :: (KnownRV rv, AExt << rv) => InstructionSet rv
a32 = instructionSet aEncode aSemantics

-- | A extension (RV64)
a64 :: (KnownRV rv, 64 <= RVWidth rv, AExt << rv) => InstructionSet rv
a64 = a32 <> instructionSet a64Encode a64Semantics

aEncode :: AExt << rv => EncodeMap rv
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

a64Encode :: (64 <= RVWidth rv, AExt << rv) => EncodeMap rv
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

aSemantics :: forall rv . (KnownRV rv, AExt << rv) => SemanticsMap rv
aSemantics = Map.fromList
  [ Pair Lrw $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Loads the four bytes from memory at address x[rs1]."
      comment "Writes them to x[rd], sign-extending the result."
      comment "Registers a reservation on that memory word."

      rd :< rs1 :< rs2 :< _rl :< _aq :< Nil <- operandEs

      -- Check that rs2 is zero
      let illegal = notE (rs2 `eqE` litBV 0)

      let x_rs1 = readGPR rs1
      let mVal  = readMem (knownNat @4) x_rs1

      branch illegal
        $> do iw <- instWord
              raiseException IllegalInstruction iw
        $> do assignGPR rd (sextE mVal)
              reserve x_rs1
              incrPC

  , Pair Scw $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Checks that there exists a load reservation on address x[rs1]."
      comment "If so, stores the four bytes in register x[rs2] at that address."
      comment "Writes 0 to x[rd] if the store succeeded, or a nonzero error code otherwise."

      rd :< rs1 :< rs2 :< _rl :< _aq :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      let reserved = checkReserved x_rs1

      branch reserved
        $> do assignMem (knownNat @4) x_rs1 (extractE 0 x_rs2)
              assignGPR rd (litBV 0)
              incrPC
        $> do assignGPR rd (litBV 1) -- TODO: this could be any nonzero value.
              incrPC
  , Pair Amoswapw $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to x[rs2]. Set x[rd] to the sign extension of t."

      amoOp32 const
  , Pair Amoaddw $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t + x[rs2]. Set x[rd] to the sign extension of t."

      amoOp32 addE
  , Pair Amoxorw $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t ^ x[rs2]. Set x[rd] to the sign extension of t."

      amoOp32 xorE
  , Pair Amoandw $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t & x[rs2]. Set x[rd] to the sign extension of t."

      amoOp32 andE
  , Pair Amoorw $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t | x[rs2]. Set x[rd] to the sign extension of t."

      amoOp32 orE
  , Pair Amominw $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to min_s(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp32 $ \e1 e2 -> iteE (e1 `ltsE` e2) e1 e2
  , Pair Amomaxw $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to max_s(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp32 $ \e1 e2 -> iteE (e1 `ltsE` e2) e2 e1
  , Pair Amominuw $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to min_u(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp32 $ \e1 e2 -> iteE (e1 `ltuE` e2) e1 e2
  , Pair Amomaxuw $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to max_u(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp32 $ \e1 e2 -> iteE (e1 `ltuE` e2) e2 e1
  ]

amoOp32 :: KnownRV rv
        => (InstExpr A rv 32 -> InstExpr A rv 32 -> InstExpr A rv 32)
        -> SemanticsM (InstExpr A) rv ()
amoOp32 op = do
      rd :< rs1 :< rs2 :< _rl :< _aq :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let mVal  = readMem (knownNat @4) x_rs1

      assignMem (knownNat @4) x_rs1 (extractE 0 x_rs2 `op` mVal)
      assignGPR rd (sextE mVal)

      incrPC


a64Semantics :: forall rv . (KnownRV rv, 64 <= RVWidth rv, AExt << rv) => SemanticsMap rv
a64Semantics = Map.fromList
  [ Pair Lrd $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Loads the eight bytes from memory at address x[rs1]."
      comment "Writes them to x[rd], sign-extending the result."
      comment "Registers a reservation on that memory word."

      rd :< rs1 :< rs2 :< _rl :< _aq :< Nil <- operandEs

      -- Check that rs2 is zero
      let illegal = notE (rs2 `eqE` litBV 0)

      let x_rs1 = readGPR rs1
      let mVal  = readMem (knownNat @8) x_rs1

      branch illegal
        $> do iw <- instWord
              raiseException IllegalInstruction iw
        $> do assignGPR rd (sextE mVal)
              reserve x_rs1

  , Pair Scd $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Checks that there exists a load reservation on address x[rs1]."
      comment "If so, stores the eight bytes in register x[rs2] at that address."
      comment "Writes 0 to x[rd] if the store succeeded, or a nonzero error code otherwise."

      rd :< rs1 :< rs2 :< _rl :< _aq :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      let reserved = checkReserved x_rs1

      branch reserved
        $> do assignMem (knownNat @8) x_rs1 (extractE 0 x_rs2)
              assignGPR rd (litBV 0)
        $> assignGPR rd (litBV 1) -- TODO: this could be any nonzero value.
  , Pair Amoswapd $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to x[rs2]. Set x[rd] to the sign extension of t."

      amoOp64 const
  , Pair Amoaddd $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t + x[rs2]. Set x[rd] to the sign extension of t."

      amoOp64 addE
  , Pair Amoxord $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t ^ x[rs2]. Set x[rd] to the sign extension of t."

      amoOp64 xorE
  , Pair Amoandd $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t & x[rs2]. Set x[rd] to the sign extension of t."

      amoOp64 andE
  , Pair Amoord $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to t | x[rs2]. Set x[rd] to the sign extension of t."

      amoOp64 orE
  , Pair Amomind $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to min_s(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp64 $ \e1 e2 -> iteE (e1 `ltsE` e2) e1 e2
  , Pair Amomaxd $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to max_s(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp64 $ \e1 e2 -> iteE (e1 `ltsE` e2) e2 e1
  , Pair Amominud $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to min_u(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp64 $ \e1 e2 -> iteE (e1 `ltuE` e2) e1 e2
  , Pair Amomaxud $ instSemantics (Rd :< Rs1 :< Rs2 :< Rl :< Aq :< Nil) $ do
      comment "Atomically, let t be the value of the memory word at address x[rs1]."
      comment "Set that memory word to max_u(t, x[rs2]). Set x[rd] to the sign extension of t."

      amoOp64 $ \e1 e2 -> iteE (e1 `ltuE` e2) e2 e1
  ]

amoOp64 :: KnownRV rv
        => (InstExpr A rv 64 -> InstExpr A rv 64 -> InstExpr A rv 64)
        -> SemanticsM (InstExpr A) rv ()
amoOp64 op = do
      rd :< rs1 :< rs2 :< _rl :< _aq :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let mVal  = readMem (knownNat @8) x_rs1

      assignMem (knownNat @8) x_rs1 (extractE 0 x_rs2 `op` mVal)
      assignGPR rd (sextE mVal)

      incrPC
