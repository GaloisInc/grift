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

{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-|
Module      : GRIFT.InstructionSet.Base
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

RV32I/RV64I base ISA; encoding and semantics.
-}

module GRIFT.InstructionSet.Base
  ( baseFromRepr
  )
  where

import Data.BitVector.Sized
import Data.BitVector.Sized.App
import qualified Data.Parameterized.Map as Map
import Data.Parameterized
import Data.Parameterized.List

import GRIFT.InstructionSet.Utils
import GRIFT.InstructionSet
import GRIFT.Semantics
import GRIFT.Types

-- | Get the I instruction from an explicit 'RVRepr'.
baseFromRepr :: RVRepr rv -> InstructionSet rv
baseFromRepr rv@(RVRepr RV32Repr _) = withRVCConfig rv $ base32
baseFromRepr rv@(RVRepr RV64Repr _) = withRVCConfig rv $ base64
baseFromRepr (RVRepr RV128Repr _) = error "RV128 not supported"

-- | RV32I/E base instruction set.
base32 :: (KnownRVWidth rv, KnownRVCConfig rv) => InstructionSet rv
base32 = instructionSet baseEncode baseSemantics

-- | RV64I base instruction set.
base64 :: (KnownRVWidth rv, 64 <= RVWidth rv, KnownRVCConfig rv) => InstructionSet rv
base64 = base32 <> instructionSet base64Encode base64Semantics

baseEncode :: EncodeMap rv
baseEncode = Map.fromList
  [ -- RV32I
    -- R type
    Pair Add  (OpBits RRepr (0b0110011 :< 0b000 :< 0b0000000 :< Nil))
  , Pair Sub  (OpBits RRepr (0b0110011 :< 0b000 :< 0b0100000 :< Nil))
  , Pair Sll  (OpBits RRepr (0b0110011 :< 0b001 :< 0b0000000 :< Nil))
  , Pair Slt  (OpBits RRepr (0b0110011 :< 0b010 :< 0b0000000 :< Nil))
  , Pair Sltu (OpBits RRepr (0b0110011 :< 0b011 :< 0b0000000 :< Nil))
  , Pair Xor  (OpBits RRepr (0b0110011 :< 0b100 :< 0b0000000 :< Nil))
  , Pair Srl  (OpBits RRepr (0b0110011 :< 0b101 :< 0b0000000 :< Nil))
  , Pair Sra  (OpBits RRepr (0b0110011 :< 0b101 :< 0b0100000 :< Nil))
  , Pair Or   (OpBits RRepr (0b0110011 :< 0b110 :< 0b0000000 :< Nil))
  , Pair And  (OpBits RRepr (0b0110011 :< 0b111 :< 0b0000000 :< Nil))

  -- I type
  , Pair Jalr   (OpBits IRepr (0b1100111 :< 0b000 :< Nil))
  , Pair Lb     (OpBits IRepr (0b0000011 :< 0b000 :< Nil))
  , Pair Lh     (OpBits IRepr (0b0000011 :< 0b001 :< Nil))
  , Pair Lw     (OpBits IRepr (0b0000011 :< 0b010 :< Nil))
  , Pair Lbu    (OpBits IRepr (0b0000011 :< 0b100 :< Nil))
  , Pair Lhu    (OpBits IRepr (0b0000011 :< 0b101 :< Nil))
  , Pair Addi   (OpBits IRepr (0b0010011 :< 0b000 :< Nil))
  , Pair Slti   (OpBits IRepr (0b0010011 :< 0b010 :< Nil))
  , Pair Sltiu  (OpBits IRepr (0b0010011 :< 0b011 :< Nil))
  , Pair Xori   (OpBits IRepr (0b0010011 :< 0b100 :< Nil))
  , Pair Ori    (OpBits IRepr (0b0010011 :< 0b110 :< Nil))
  , Pair Andi   (OpBits IRepr (0b0010011 :< 0b111 :< Nil))
  , Pair Fence  (OpBits IRepr (0b0001111 :< 0b000 :< Nil))
  , Pair FenceI (OpBits IRepr (0b0001111 :< 0b001 :< Nil))
  , Pair Csrrw  (OpBits IRepr (0b1110011 :< 0b001 :< Nil))
  , Pair Csrrs  (OpBits IRepr (0b1110011 :< 0b010 :< Nil))
  , Pair Csrrc  (OpBits IRepr (0b1110011 :< 0b011 :< Nil))
  , Pair Csrrwi (OpBits IRepr (0b1110011 :< 0b101 :< Nil))
  , Pair Csrrsi (OpBits IRepr (0b1110011 :< 0b110 :< Nil))
  , Pair Csrrci (OpBits IRepr (0b1110011 :< 0b111 :< Nil))

  -- S type
  , Pair Sb (OpBits SRepr (0b0100011 :< 0b000 :< Nil))
  , Pair Sh (OpBits SRepr (0b0100011 :< 0b001 :< Nil))
  , Pair Sw (OpBits SRepr (0b0100011 :< 0b010 :< Nil))

  -- B type
  , Pair Beq  (OpBits BRepr (0b1100011 :< 0b000 :< Nil))
  , Pair Bne  (OpBits BRepr (0b1100011 :< 0b001 :< Nil))
  , Pair Blt  (OpBits BRepr (0b1100011 :< 0b100 :< Nil))
  , Pair Bge  (OpBits BRepr (0b1100011 :< 0b101 :< Nil))
  , Pair Bltu (OpBits BRepr (0b1100011 :< 0b110 :< Nil))
  , Pair Bgeu (OpBits BRepr (0b1100011 :< 0b111 :< Nil))

  -- H type
  , Pair Slli   (OpBits HRepr (0b0010011 :< 0b001 :< 0b00000 :< Nil))
  , Pair Srli   (OpBits HRepr (0b0010011 :< 0b101 :< 0b00000 :< Nil))
  , Pair Srai   (OpBits HRepr (0b0010011 :< 0b101 :< 0b01000 :< Nil))

  -- U type
  , Pair Lui   (OpBits URepr (0b0110111 :< Nil))
  , Pair Auipc (OpBits URepr (0b0010111 :< Nil))

  -- J type
  , Pair Jal (OpBits JRepr (0b1101111 :< Nil))

  -- P type
  , Pair Ecall  (OpBits PRepr (0b00000000000000000000000001110011 :< Nil))
  , Pair Ebreak (OpBits PRepr (0b00000000000100000000000001110011 :< Nil))

  -- X type
  , Pair Illegal (OpBits XRepr Nil)
  ]

baseSemantics :: forall rv . (KnownRVWidth rv, KnownRVCConfig rv) => SemanticsMap rv
baseSemantics = Map.fromList
  [ Pair Add $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Adds register x[rs2] to register x[rs1] and writes the result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let res   = x_rs1 `addE` x_rs2

      assignGPR rd res
      incrPC
  , Pair Sub $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Subtracts register x[rs2] from register x[rs1] and writes the result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let res   = x_rs1 `subE` x_rs2

      assignGPR rd res
      incrPC
  , Pair Sll $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Shifts register x[rs1] left by x[rs2] bit positions."
      comment "The vacated bits are filled with zeros, and the result is written to x[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      archWidth <- getArchWidth

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let res = x_rs1 `sllE` (x_rs2 `andE` litBV (bitVector (natValue archWidth) - 1))

      assignGPR rd res
      incrPC
  , Pair Slt $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Compares x[rs1] and x[rs2] as two's complement numbers."
      comment "Writes 1 to x[rd] if x[rs1] is smaller, or 0 if not."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let res   = zextE (ltsE x_rs1 x_rs2)

      assignGPR rd res
      incrPC
  , Pair Sltu $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Compares x[rs1] and x[rs2] as unsigned numbers."
      comment "Writes 1 to x[rd] if x[rs1] is smaller, or 0 if not."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let res   = zextE (ltuE x_rs1 x_rs2)

      assignGPR rd res
      incrPC
  , Pair Xor $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Computes the bitwise exclusive-OR of registers x[rs1] and x[rs2]."
      comment "Writes the result to x[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let res   = x_rs1 `xorE` x_rs2

      assignGPR rd res
      incrPC
  , Pair Srl $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Shifts register x[rs1] right by x[rs2] bit positions."
      comment "The vacated bits are filled with zeros, and the result is written to x[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      archWidth <- getArchWidth

      let mask = litBV (bitVector (natValue archWidth - 1))
      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let res   = x_rs1 `srlE` (x_rs2 `andE` mask)

      assignGPR rd res
      incrPC
  , Pair Sra $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Shifts register x[rs1] right by x[rs2] bit positions."
      comment "The vacated bits are filled with copies of x[rs1]'s most significant bit."
      comment "The result is written to x[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      archWidth <- getArchWidth

      let mask = litBV (bitVector (natValue archWidth - 1))
      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let res   = x_rs1 `sraE` (x_rs2 `andE` mask)

      assignGPR rd res
      incrPC
  , Pair Or $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Computes the bitwise inclusive-OR of registers x[rs1] and x[rs2]."
      comment "Writes the result to x[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let res   = x_rs1 `orE` x_rs2

      assignGPR rd res
      incrPC
  , Pair And $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Computes the bitwise AND of registers x[rs1] and x[rs2]."
      comment "Writes the result to x[rd]."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let res   = x_rs1 `andE` x_rs2

      assignGPR rd res
      incrPC
  -- I type
  , Pair Jalr $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Sets the pc to x[rs1] + sext(offset)."
      comment "Masks off the least significant bit of the computed address."
      comment "Writes the previous pc+4 to x[rd]."

      rd :< rs1 :< offset :< Nil <- operandEs
      let pc = readPC
      ib <- instBytes
      let x_rs1 = readGPR rs1

      assignGPR rd $ pc `addE` zextE ib
      jump $ (x_rs1 `addE` sextE offset) `andE` notE (litBV 1)
  , Pair Lb $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Loads a byte from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], sign-extending the result."

      rd :< rs1 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let addr  = x_rs1 `addE` sextE offset
      let mVal  = readMem (knownNat @1) addr

      assignGPR rd (sextE mVal)
      incrPC
  , Pair Lh $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Loads a half-word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], sign-extending the result."

      rd :< rs1 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let addr  = x_rs1 `addE` sextE offset
      let mVal  = readMem (knownNat @2) addr

      assignGPR rd (sextE mVal)
      incrPC
  , Pair Lw $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Loads a word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], sign-extending the result."

      rd :< rs1 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let addr  = x_rs1 `addE` sextE offset
      let mVal  = readMem (knownNat @4) addr

      assignGPR rd (sextE mVal)
      incrPC
  , Pair Lbu $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Loads a byte from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], zero-extending the result."

      rd :< rs1 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let addr  = x_rs1 `addE` sextE offset
      let mVal  = readMem (knownNat @1) addr

      assignGPR rd (zextE mVal)
      incrPC
  , Pair Lhu $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Loads a half-word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], zero-extending the result."

      rd :< rs1 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let addr = x_rs1 `addE` sextE offset
      let mVal  = readMem (knownNat @2) addr

      assignGPR rd (zextE mVal)
      incrPC
  , Pair Addi $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Adds the sign-extended immediate to register x[rs1] and writes the result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rd :< rs1 :< imm12 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let res   = x_rs1 `addE` (sextE imm12)

      assignGPR rd res
      incrPC
  , Pair Slti $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Compares x[rs1] and the sign-extended immediate as two's complement numbers."
      comment "Writes 1 to x[rd] if x[rs1] is smaller, 0 if not."

      rd :< rs1 :< imm12 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let res   = zextE (ltsE x_rs1 (sextE imm12))

      assignGPR rd res
      incrPC
  , Pair Sltiu $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Compares x[rs1] and the sign-extended immediate as unsigned numbers."
      comment "Writes 1 to x[rd] if x[rs1] is smaller, 0 if not."

      rd :< rs1 :< imm12 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let res   = zextE (ltuE x_rs1 (sextE imm12))

      assignGPR rd res
      incrPC
  , Pair Xori $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Computes the bitwise exclusive-OR of the sign-extended immediate and register x[rs1]."
      comment "Writes the result to x[rd]."

      rd :< rs1 :< imm12 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let res   = x_rs1 `xorE` (sextE imm12)

      assignGPR rd res
      incrPC
  , Pair Ori $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Computes the bitwise inclusive-OR of the sign-extended immediate and register x[rs1]."
      comment "Writes the result to x[rd]."

      rd :< rs1 :< imm12 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let res   = x_rs1 `orE` (sextE imm12)

      assignGPR rd res
      incrPC
  , Pair Andi $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Computes the bitwise AND of the sign-extended immediate and register x[rs1]."
      comment "Writes the result to x[rd]."

      rd :< rs1 :< imm12 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let res   = x_rs1 `andE` (sextE imm12)

      assignGPR rd res
      incrPC
  , Pair Slli $ instSemantics (Rd :< Rs1 :< Shamt7 :< Nil) $ do
      comment "Shifts register x[rs1] left by shamt bit positions."
      comment "The vacated bits are filled with zeros, and the result is written to x[rd]."

      rd :< rs1 :< shamt :< Nil <- operandEs

      let x_rs1 = readGPR rs1

      archWidth <- getArchWidth
      let shiftBound = litBV (bitVector (natValue archWidth) :: BitVector 7)
      let shamtBad = notE (shamt `ltuE` shiftBound)

      -- Check that the control bits are all zero.

      branch shamtBad
        $> do iw <- instWord
              raiseException IllegalInstruction iw
        $> do assignGPR rd $ x_rs1 `sllE` zextE shamt
              incrPC
  , Pair Srli $ instSemantics (Rd :< Rs1 :< Shamt7 :< Nil) $ do
      comment "Shifts register x[rs1] left by shamt bit positions."
      comment "The vacated bits are filled with zeros, and the result is written to x[rd]."

      rd :< rs1 :< shamt :< Nil <- operandEs

      let x_rs1 = readGPR rs1

      archWidth <- getArchWidth
      let shiftBound = litBV (bitVector (natValue archWidth) :: BitVector 7)
      let shamtBad = notE (shamt `ltuE` shiftBound)

      -- Check that the control bits are all zero.

      branch shamtBad
        $> do iw <- instWord
              raiseException IllegalInstruction iw
        $> do assignGPR rd $ x_rs1 `srlE` zextE shamt
              incrPC
  , Pair Srai $ instSemantics (Rd :< Rs1 :< Shamt7 :< Nil) $ do
      comment "Shifts register x[rs1] left by shamt bit positions."
      comment "The vacated bits are filled with zeros, and the result is written to x[rd]."

      rd :< rs1 :< shamt :< Nil <- operandEs

      let x_rs1 = readGPR rs1

      archWidth <- getArchWidth
      let shiftBound = litBV (bitVector (natValue archWidth) :: BitVector 7)
      let shamtBad = notE (shamt `ltuE` shiftBound)

      -- Check that the control bits are all zero.

      branch shamtBad
        $> do iw <- instWord
              raiseException IllegalInstruction iw
        $> do assignGPR rd $ x_rs1 `sraE` zextE shamt
              incrPC

  , Pair Ecall $ instSemantics Nil $ do
      comment "Makes a request of the execution environment."

      raiseException EnvironmentCall (litBV 0x0)

  , Pair Ebreak $ instSemantics Nil $ do
      comment "Makes a request to the debugger."

      raiseException Breakpoint (litBV 0x0)

  -- TODO: Fence instructions.
  , Pair Fence $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Fence. Currently a no-op."

      incrPC
  , Pair FenceI $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "FenceI. Currently a no-op."

      incrPC
  , Pair Csrrw $ instSemantics (Rd :< Rs1 :< Csr :< Nil) $ do
      comment "Let t be the value of control and status register csr."
      comment "Copy x[rs1] to the csr, then write t to x[rd]."

      rd :< rs1 :< csr :< Nil <- operandEs

      let t = readCSR csr
      let x_rs1 = readGPR rs1

      checkCSR (litBV 0b0 `ltuE` rs1) csr $ do
        writeCSR csr x_rs1
        assignGPR rd t
        incrPC
  , Pair Csrrs $ instSemantics (Rd :< Rs1 :< Csr :< Nil) $ do
      comment "Let t be the value of control and status register csr."
      comment "Write the bitwise OR of t and x[rs1] to the csr, then write t to x[rd]."

      rd :< rs1 :< csr :< Nil <- operandEs

      let t = readCSR csr
      let x_rs1 = readGPR rs1

      checkCSR (litBV 0b0 `ltuE` rs1) csr $ do
        writeCSR csr (x_rs1 `orE` t)
        assignGPR rd t
        incrPC
  , Pair Csrrc $ instSemantics (Rd :< Rs1 :< Csr :< Nil) $ do
      comment "Let t be the value of control and status register csr."
      comment "Write the bitwise AND of t and the ones' complement of x[rs1] to the csr."
      comment "Then, write t to x[rd]."

      rd :< rs1 :< csr :< Nil <- operandEs

      let t = readCSR csr
      let x_rs1 = readGPR rs1

      checkCSR (litBV 0b0 `ltuE` rs1) csr $ do
        writeCSR csr ((notE x_rs1) `andE` t)
        assignGPR rd t
        incrPC
  , Pair Csrrwi $ instSemantics (Rd :< Imm5 :< Csr :< Nil) $ do
      comment "Copies the control and status register csr to x[rd]."
      comment "Then, writes the five-bit zero-extended immediate zimm to the csr."

      rd :< zimm :< csr :< Nil <- operandEs
      let t = readCSR csr

      checkCSR (litBV 0b1) csr $ do
        assignGPR rd t
        writeCSR csr (zextE zimm)
        incrPC
  , Pair Csrrsi $ instSemantics (Rd :< Imm5 :< Csr :< Nil) $ do
      comment "Let t be the value of control and status register csr."
      comment "Write the bitwise OR of t and the five-bit zero-extended immediate zimm to the csr."
      comment "Then, write to to x[rd]."

      rd :< zimm :< csr :< Nil <- operandEs
      let t = readCSR csr

      checkCSR (litBV 0b1) csr $ do
        writeCSR csr (zextE zimm `orE` t)
        assignGPR rd t
        incrPC
  , Pair Csrrci $ instSemantics (Rd :< Imm5 :< Csr :< Nil) $ do
      comment "Let t be the value of control and status register csr."
      comment "Write the bitwise AND of t and the ones' complement of the five-bit zero-extended zimm to the csr."
      comment "Then, write t to x[rd]."

      rd :< zimm :< csr :< Nil <- operandEs
      let t = readCSR csr

      checkCSR (litBV 0b1) csr $ do
        writeCSR csr (notE (zextE zimm) `andE` t)
        assignGPR rd t
        incrPC

  -- S type
  , Pair Sb $ instSemantics (Rs1 :< Rs2 :< Imm12 :< Nil) $ do
      comment "Computes the least-significant byte in register x[rs2]."
      comment "Stores the result at memory address x[rs1] + sext(offset)."

      rs1 :< rs2 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let addr = x_rs1 `addE` sextE offset

      assignMem (knownNat @1) addr (extractE 0 x_rs2)
      incrPC
  , Pair Sh $ instSemantics (Rs1 :< Rs2 :< Imm12 :< Nil) $ do
      comment "Computes the least-significant half-word in register x[rs2]."
      comment "Stores the result at memory address x[rs1] + sext(offset)."

      rs1 :< rs2 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let addr  = x_rs1 `addE` sextE offset

      assignMem (knownNat @2) addr (extractE 0 x_rs2)
      incrPC
  , Pair Sw $ instSemantics (Rs1 :< Rs2 :< Imm12 :< Nil) $ do
      comment "Computes the least-significant word in register x[rs2]."
      comment "Stores the result at memory address x[rs1] + sext(offset)."

      rs1 :< rs2 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let addr  = x_rs1 `addE` sextE offset

      assignMem (knownNat @4) addr (extractE 0 x_rs2)
      incrPC
  -- B type
  , Pair Beq $ instSemantics (Rs1 :< Rs2 :< Imm12 :< Nil) $ do
      comment "If register x[rs1] equals register x[rs2], add sext(offset) to the pc."

      rs1 :< rs2 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      let pc = readPC
      ib <- instBytes
      let branchOffset = sextE (offset `concatE` (litBV 0 :: InstExpr 'B rv 1))

      jump (iteE (x_rs1 `eqE` x_rs2)
                (pc `addE` branchOffset)
                (pc `addE` zextE ib))
  , Pair Bne $ instSemantics (Rs1 :< Rs2 :< Imm12 :< Nil) $ do
      comment "If register x[rs1] does not equal register x[rs2], add sext(offset) to the pc."

      rs1 :< rs2 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      let pc = readPC
      ib <- instBytes
      let branchOffset = sextE (offset `concatE` (litBV 0 :: InstExpr 'B rv 1))

      jump (iteE (notE (x_rs1 `eqE` x_rs2))
                (pc `addE` branchOffset)
                (pc `addE` zextE ib))
  , Pair Blt $ instSemantics (Rs1 :< Rs2 :< Imm12 :< Nil) $ do
      comment "If register x[rs1] is less than register x[rs2], add sext(offset) to the pc."

      rs1 :< rs2 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      let pc = readPC
      ib <- instBytes
      let branchOffset = sextE (offset `concatE` (litBV 0 :: InstExpr 'B rv 1))

      jump (iteE (x_rs1 `ltsE` x_rs2)
                (pc `addE` branchOffset)
                (pc `addE` zextE ib))
  , Pair Bge $ instSemantics (Rs1 :< Rs2 :< Imm12 :< Nil) $ do
      comment "If register x[rs1] is greater than or equal to register x[rs2], add sext(offset) to the pc."

      rs1 :< rs2 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      let pc = readPC
      ib <- instBytes
      let branchOffset = sextE (offset `concatE` (litBV 0 :: InstExpr 'B rv 1))

      jump (iteE (notE (x_rs1 `ltsE` x_rs2))
                (pc `addE` branchOffset)
                (pc `addE` zextE ib))
  , Pair Bltu $ instSemantics (Rs1 :< Rs2 :< Imm12 :< Nil) $ do
      comment "If register x[rs1] is less than register x[rs2] as unsigned numbers, add sext(offset) to the pc."

      rs1 :< rs2 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      let pc = readPC
      ib <- instBytes
      let branchOffset = sextE (offset `concatE` (litBV 0 :: InstExpr 'B rv 1))

      jump (iteE (x_rs1 `ltuE` x_rs2)
                (pc `addE` branchOffset)
                (pc `addE` zextE ib))
  , Pair Bgeu $ instSemantics (Rs1 :< Rs2 :< Imm12 :< Nil) $ do
      comment "If register x[rs1] is greater than or equal to register x[rs2] as unsigned numbers, add sext(offset) to the pc."

      rs1 :< rs2 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      let pc = readPC
      ib <- instBytes
      let branchOffset = sextE (offset `concatE` (litBV 0 :: InstExpr 'B rv 1))

      jump (iteE (notE (x_rs1 `ltuE` x_rs2))
                (pc `addE` branchOffset)
                (pc `addE` zextE ib))

  -- U type
  , Pair Lui $ instSemantics (Rd :< Imm20 :< Nil) $ do
      comment "Writes the sign-extended 20-bit immediate, left-shifted by 12 bits, to x[rd]."
      comment "Zeros the lower 12 bits."

      rd :< imm20 :< Nil <- operandEs

      assignGPR rd $ sextE imm20 `sllE` litBV 12
      incrPC
  , Pair Auipc $ instSemantics (Rd :< Imm20 :< Nil) $ do
      comment "Adds the sign-extended 20-bit immediate, left-shifted by 12 bits, to the pc."
      comment "Writes the result to x[rd]."

      rd :< imm20 :< Nil <- operandEs
      let pc = readPC

      assignGPR rd $ pc `addE` (sextE imm20 `sllE` litBV 12)
      incrPC

  -- J type
  , Pair Jal $ instSemantics (Rd :< Imm20 :< Nil) $ do
      comment "Writes the address of the next instruction to x[rd]."
      comment "Then sets the pc to the current pc plus the sign-extended offset."

      rd :< imm20 :< Nil <- operandEs
      ib <- instBytes
      let pc = readPC

      assignGPR rd $ pc `addE` zextE ib
      jump $ pc `addE` sextE (imm20 `sllE` litBV 1)

  -- X type
  , Pair Illegal $ instSemantics (Imm32 :< Nil) $ do
      comment "Raise an IllegalInstruction exception"

      iw <- instWord
      raiseException IllegalInstruction iw
  ]

base64Encode :: 64 <= RVWidth rv => EncodeMap rv
base64Encode = Map.fromList
  [ Pair Addw  (OpBits RRepr (0b0111011 :< 0b000 :< 0b0000000 :< Nil))
  , Pair Subw  (OpBits RRepr (0b0111011 :< 0b000 :< 0b0100000 :< Nil))
  , Pair Sllw  (OpBits RRepr (0b0111011 :< 0b001 :< 0b0000000 :< Nil))
  , Pair Srlw  (OpBits RRepr (0b0111011 :< 0b101 :< 0b0000000 :< Nil))
  , Pair Sraw  (OpBits RRepr (0b0111011 :< 0b101 :< 0b0100000 :< Nil))
  , Pair Slliw (OpBits RRepr (0b0011011 :< 0b001 :< 0b0000000 :< Nil))
  , Pair Srliw (OpBits RRepr (0b0011011 :< 0b101 :< 0b0000000 :< Nil))
  , Pair Sraiw (OpBits RRepr (0b0011011 :< 0b101 :< 0b0100000 :< Nil))
  , Pair Lwu   (OpBits IRepr (0b0000011 :< 0b110 :< Nil))
  , Pair Ld    (OpBits IRepr (0b0000011 :< 0b011 :< Nil))
  , Pair Addiw (OpBits IRepr (0b0011011 :< 0b000 :< Nil))
  , Pair Sd    (OpBits SRepr (0b0100011 :< 0b011 :< Nil))
  ]

base64Semantics :: (KnownRVWidth rv, 64 <= RVWidth rv, KnownRVCConfig rv) => SemanticsMap rv
base64Semantics = Map.fromList
  [ Pair Addw $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Adds x[rs2] to [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rd :< rs1 :< rs2 :< Nil  <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let res   = x_rs1 `addE` x_rs2

      assignGPR rd $ sextE (extractE' (knownNat @32) 0 res)
      incrPC
  , Pair Subw $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rd :< rs1 :< rs2 :< Nil  <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2
      let res   = x_rs1 `subE` x_rs2

      assignGPR rd $ sextE (extractE' (knownNat @32) 0 res)
      incrPC

  , Pair Sllw $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1'  = extractE' (knownNat @32) 0 (readGPR rs1)
      let x_rs2 = readGPR rs2
      let shamt = extractE' (knownNat @5) 0 x_rs2

      assignGPR rd $ sextE (x_rs1' `sllE` zextE shamt)
      incrPC

  , Pair Srlw $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1'  = extractE' (knownNat @32) 0 (readGPR rs1)
      let x_rs2 = readGPR rs2
      let shamt = extractE' (knownNat @5) 0 x_rs2

      assignGPR rd $ sextE (x_rs1' `srlE` zextE shamt)
      incrPC

  , Pair Sraw $ instSemantics (Rd :< Rs1 :< Rs2 :< Nil) $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rd :< rs1 :< rs2 :< Nil <- operandEs

      let x_rs1'  = extractE' (knownNat @32) 0 (readGPR rs1)
      let x_rs2 = readGPR rs2
      let shamt = extractE' (knownNat @5) 0 x_rs2

      assignGPR rd $ sextE (x_rs1' `sraE` zextE shamt)
      incrPC

  , Pair Lwu $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Loads a word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], zero-extending the result."

      rd :< rs1 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let mVal  = readMem (knownNat @4) (x_rs1 `addE` sextE offset)

      assignGPR rd (zextE mVal)
      incrPC
  , Pair Ld $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Loads a double-word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], sign-extending the result."

      rd :< rs1 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let mVal  = readMem (knownNat @8) (x_rs1 `addE` sextE offset)

      assignGPR rd (sextE mVal)
      incrPC
  , Pair Addiw $ instSemantics (Rd :< Rs1 :< Imm12 :< Nil) $ do
      comment "Adds the sign-extended immediate to register x[rs1], truncating the result to 32 bits."
      comment "Writes the result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rd :< rs1 :< imm12 :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let res   = sextE (extractE' (knownNat @32) 0 (x_rs1 `addE` (sextE imm12)))

      assignGPR rd res
      incrPC

  , Pair Slliw $ instSemantics (Rd :< Rs1 :< Shamt5 :< Nil) $ do
      comment "Shifts lower 32 bits of register x[rs1] left by shamt bit positions."
      comment "The vacated bits are filled with zeros, and the sign-extended 32-bit result is written to x[rd]."

      rd :< rs1 :< shamt :< Nil <- operandEs
      let x_rs1'  = extractE' (knownNat @32) 0 (readGPR rs1)

      assignGPR rd $ sextE (x_rs1' `sllE` zextE shamt)
      incrPC

  , Pair Srliw $ instSemantics (Rd :< Rs1 :< Shamt5 :< Nil) $ do
      rd :< rs1 :< shamt :< Nil <- operandEs
      let x_rs1'  = extractE' (knownNat @32) 0 (readGPR rs1)

      assignGPR rd $ sextE (x_rs1' `srlE` zextE shamt)
      incrPC

  , Pair Sraiw $ instSemantics (Rd :< Rs1 :< Shamt5 :< Nil) $ do

      rd :< rs1 :< shamt :< Nil <- operandEs
      let x_rs1'  = extractE' (knownNat @32) 0 (readGPR rs1)

      assignGPR rd $ sextE (x_rs1' `sraE` zextE shamt)
      incrPC

  , Pair Sd $ instSemantics (Rs1 :< Rs2 :< Imm12 :< Nil) $ do
      comment "Computes the least-significant double-word in register x[rs2]."
      comment "Stores the result at memory address x[rs1] + sext(offset)."

      rs1 :< rs2 :< offset :< Nil <- operandEs

      let x_rs1 = readGPR rs1
      let x_rs2 = readGPR rs2

      assignMem (knownNat @8) (x_rs1 `addE` sextE offset) (extractE 0 x_rs2)
      incrPC
  ]
