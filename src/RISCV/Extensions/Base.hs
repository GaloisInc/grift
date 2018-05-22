{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-|
Module      : RISCV.Extensions.Base
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

RV32I base ISA, encoding and semantics.
-}

module RISCV.Extensions.Base
  ( base32
  , base64
  )
  where

import Data.BitVector.Sized
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

-- | RV32I/E base instruction set.
base32 :: KnownArch arch => InstructionSet arch exts
base32 = instructionSet baseEncode baseSemantics

-- | RV64I base instruction set.
base64 :: (KnownArch arch, 64 <= ArchWidth arch) => InstructionSet arch exts
base64 = base32 <> instructionSet base64Encode base64Semantics

baseEncode :: EncodeMap arch
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
  , Pair Slli   (OpBits IRepr (0b0010011 :< 0b001 :< Nil))
  , Pair Sri    (OpBits IRepr (0b0010011 :< 0b101 :< Nil))
  , Pair Fence  (OpBits IRepr (0b0001111 :< 0b000 :< Nil))
  , Pair FenceI (OpBits IRepr (0b0001111 :< 0b001 :< Nil))
  , Pair Ecb    (OpBits IRepr (0b1110011 :< 0b000 :< Nil))
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

  -- U type
  , Pair Lui   (OpBits URepr (0b0110111 :< Nil))
  , Pair Auipc (OpBits URepr (0b0010111 :< Nil))

  -- J type
  , Pair Jal (OpBits JRepr (0b1101111 :< Nil))

  -- X type
  , Pair Illegal (OpBits XRepr Nil)
  ]

baseSemantics :: forall arch . KnownArch arch => SemanticsMap arch
baseSemantics = Map.fromList
  [ Pair Add $ getFormula $ do
      comment "Adds register x[rs2] to register x[rs1] and writes the result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp $ \x y -> return (addE x y)
  , Pair Sub $ getFormula $ do
      comment "Subtracts register x[rs2] from register x[rs1] and writes the result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp $ \x y -> return (subE x y)
  , Pair Sll $ getFormula $ do
      comment "Shifts register x[rs1] left by x[rs2] bit positions."
      comment "The vacated bits are filled with zeros, and the result is written to x[rd]."

      rOp $ \e1 e2 -> do
        let archWidth = knownNat @(ArchWidth arch)
        return $ e1 `sllE` (e2 `andE` litBV (bitVector (fromIntegral (natValue archWidth) - 1)))
  , Pair Slt $ getFormula $ do
      comment "Compares x[rs1] and x[rs2] as two's complement numbers."
      comment "Writes 1 to x[rd] if x[rs1] is smaller, or 0 if not."

      rOp $ \e1 e2 -> return (zextE (ltsE e1 e2))
  , Pair Sltu $ getFormula $ do
      comment "Compares x[rs1] and x[rs2] as unsigned numbers."
      comment "Writes 1 to x[rd] if x[rs1] is smaller, or 0 if not."

      rOp $ \e1 e2 -> return (zextE (ltuE e1 e2))
  , Pair Xor $ getFormula $ do
      comment "Computes the bitwise exclusive-OR of registers x[rs1] and x[rs2]."
      comment "Writes the result to x[rd]."

      rOp $ \x y -> return (xorE x y)
  , Pair Srl $ getFormula $ do
      comment "Shifts register x[rs1] right by x[rs2] bit positions."
      comment "The vacated bits are filled with zeros, and the result is written to x[rd]."

      rOp $ \e1 e2 -> do
        let archWidth = knownNat @(ArchWidth arch)
            mask = litBV (bitVector (natValue archWidth - 1))

        return $ e1 `srlE` (e2 `andE` mask)
  , Pair Sra $ getFormula $ do
      comment "Shifts register x[rs1] right by x[rs2] bit positions."
      comment "The vacated bits are filled with copies of x[rs1]'s most significant bit."
      comment "The result is written to x[rd]."

      rOp $ \e1 e2 -> do
        let archWidth = knownNat @(ArchWidth arch)
        let mask = litBV (bitVector (natValue archWidth - 1))

        return $ e1 `sraE` (e2 `andE` mask)
  , Pair Or $ getFormula $ do
      comment "Computes the bitwise inclusive-OR of registers x[rs1] and x[rs2]."
      comment "Writes the result to x[rd]."

      rOp $ \x y -> return (orE x y)
  , Pair And $ getFormula $ do
      comment "Computes the bitwise AND of registers x[rs1] and x[rs2]."
      comment "Writes the result to x[rd]."

      rOp $ \x y -> return (andE x y)

  -- I type
  , Pair Jalr $ getFormula $ do
      comment "Sets the pc to x[rs1] + sext(offset)."
      comment "Masks off the least significant bit of the computed address."
      comment "Writes the previous pc+4 to x[rd]."

      rd :< rs1 :< offset :< Nil <- operandEs
      pc <- readPC
      ib <- instBytes
      x_rs1 <- readReg rs1

      assignReg rd $ pc `addE` zextE ib
      assignPC $ (x_rs1 `addE` sextE offset) `andE` notE (litBV 1)
  , Pair Lb $ getFormula $ do
      comment "Loads a byte from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], sign-extending the result."

      l readMem sextE
  , Pair Lh $ getFormula $ do
      comment "Loads a half-word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], sign-extending the result."

      l readMem16 sextE
  , Pair Lw $ getFormula $ do
      comment "Loads a word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], sign-extending the result."

      l readMem32 sextE
  , Pair Lbu $ getFormula $ do
      comment "Loads a byte from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], zero-extending the result."

      l readMem zextE
  , Pair Lhu $ getFormula $ do
      comment "Loads a half-word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], zero-extending the result."

      l readMem16 zextE
  , Pair Addi $ getFormula $ do
      comment "Adds the sign-extended immediate to register x[rs1] and writes the result to x[rd]."
      comment "Arithmetic overflow is ignored."

      iOp $ \x y -> return (addE x y)
  , Pair Slti $ getFormula $ do
      comment "Compares x[rs1] and the sign-extended immediate as two's not numbers."
      comment "Writes 1 to x[rd] if x[rs1] is smaller, 0 if not."

      iOp $ \e1 e2 -> return (zextE (ltsE e1 e2))
  , Pair Sltiu $ getFormula $ do
      comment "Compares x[rs1] and the sign-extended immediate as unsigned numbers."
      comment "Writes 1 to x[rd] if x[rs1] is smaller, 0 if not."

      iOp $ \e1 e2 -> return (zextE (ltuE e1 e2))
  , Pair Xori $ getFormula $ do
      comment "Computes the bitwise exclusive-OR of the sign-extended immediate and register x[rs1]."
      comment "Writes the result to x[rd]."

      iOp $ \x y -> return (xorE x y)
  , Pair Ori $ getFormula $ do
      comment "Computes the bitwise inclusive-OR of the sign-extended immediate and register x[rs1]."
      comment "Writes the result to x[rd]."

      iOp $ \x y -> return (orE x y)
  , Pair Andi $ getFormula $ do
      comment "Computes the bitwise AND of the sign-extended immediate and register x[rs1]."
      comment "Writes the result to x[rd]."

      iOp $ \x y -> return (andE x y)
  , Pair Slli $ getFormula $ do
      comment "Shifts register x[rs1] left by shamt bit positions."
      comment "The vacated bits are filled with zeros, and the result is written to x[rd]."

      rd :< rs1 :< imm12 :< Nil <- operandEs

      x_rs1 <- readReg rs1
      let shamt = extractEWithRepr (knownNat @7) 0 imm12
          ctrl  = extractEWithRepr (knownNat @5) 7 imm12

      let archWidth  = knownNat @(ArchWidth arch)
          shiftBound = litBV (bitVector (natValue archWidth) :: BitVector 7)

      let ctrlBad  = notE (ctrl `eqE` litBV 0b00000)
          shamtBad = notE (shamt `ltuE` shiftBound)

      -- Check that the control bits are all zero.

      branch (ctrlBad `orE` shamtBad)
        $> raiseException IllegalInstruction
        $> do assignReg rd $ x_rs1 `sllE` zextE shamt
              incrPC

  , Pair Sri $ getFormula $ do
      comment "Shifts register x[rs1] right (arithmetic or logical) by shamt bit positions."
      comment "The vacated bits are filled with copies of x[rs1]'s most significant bit."
      comment "The result is written to x[rd]."

      rd :< rs1 :< imm12 :< Nil <- operandEs

      x_rs1 <- readReg rs1
      let shamt = extractEWithRepr (knownNat @7) 0 imm12
          ctrl  = extractEWithRepr (knownNat @5) 7 imm12

      -- The control bits determine if the shift is arithmetic or logical.
      let lShift = ctrl `eqE` litBV 0b00000
          aShift = ctrl `eqE` litBV 0b01000

      -- Check that the shift amount is within the architecture width.
      let archWidth = knownNat @(ArchWidth arch)
          shiftBound = litBV (bitVector (natValue archWidth) :: BitVector 7)

      let ctrlBad  = notE (lShift `orE` aShift)
          shamtBad = notE (shamt `ltuE` shiftBound)

      let zext_shamt = zextE shamt
          result_l   = x_rs1 `srlE` zext_shamt
          result_r   = x_rs1 `sraE` zext_shamt

      branch (ctrlBad `orE` shamtBad)
        $> raiseException IllegalInstruction
        $> do assignReg rd $ iteE lShift result_l result_r
              incrPC

  -- TODO: in the case where the immediate operand is equal to 1, we need to raise an
  -- EnvironmentBreak exception.
  , Pair Ecb $ getFormula $ do
      comment "Makes a request of the execution environment or the debugger."

      raiseException EnvironmentCall

  -- TODO: Fence instructions.
  , Pair Fence $ getFormula $ do
      comment "Fence. Currently a no-op."
      return ()
  , Pair FenceI $ getFormula $ do
      comment "FenceI. Currently a no-op."
      return ()
  , Pair Csrrw $ getFormula $ do
      comment "Let t be the value of control and status register csr."
      comment "Copy x[rs1] to the csr, then write t to x[rd]."

      rd :< rs1 :< csr :< Nil <- operandEs

      t <- readCSR csr
      x_rs1 <- readReg rs1

      checkCSR (litBV 0b0 `ltuE` rs1) csr $ do
        assignCSR csr x_rs1
        assignReg rd t
  , Pair Csrrs $ getFormula $ do
      comment "Let t be the value of control and status register csr."
      comment "Write the bitwise OR of t and x[rs1] to the csr, then write t to x[rd]."

      rd :< rs1 :< csr :< Nil <- operandEs

      t <- readCSR csr
      x_rs1 <- readReg rs1

      checkCSR (litBV 0b0 `ltuE` rs1) csr $ do
        assignCSR csr (x_rs1 `orE` t)
        assignReg rd t
  , Pair Csrrc $ getFormula $ do
      comment "Let t be the value of control and status register csr."
      comment "Write the bitwise AND of t and the ones' complement of x[rs1] to the csr."
      comment "Then, write t to x[rd]."

      rd :< rs1 :< csr :< Nil <- operandEs

      t <- readCSR csr
      x_rs1 <- readReg rs1

      checkCSR (litBV 0b0 `ltuE` rs1) csr $ do
        assignCSR csr ((notE x_rs1) `andE` t)
        assignReg rd t
  , Pair Csrrwi $ getFormula $ do
      comment "Copies the control and status register csr to x[rd]."
      comment "Then, writes the five-bit zero-extended immediate zimm to the csr."

      rd :< zimm :< csr :< Nil <- operandEs
      t <- readCSR csr

      checkCSR (litBV 0b1) csr $ do
        assignReg rd t
        assignCSR csr (zextE zimm)
  , Pair Csrrsi $ getFormula $ do
      comment "Let t be the value of control and status register csr."
      comment "Write the bitwise OR of t and the five-bit zero-extended immediate zimm to the csr."
      comment "Then, write to to x[rd]."

      rd :< zimm :< csr :< Nil <- operandEs
      t <- readCSR csr

      checkCSR (litBV 0b1) csr $ do
        assignCSR csr (zextE zimm `orE` t)
        assignReg rd t
  , Pair Csrrci $ getFormula $ do
      comment "Let t be the value of control and status register csr."
      comment "Write the bitwise AND of t and the ones' complement of the five-bit zero-extended zimm to the csr."
      comment "Then, write t to x[rd]."

      rd :< zimm :< csr :< Nil <- operandEs
      t <- readCSR csr

      checkCSR (litBV 0b1) csr $ do
        assignCSR csr (notE (zextE zimm) `andE` t)
        assignReg rd t

  -- S type
  , Pair Sb $ getFormula $ do
      comment "Computes the least-significant byte in register x[rs2]."
      comment "Stores the result at memory address x[rs1] + sext(offset)."

      s assignMem
  , Pair Sh $ getFormula $ do
      comment "Computes the least-significant half-word in register x[rs2]."
      comment "Stores the result at memory address x[rs1] + sext(offset)."

      s assignMem16
  , Pair Sw $ getFormula $ do
      comment "Computes the least-significant word in register x[rs2]."
      comment "Stores the result at memory address x[rs1] + sext(offset)."

      s assignMem32
  -- B type
  , Pair Beq $ getFormula $ do
      comment "If register x[rs1] equals register x[rs2], add sext(offset) to the pc."

      b eqE
  , Pair Bne $ getFormula $ do
      comment "If register x[rs1] does not equal register x[rs2], add sext(offset) to the pc."

      b $ \e1 e2 -> notE (eqE e1 e2)
  , Pair Blt $ getFormula $ do
      comment "If register x[rs1] is less than register x[rs2], add sext(offset) to the pc."

      b ltsE
  , Pair Bge $ getFormula $ do
      comment "If register x[rs1] is greater than or equal to register x[rs2], add sext(offset) to the pc."

      b $ \e1 e2 -> notE (ltsE e1 e2)
  , Pair Bltu $ getFormula $ do
      comment "If register x[rs1] is less than register x[rs2] as unsigned numbers, add sext(offset) to the pc."

      b ltuE
  , Pair Bgeu $ getFormula $ do
      comment "If register x[rs1] is greater than or equal to register x[rs2] as unsigned numbers, add sext(offset) to the pc."

      b $ \e1 e2 -> notE (ltuE e1 e2)

  -- U type
  , Pair Lui $ getFormula $ do
      comment "Writes the sign-extended 20-bit immediate, left-shifted by 12 bits, to x[rd]."
      comment "Zeros the lower 12 bits."

      rd :< imm20 :< Nil <- operandEs

      assignReg rd $ sextE imm20 `sllE` litBV 12
      incrPC
  , Pair Auipc $ getFormula $ do
      comment "Adds the sign-extended 20-bit immediate, left-shifted by 12 bits, to the pc."
      comment "Writes the result to x[rd]."

      rd :< imm20 :< Nil <- operandEs
      pc <- readPC

      assignReg rd $ pc `addE` sextE imm20 `sllE` litBV 12
      incrPC

  -- J type
  , Pair Jal $ getFormula $ do
      comment "Writes the address of the next instruction to x[rd]."
      comment "Then sets the pc to the current pc plus the sign-extended offset."

      rd :< imm20 :< Nil <- operandEs
      ib <- instBytes
      pc <- readPC

      assignReg rd $ pc `addE` zextE ib
      assignPC $ pc `addE` sextE (imm20 `sllE` litBV 1)

  -- X type
  , Pair Illegal $ getFormula $ do
      comment "Raise an IllegalInstruction exception"

      raiseException IllegalInstruction
  ]

base64Encode :: 64 <= ArchWidth arch => EncodeMap arch
base64Encode = Map.fromList
  [ Pair Addw  (OpBits RRepr (0b0111011 :< 0b000 :< 0b0000000 :< Nil))
  , Pair Subw  (OpBits RRepr (0b0111011 :< 0b000 :< 0b0100000 :< Nil))
  , Pair Sllw  (OpBits RRepr (0b0111011 :< 0b001 :< 0b0000000 :< Nil))
  , Pair Srlw  (OpBits RRepr (0b0111011 :< 0b101 :< 0b0000000 :< Nil))
  , Pair Sraw  (OpBits RRepr (0b0111011 :< 0b101 :< 0b0100000 :< Nil))
  , Pair Lwu   (OpBits IRepr (0b0000011 :< 0b110 :< Nil))
  , Pair Ld    (OpBits IRepr (0b0000011 :< 0b011 :< Nil))
  , Pair Addiw (OpBits IRepr (0b0011011 :< 0b000 :< Nil))
  , Pair Slliw (OpBits IRepr (0b0011011 :< 0b001 :< Nil))
  , Pair Sriw  (OpBits IRepr (0b0011011 :< 0b101 :< Nil))
  , Pair Sd    (OpBits SRepr (0b0100011 :< 0b011 :< Nil))
  ]

base64Semantics :: (KnownArch arch, 64 <= ArchWidth arch) => SemanticsMap arch
base64Semantics = Map.fromList
  [ Pair Addw $ getFormula $ do
      comment "Adds x[rs2] to [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp32 $ \e1 e2 -> return (e1 `addE` e2)

  , Pair Subw $ getFormula $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp32 $ \e1 e2 -> return (e1 `subE` e2)

  -- TODO: Correct this
  , Pair Sllw $ getFormula $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp32 $ \e1 e2 -> return (e1 `sllE` e2)

  -- TODO: Correct this
  , Pair Srlw $ getFormula $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp32 $ \e1 e2 -> return (e1 `srlE` e2)

  -- TODO: Correct this
  , Pair Sraw $ getFormula $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp32 $ \e1 e2 -> return (e1 `sraE` e2)
  , Pair Lwu $ getFormula $ do
      comment "Loads a word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], zero-extending the result."

      l readMem32 zextE
  , Pair Ld $ getFormula $ do
      comment "Loads a double-word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], sign-extending the result."

      l readMem64 sextE
  , Pair Addiw $ getFormula $ do
      comment "Adds the sign-extended immediate to register x[rs1], truncating the result to 32 bits."
      comment "Writes the result to x[rd]."
      comment "Arithmetic overflow is ignored."

      iOp $ \e1 e2 -> return $ sextE (extractEWithRepr (knownNat @32) 0 (e1 `addE` e2))

  -- TODO: Correct this
  , Pair Slliw $ getFormula $ do
      comment "Shifts register x[rs1] left by shamt bit positions."
      comment "Truncates the result to 32 bits."
      comment "The vacated bits are filled with zeros, and the sign-extended result is written to x[rd]."

      rd :< rs1 :< imm12 :< Nil <- operandEs
      x_rs1 <- readReg rs1

      let shamt = extractEWithRepr (knownNat @7) 0 imm12
          ctrl  = extractEWithRepr (knownNat @5) 7 imm12

      let shiftBound = litBV (bitVector 32 :: BitVector 7)

      let ctrlBad  = notE (ctrl `eqE` litBV 0b00000)
          shiftBad = notE (shamt `ltuE` shiftBound)

      branch (ctrlBad `orE` shiftBad)
        $> raiseException IllegalInstruction
        $> do assignReg rd $ x_rs1 `sllE` zextE shamt
              incrPC

  -- TODO: Correct this
  , Pair Sriw $ getFormula $ do
      comment "Shifts register x[rs1] right logically/arithmetically by shamt bit positions."
      comment "Truncates the result to 32 bits."
      comment "The vacated bits are filled with zeros, and the sign-extended result is written to x[rd]."

      rd :< rs1 :< imm12 :< Nil <- operandEs
      x_rs1 <- readReg rs1

      let shamt = extractEWithRepr (knownNat @7) 0 imm12
          ctrl  = extractEWithRepr (knownNat @5) 7 imm12

      -- The control bits determine if the shift is arithmetic or logical.
      let lShift = ctrl `eqE` litBV 0b00000
          aShift = ctrl `eqE` litBV 0b01000

      let shiftBound = litBV (bitVector 32 :: BitVector 7)

      let ctrlBad  = notE (lShift `orE` aShift)
          shiftBad = notE (shamt `ltuE` shiftBound)

      let result_l = x_rs1 `srlE` zextE shamt
          result_r = x_rs1 `sraE` zextE shamt

      branch (ctrlBad `orE` shiftBad)
        $> raiseException IllegalInstruction
        $> do assignReg rd $ iteE lShift result_l result_r
              incrPC

  , Pair Sd $ getFormula $ do
      comment "Computes the least-significant double-word in register x[rs2]."
      comment "Stores the result at memory address x[rs1] + sext(offset)."

      s assignMem64
  ]
