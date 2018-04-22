{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
        let archWidth = knownNat :: NatRepr (ArchWidth arch)
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
        let archWidth = knownNat :: NatRepr (ArchWidth arch)
            mask = litBV (bitVector (natValue archWidth - 1))

        return $ e1 `srlE` (e2 `andE` mask)
  , Pair Sra $ getFormula $ do
      comment "Shifts register x[rs1] right by x[rs2] bit positions."
      comment "The vacated bits are filled with copies of x[rs1]'s most significant bit."
      comment "The result is written to x[rd]."

      rOp $ \e1 e2 -> do
        let archWidth = knownNat :: NatRepr (ArchWidth arch)
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
      pc <- pcRead
      x_rs1 <- regRead rs1

      assignPC $ (x_rs1 `addE` sextE offset) `andE` notE (litBV 1)
      assignReg rd (pc `addE` litBV 4)
  , Pair Lb $ getFormula $ do
      comment "Loads a byte from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], sign-extending the result."

      l memRead sextE
  , Pair Lh $ getFormula $ do
      comment "Loads a half-word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], sign-extending the result."

      l memRead16 sextE
  , Pair Lw $ getFormula $ do
      comment "Loads a word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], sign-extending the result."

      l memRead32 sextE
  , Pair Lbu $ getFormula $ do
      comment "Loads a byte from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], zero-extending the result."

      l memRead zextE
  , Pair Lhu $ getFormula $ do
      comment "Loads a half-word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], zero-extending the result."

      l memRead16 zextE
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

      x_rs1 <- regRead rs1
      let shamt = extractEWithRepr (knownNat :: NatRepr 7) 0 imm12
          ctrl  = extractEWithRepr (knownNat :: NatRepr 5) 7 imm12

      -- Check that the control bits are all zero.
      raiseException (notE (ctrl `eqE` litBV 0b00000)) IllegalInstruction

      -- Check that the shift amount is within the architecture width.
      let archWidth  = knownNat :: NatRepr (ArchWidth arch)
          shiftBound = litBV (bitVector (natValue archWidth) :: BitVector 7)
      raiseException (notE (shamt `ltuE` shiftBound)) IllegalInstruction

      -- Perform the left logical shift.
      assignReg rd $ x_rs1 `sllE` zextE shamt
      incrPC

  , Pair Sri $ getFormula $ do
      comment "Shifts register x[rs1] right (arithmetic or logical) by shamt bit positions."
      comment "The vacated bits are filled with copies of x[rs1]'s most significant bit."
      comment "The result is written to x[rd]."

      rd :< rs1 :< imm12 :< Nil <- operandEs

      x_rs1 <- regRead rs1
      let shamt = extractEWithRepr (knownNat :: NatRepr 7) 0 imm12
          ctrl  = extractEWithRepr (knownNat :: NatRepr 5) 7 imm12

      -- The control bits determine if the shift is arithmetic or logical.
      let lShift = ctrl `eqE` litBV 0b00000
          aShift = ctrl `eqE` litBV 0b01000

      -- Check that the control bits are valid.
      raiseException (notE (lShift `orE` aShift)) IllegalInstruction

      -- Check that the shift amount is within the architecture width.
      let archWidth = knownNat :: NatRepr (ArchWidth arch)
          shiftBound = litBV (bitVector (natValue archWidth) :: BitVector 7)
      raiseException (notE (shamt `ltuE` shiftBound)) IllegalInstruction

      -- Perform the right logical/arithmetic shift.
      let zext_shamt = zextE shamt
          result_l   = x_rs1 `srlE` zext_shamt
          result_r   = x_rs1 `sraE` zext_shamt
      assignReg rd $ iteE lShift result_l result_r
      incrPC

  -- TODO: in the case where the immediate operand is equal to 1, we need to raise an
  -- EnvironmentBreak exception.
  , Pair Ecb $ getFormula $ do
      comment "Makes a request of the execution environment or the debugger."

      raiseException (litBV 0b1) EnvironmentCall

  -- TODO: Fence/csr instructions.
  -- , Pair Fence   undefined
  -- , Pair FenceI  undefined
  -- , Pair Csrrw   undefined
  -- , Pair Csrrs   undefined
  -- , Pair Csrrc   undefined
  -- , Pair Csrrwi  undefined
  -- , Pair Csrrsi  undefined
  -- , Pair Csrrci  undefined

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
      pc <- pcRead

      assignReg rd $ pc `addE` sextE imm20 `sllE` litBV 12
      incrPC

  -- J type
  , Pair Jal $ getFormula $ do
      comment "Writes the address of the next instruction to x[rd]."
      comment "Then sets the pc to the current pc plus the sign-extended offset."

      rd :< imm20 :< Nil <- operandEs
      ib <- instBytes
      pc <- pcRead

      assignReg rd $ pc `addE` zextE ib
      assignPC $ pc `addE` sextE (imm20 `sllE` litBV 1)

  -- X type
  , Pair Illegal $ getFormula $ do
      comment "Raise an IllegalInstruction exception"

      raiseException (litBV 0b1) IllegalInstruction
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

      rOp $ \e1 e2 -> return $ sextE (extractEWithRepr (knownNat :: NatRepr 32) 0 (e1 `addE` e2))

  , Pair Subw $ getFormula $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp $ \e1 e2 -> return $ sextE (extractEWithRepr (knownNat :: NatRepr 32) 0 (e1 `subE` e2))

  -- TODO: Correct this
  , Pair Sllw $ getFormula $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp $ \e1 e2 -> return $ sextE (extractEWithRepr (knownNat :: NatRepr 32) 0 (e1 `sllE` e2))

  -- TODO: Correct this
  , Pair Srlw $ getFormula $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp $ \e1 e2 -> return $ sextE (extractEWithRepr (knownNat :: NatRepr 32) 0 (e1 `srlE` e2))

  -- TODO: Correct this
  , Pair Sraw $ getFormula $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp $ \e1 e2 -> return $ sextE (extractEWithRepr (knownNat :: NatRepr 32) 0 (e1 `sraE` e2))
  , Pair Lwu $ getFormula $ do
      comment "Loads a word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], zero-extending the result."

      l memRead32 zextE
  , Pair Ld $ getFormula $ do
      comment "Loads a double-word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], sign-extending the result."

      l memRead64 sextE
  , Pair Addiw $ getFormula $ do
      comment "Adds the sign-extended immediate to register x[rs1], truncating the result to 32 bits."
      comment "Writes the result to x[rd]."
      comment "Arithmetic overflow is ignored."

      iOp $ \e1 e2 -> return $ sextE (extractEWithRepr (knownNat :: NatRepr 32) 0 (e1 `addE` e2))

  -- TODO: Correct this
  , Pair Slliw $ getFormula $ do
      comment "Shifts register x[rs1] left by shamt bit positions."
      comment "Truncates the result to 32 bits."
      comment "The vacated bits are filled with zeros, and the sign-extended result is written to x[rd]."

      rd :< rs1 :< imm12 :< Nil <- operandEs
      x_rs1 <- regRead rs1

      let shamt = extractEWithRepr (knownNat :: NatRepr 7) 0 imm12
          ctrl  = extractEWithRepr (knownNat :: NatRepr 5) 7 imm12

      -- Check that the control bits are all zero.
      raiseException (notE (ctrl `eqE` litBV 0b00000)) IllegalInstruction

      -- Check that the shift amount is within 5 bits.
      let shiftBound = litBV (bitVector 32 :: BitVector 7)
      raiseException (notE (shamt `ltuE` shiftBound)) IllegalInstruction

      -- Perform the left logical shift.
      assignReg rd $ x_rs1 `sllE` zextE shamt
      incrPC

  -- TODO: Correct this
  , Pair Sriw $ getFormula $ do
      comment "Shifts register x[rs1] right logically/arithmetically by shamt bit positions."
      comment "Truncates the result to 32 bits."
      comment "The vacated bits are filled with zeros, and the sign-extended result is written to x[rd]."

      rd :< rs1 :< imm12 :< Nil <- operandEs
      x_rs1 <- regRead rs1

      let shamt = extractEWithRepr (knownNat :: NatRepr 7) 0 imm12
          ctrl  = extractEWithRepr (knownNat :: NatRepr 5) 7 imm12

      -- The control bits determine if the shift is arithmetic or logical.
      let lShift = ctrl `eqE` litBV 0b00000
          aShift = ctrl `eqE` litBV 0b01000

      -- Check that the control bits are valid.
      raiseException (notE (lShift `orE` aShift)) IllegalInstruction

      -- Check that the shift amount is within the architecture width.
      let shiftBound = litBV (bitVector 32 :: BitVector 7)
      raiseException (notE (shamt `ltuE` shiftBound)) IllegalInstruction

      -- Perform the right logical/arithmetic shift.
      let result_l = x_rs1 `srlE` zextE shamt
          result_r = x_rs1 `sraE` zextE shamt
      assignReg rd $ iteE lShift result_l result_r
      incrPC
  , Pair Sd $ getFormula $ do
      comment "Computes the least-significant double-word in register x[rs2]."
      comment "Stores the result at memory address x[rs1] + sext(offset)."

      s assignMem64
  ]
