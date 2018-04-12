{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

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

import Data.Monoid
import qualified Data.Parameterized.Map as Map
import Data.Parameterized

import RISCV.Extensions.Helpers
import RISCV.Instruction
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
    Pair Add  (ROpBits 0b0110011 0b000 0b0000000)
  , Pair Sub  (ROpBits 0b0110011 0b000 0b0100000)
  , Pair Sll  (ROpBits 0b0110011 0b001 0b0000000)
  , Pair Slt  (ROpBits 0b0110011 0b010 0b0000000)
  , Pair Sltu (ROpBits 0b0110011 0b011 0b0000000)
  , Pair Xor  (ROpBits 0b0110011 0b100 0b0000000)
  , Pair Srl  (ROpBits 0b0110011 0b101 0b0000000)
  , Pair Sra  (ROpBits 0b0110011 0b101 0b0100000)
  , Pair Or   (ROpBits 0b0110011 0b110 0b0000000)
  , Pair And  (ROpBits 0b0110011 0b111 0b0000000)

  -- I type
  , Pair Jalr   (IOpBits 0b1100111 0b000)
  , Pair Lb     (IOpBits 0b0000011 0b000)
  , Pair Lh     (IOpBits 0b0000011 0b001)
  , Pair Lw     (IOpBits 0b0000011 0b010)
  , Pair Lbu    (IOpBits 0b0000011 0b100)
  , Pair Lhu    (IOpBits 0b0000011 0b101)
  , Pair Addi   (IOpBits 0b0010011 0b000)
  , Pair Slti   (IOpBits 0b0010011 0b010)
  , Pair Sltiu  (IOpBits 0b0010011 0b011)
  , Pair Xori   (IOpBits 0b0010011 0b100)
  , Pair Ori    (IOpBits 0b0010011 0b110)
  , Pair Andi   (IOpBits 0b0010011 0b111)
  , Pair Slli   (IOpBits 0b0010011 0b001)
  , Pair Sri    (IOpBits 0b0010011 0b101)
  , Pair Fence  (IOpBits 0b0001111 0b000)
  , Pair FenceI (IOpBits 0b0001111 0b001)
  , Pair Ecb    (IOpBits 0b1110011 0b000)
  , Pair Csrrw  (IOpBits 0b1110011 0b001)
  , Pair Csrrs  (IOpBits 0b1110011 0b010)
  , Pair Csrrc  (IOpBits 0b1110011 0b011)
  , Pair Csrrwi (IOpBits 0b1110011 0b101)
  , Pair Csrrsi (IOpBits 0b1110011 0b110)
  , Pair Csrrci (IOpBits 0b1110011 0b111)

  -- S type
  , Pair Sb (SOpBits 0b0100011 0b000)
  , Pair Sh (SOpBits 0b0100011 0b001)
  , Pair Sw (SOpBits 0b0100011 0b010)

  -- B type
  , Pair Beq  (BOpBits 0b1100011 0b000)
  , Pair Bne  (BOpBits 0b1100011 0b001)
  , Pair Blt  (BOpBits 0b1100011 0b100)
  , Pair Bge  (BOpBits 0b1100011 0b101)
  , Pair Bltu (BOpBits 0b1100011 0b110)
  , Pair Bgeu (BOpBits 0b1100011 0b111)

  -- U type
  , Pair Lui   (UOpBits 0b0110111)
  , Pair Auipc (UOpBits 0b0010111)

  -- J type
  , Pair Jal (JOpBits 0b1101111)

  -- X type
  , Pair Illegal XOpBits
  ]

baseSemantics :: KnownArch arch => SemanticsMap arch
baseSemantics = Map.fromList
  [ Pair Add $ getFormula $ do
      comment "Adds register x[rs2] to register x[rs1] and writes the result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp addE
  , Pair Sub $ getFormula $ do
      comment "Subtracts register x[rs2] from register x[rs1] and writes the result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp subE
  , Pair Sll $ getFormula $ do
      comment "Shifts register x[rs1] left by x[rs2] bit positions."
      comment "The vacated bits are filled with zeros, and the result is writtein to x[rd]."

      rOp sllE
  , Pair Slt $ getFormula $ do
      comment "Compares x[rs1] and x[rs2] as two's complement numbers."
      comment "Writes 1 to x[rd] if x[rs1] is smaller, or 0 if not."

      rOp (\e1 e2 -> ltsE e1 e2 >>= zextE)
  , Pair Sltu $ getFormula $ do
      comment "Compares x[rs1] and x[rs2] as unsigned numbers."
      comment "Writes 1 to x[rd] if x[rs1] is smaller, or 0 if not."

      rOp (\e1 e2 -> ltuE e1 e2 >>= zextE)
  , Pair Xor $ getFormula $ do
      comment "Computes the bitwise exclusive-OR of registers x[rs1] and x[rs2]."
      comment "Writes the result to x[rd]."

      rOp xorE
  , Pair Srl $ getFormula $ do
      comment "Shifts register x[rs1] right by x[rs2] bit positions."
      comment "The vacated bits are filled with zeros, and the result is written to x[rd]."

      rOp srlE
  , Pair Sra $ getFormula $ do
      comment "Shifts register x[rs1] right by x[rs2] bit positions."
      comment "The vacated bits are filled with copies of x[rs1]'s most significant bit."
      comment "The result is written to x[rd]."

      rOp sraE
  , Pair Or $ getFormula $ do
      comment "Computes the bitwise inclusive-OR of registers x[rs1] and x[rs2]."
      comment "Writes the result to x[rd]."

      rOp orE
  , Pair And $ getFormula $ do
      comment "Computes the bitwise AND of registers x[rs1] and x[rs2]."
      comment "Writes the result to x[rd]."

      rOp andE

  -- -- I type
  , Pair Jalr $ getFormula $ do
      comment "Sets the pc to x[rs1] + sext(offset)."
      comment "Masks off the least significant bit of the computed address."
      comment "Writes the previous pc+4 to x[rd]."

      (rd, rs1, offset) <- params

      pc <- pcRead
      t  <- pc `addE` litBV 4

      x_rs1       <- regRead rs1
      sext_offset <- sextE offset
      new_pc'     <- x_rs1 `addE` sext_offset
      mask        <- notE (litBV 1)
      new_pc      <- new_pc' `andE` mask

      assignPC new_pc
      assignReg rd t
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

      iOp addE
  , Pair Slti $ getFormula $ do
      comment "Compares x[rs1] and the sign-extended immediate as two's not numbers."
      comment "Writes 1 to x[rd] if x[rs1] is smaller, 0 if not."

      iOp (\e1 e2 -> ltsE e1 e2 >>= zextE)
  , Pair Sltiu $ getFormula $ do
      comment "Compares x[rs1] and the sign-extended immediate as unsigned numbers."
      comment "Writes 1 to x[rd] if x[rs1] is smaller, 0 if not."

      iOp (\e1 e2 -> ltuE e1 e2 >>= zextE)
  , Pair Xori $ getFormula $ do
      comment "Computes the bitwise exclusive-OR of the sign-extended immediate and register x[rs1]."
      comment "Writes the result to x[rd]."

      iOp xorE
  , Pair Ori $ getFormula $ do
      comment "Computes the bitwise inclusive-OR of the sign-extended immediate and register x[rs1]."
      comment "Writes the result to x[rd]."

      iOp orE
  , Pair Andi $ getFormula $ do
      comment "Computes the bitwise AND of the sign-extended immediate and register x[rs1]."
      comment "Writes the result to x[rd]."

      iOp andE
  , Pair Slli $ getFormula $ do
      comment "Shifts register x[rs1] left by shamt bit positions."
      comment "The vacated bits are filled with zeros, and the result is written to x[rd]."

      iOp sllE
  -- TODO: Modify this to dynamically check if arch is 32, 64, or 128 bits. Remove
  -- the XLen constructor from the semantics if possible.
  , Pair Sri $ getFormula $ do
      comment "Shifts register x[rs1] right by shamt bit positions."
      comment "The vacated bits are filled with copies of x[rs1]'s most significant bit."
      comment "The result is written to x[rd]."

      (rd, rs1, imm12) <- params

      x_rs1 <- regRead rs1
      -- Extract the LS 7 bits from the immediate.
      shamt <- extractEWithRepr (knownNat :: NatRepr 7) 0 imm12
      -- Extract the HS 5 bits from the immediate.
      ctrl <- extractEWithRepr (knownNat :: NatRepr 5) 7 imm12

      -- The control bits determine if the shift is arithmetic or logical.
      lShift <- ctrl `eqE` litBV 0b00000
      aShift <- ctrl `eqE` litBV 0b01000

      -- Conditionally throw an exception if the shift control bits are invalid.
      shiftTypeOK <- lShift `orE` aShift
      illShiftType <- notE shiftTypeOK
      raiseException illShiftType IllegalInstruction

      -- Conditionally throw an exception if the shift amount is too large.
      zext_shamt <- zextE shamt
      shamtOK <- zext_shamt `ltuE` xlen
      illShiftAmount <- notE shamtOK
      raiseException illShiftAmount IllegalInstruction

      x_rs1_l <- srlE x_rs1 zext_shamt
      x_rs1_r <- sraE x_rs1 zext_shamt
      result <- iteE lShift x_rs1_l x_rs1_r
      assignReg rd result
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

      b (\e1 e2 -> eqE e1 e2 >>= notE)
  , Pair Blt $ getFormula $ do
      comment "If register x[rs1] is less than register x[rs2], add sext(offset) to the pc."

      b ltsE
  , Pair Bge $ getFormula $ do
      comment "If register x[rs1] is greater than or equal to register x[rs2], add sext(offset) to the pc."

      b (\e1 e2 -> ltsE e1 e2 >>= notE)
  , Pair Bltu $ getFormula $ do
      comment "If register x[rs1] is less than register x[rs2] as unsigned numbers, add sext(offset) to the pc."

      b ltuE
  , Pair Bgeu $ getFormula $ do
      comment "If register x[rs1] is greater than or equal to register x[rs2] as unsigned numbers, add sext(offset) to the pc."

      b (\e1 e2 -> ltuE e1 e2 >>= notE)

  -- U type
  , Pair Lui $ getFormula $ do
      comment "Writes the sign-extended 20-bit immediate, left-shifted by 12 bits, to x[rd]."
      comment "Zeros the lower 12 bits."

      (rd, imm20) <- params

      sext_imm20 <- sextE imm20
      result <- sext_imm20 `sllE` litBV 12

      assignReg rd result
      incrPC
  , Pair Auipc $ getFormula $ do
      comment "Adds the sign-extended 20-bit immediate, left-shifted by 12 bits, to the pc."
      comment "Writes the result to x[rd]."

      (rd, imm20) <- params

      sext_imm20 <- sextE imm20
      shifted <- sext_imm20 `sllE` litBV 12
      pc <- pcRead
      result <- pc `addE` shifted

      assignReg rd result
      incrPC

  -- J type
  , Pair Jal $ getFormula $ do
      comment "Writes the address of the next instruction to x[rd]."
      comment "Then sets the pc to the current pc plus the sign-extended offset."

      (rd, imm20') <- params
      ib' <- instBytes
      ib <- zextE ib'
      imm21' <- imm20' `sllE` litBV 1
      imm21 <- sextE imm21'

      pc <- pcRead
      incr_pc <- pc `addE` ib
      pc_offset <- pc `addE` imm21

      assignReg rd incr_pc
      assignPC pc_offset

  -- X type
  , Pair Illegal $ getFormula $ do
      comment "Raise an IllegalInstruction exception"

      raiseException (litBV 0b1) IllegalInstruction
  ]

base64Encode :: 64 <= ArchWidth arch => EncodeMap arch
base64Encode = Map.fromList
  [ Pair Addw  (ROpBits 0b0111011 0b000 0b0000000)
  , Pair Subw  (ROpBits 0b0111011 0b000 0b0100000)
  , Pair Sllw  (ROpBits 0b0111011 0b001 0b0000000)
  , Pair Srlw  (ROpBits 0b0111011 0b101 0b0000000)
  , Pair Sraw  (ROpBits 0b0111011 0b101 0b0100000)
  , Pair Lwu   (IOpBits 0b0000011 0b110)
  , Pair Ld    (IOpBits 0b0000011 0b011)
  , Pair Addiw (IOpBits 0b0011011 0b000)
  , Pair Slliw (IOpBits 0b0011011 0b001)
  , Pair Sriw  (IOpBits 0b0011011 0b101)
  , Pair Sd    (SOpBits 0b0100011 0b011)
  ]

base64Semantics :: (KnownArch arch, 64 <= ArchWidth arch) => SemanticsMap arch
base64Semantics = Map.fromList
  [ Pair Addw $ getFormula $ do
      comment "Adds x[rs2] to [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp $ \e1 e2 -> do
        a  <- e1 `addE` e2
        a' <- extractEWithRepr (knownNat :: NatRepr 32) 0 a

        res <- sextE a'
        return res

  , Pair Subw $ getFormula $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp $ \e1 e2 -> do
        a  <- e1 `subE` e2
        a' <- extractEWithRepr (knownNat :: NatRepr 32) 0 a

        res <- sextE a'
        return res
  , Pair Sllw $ getFormula $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp $ \e1 e2 -> do
        a  <- e1 `sllE` e2
        a' <- extractEWithRepr (knownNat :: NatRepr 32) 0 a

        res <- sextE a'
        return res
  , Pair Srlw $ getFormula $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp $ \e1 e2 -> do
        a  <- e1 `srlE` e2
        a' <- extractEWithRepr (knownNat :: NatRepr 32) 0 a

        res <- sextE a'
        return res
  , Pair Sraw $ getFormula $ do
      comment "Subtracts x[rs2] from [rs1], truncating the result to 32 bits."
      comment "Writes the sign-extended result to x[rd]."
      comment "Arithmetic overflow is ignored."

      rOp $ \e1 e2 -> do
        a  <- e1 `sraE` e2
        a' <- extractEWithRepr (knownNat :: NatRepr 32) 0 a

        res <- sextE a'
        return res
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

      iOp $ \e1 e2 -> do
        a  <- e1 `addE` e2
        a' <- extractEWithRepr (knownNat :: NatRepr 32) 0 a

        res <- sextE a'
        return res
  , Pair Slliw $ getFormula $ do
      comment "Shifts register x[rs1] left by shamt bit positions."
      comment "Truncates the result to 32 bits."
      comment "The vacated bits are filled with zeros, and the sign-extended result is written to x[rd]."

      iOp $ \e1 e2 -> do
        a  <- e1 `sllE` e2
        a' <- extractEWithRepr (knownNat :: NatRepr 32) 0 a

        res <- sextE a'
        return res
  , Pair Sriw $ getFormula $ do
      comment "Shifts register x[rs1] right logically/arithmetically by shamt bit positions."
      comment "Truncates the result to 32 bits."
      comment "The vacated bits are filled with zeros, and the sign-extended result is written to x[rd]."

      (rd, rs1, imm12) <- params

      x_rs1' <- regRead rs1
      x_rs1 <- extractEWithRepr (knownNat :: NatRepr 32) 0 x_rs1'
      -- Extract the LS 7 bits from the immediate.
      shamt <- extractEWithRepr (knownNat :: NatRepr 7) 0 imm12
      -- Extract the HS 5 bits from the immediate.
      ctrl <- extractEWithRepr (knownNat :: NatRepr 5) 7 imm12

      -- The control bits determine if the shift is arithmetic or logical.
      lShift <- ctrl `eqE` litBV 0b00000
      aShift <- ctrl `eqE` litBV 0b01000

      -- Conditionally throw an exception if the shift control bits are invalid.
      shiftTypeOK <- lShift `orE` aShift
      illShiftType <- notE shiftTypeOK
      raiseException illShiftType IllegalInstruction

      -- Conditionally throw an exception if the shift amount is too large.
      zext_shamt <- zextE shamt
      xlen' <- extractEWithRepr (knownNat :: NatRepr 32) 0 xlen
      shamtOK <- zext_shamt `ltuE` xlen'
      illShiftAmount <- notE shamtOK
      raiseException illShiftAmount IllegalInstruction

      x_rs1_l <- srlE x_rs1 zext_shamt
      x_rs1_r <- sraE x_rs1 zext_shamt
      result' <- iteE lShift x_rs1_l x_rs1_r
      result <- sextE result'
      assignReg rd result
      incrPC
  , Pair Sd $ getFormula $ do
      comment "Computes the least-significant double-word in register x[rs2]."
      comment "Stores the result at memory address x[rs1] + sext(offset)."

      s assignMem64
  ]
