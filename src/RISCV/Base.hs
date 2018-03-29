{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : RISCV.Base
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

RV32I base ISA, encoding and semantics.
-}

module RISCV.Base
  ( base
  , rv32i
  , rv64i
  )
  where

import Data.Monoid
import qualified Data.Parameterized.Map as Map
import Data.Parameterized
import GHC.TypeLits

import RISCV.Instruction
import RISCV.InstructionSet
import RISCV.Semantics
import RISCV.Semantics.Helpers

-- RV32I
rv32i :: InstructionSet 'RV32I
rv32i = base

-- RV64I
rv64i :: InstructionSet 'RV64I
rv64i = base <> base64

base :: KnownNat (ArchWidth arch) => InstructionSet arch
base = instructionSet baseEncode baseSemantics

base64 :: InstructionSet 'RV64I
base64 = instructionSet base64Encode base64Semantics

baseEncode :: EncodeMap
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
  , Pair Srli   (IOpBits 0b0010011 0b101)
  , Pair Srai   (IOpBits 0b0010011 0b101)
  , Pair Fence  (IOpBits 0b0001111 0b000)
  , Pair FenceI (IOpBits 0b0001111 0b001)
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

  -- E type
  , Pair Ecall  (EOpBits 0b1110011 0b0000000000000000000000000)
  , Pair Ebreak (EOpBits 0b1110011 0b0000000000010000000000000)

  -- X type
  , Pair Illegal XOpBits
  ]

baseSemantics :: KnownNat (ArchWidth arch) => SemanticsMap arch
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

      rOp srlE
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

      ls (knownNat :: NatRepr 1)
  , Pair Lh $ getFormula $ do
      comment "Loads a half-word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], sign-extending the result."

      ls (knownNat :: NatRepr 2)
  , Pair Lw $ getFormula $ do
      comment "Loads a word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], sign-extending the result."

      ls (knownNat :: NatRepr 4)
  , Pair Lbu $ getFormula $ do
      comment "Loads a byte from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], zero-extending the result."

      lu (knownNat :: NatRepr 1)
  , Pair Lhu $ getFormula $ do
      comment "Loads a half-word from memory at address x[rs1] + sext(offset)."
      comment "Writes the result to x[rd], zero-extending the result."

      lu (knownNat :: NatRepr 2)
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
  , Pair Srli $ getFormula $ do
      comment "Shifts register x[rs1] right by shamt bit positions."
      comment "The vacated bits are filled with zeros, and the result is written to x[rd]."

      iOp srlE
  , Pair Srai $ getFormula $ do
      comment "Shifts register x[rs1] left by shamt bit positions."
      comment "The vacated bits are filled with copies of x[rs1]'s most significant bit."
      comment "The result is written to x[rd]."

      iOp sllE

  -- TODO: Fence/csr instructions.
  -- , Pair Fence   undefined
  -- , Pair FenceI undefined
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

      (rs1, rs2, offset) <- params

      x_rs1 <- regRead rs1
      x_rs2 <- regRead rs2

      x_rs2_byte <- extractEWithRepr (knownNat :: NatRepr 8) 0 x_rs2
      sext_offset <- sextE offset
      addr <- x_rs1 `addE` sext_offset

      assignMem addr x_rs2_byte
      incrPC
  , Pair Sh $ getFormula $ do
      comment "Computes the least-significant half-word in register x[rs2]."
      comment "Stores the result at memory address x[rs1] + sext(offset)."

      (rs1, rs2, offset) <- params

      x_rs1 <- regRead rs1
      x_rs2 <- regRead rs2

      x_rs2_byte <- extractEWithRepr (knownNat :: NatRepr 16) 0 x_rs2
      sext_offset <- sextE offset
      addr <- x_rs1 `addE` sext_offset

      assignMem addr x_rs2_byte
      incrPC
  , Pair Sw $ getFormula $ do
      comment "Computes the least-significant word in register x[rs2]."
      comment "Stores the result at memory address x[rs1] + sext(offset)."

      (rs1, rs2, offset) <- params

      x_rs1 <- regRead rs1
      x_rs2 <- regRead rs2

      x_rs2_byte <- extractEWithRepr (knownNat :: NatRepr 32) 0 x_rs2
      sext_offset <- sextE offset
      addr <- x_rs1 `addE` sext_offset

      assignMem addr x_rs2_byte
      incrPC

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
      imm20 <- sextE imm20'

      pc <- pcRead
      incr_pc <- pc `addE` ib
      pc_offset <- pc `addE` imm20

      assignReg rd incr_pc
      assignPC pc_offset

  -- E type
  , Pair Ecall $ getFormula $ do
      comment "Makes a request of the execution environment by raising an Environment Call exception."

      raiseException EnvironmentCall
  , Pair Ebreak $ getFormula $ do
      comment "Makes a request of the debugger by reaising a Breakpoint exception."

      raiseException Breakpoint

  -- X type
  , Pair Illegal $ getFormula $ do
      comment "Raise an IllegalInstruction exception"

      raiseException IllegalInstruction
  ]

base64Encode :: EncodeMap
base64Encode = undefined

base64Semantics :: SemanticsMap 'RV64I
base64Semantics = undefined

-- FIXME: Is there any way to replace the constraint here with something more
-- reasonable?
iOp :: KnownNat (ArchWidth arch) => ArithOp arch 'I -> FormulaBuilder arch 'I ()
-- iOp :: KnownRepr ArchRepr arch => ArithOp arch 'I -> FormulaBuilder arch 'I ()
iOp op = do
  (rd, rs1, imm12) <- params

  x_rs1 <- regRead rs1
  sext_imm12 <- sextE imm12

  result <- op x_rs1 sext_imm12
  assignReg rd result
  incrPC

ls :: KnownNat (ArchWidth arch) => NatRepr bytes -> FormulaBuilder arch 'I ()
ls bRepr = do
  (rd, rs1, offset) <- params

  x_rs1 <- regRead rs1
  sext_offset <- sextE offset
  addr <- x_rs1 `addE` sext_offset
  m_byte  <- memReadWithRepr bRepr addr
  sext_byte <- sextE m_byte

  assignReg rd sext_byte
  incrPC

lu :: KnownNat (ArchWidth arch) => NatRepr bytes -> FormulaBuilder arch 'I ()
lu bRepr = do
  (rd, rs1, offset) <- params

  x_rs1 <- regRead rs1
  sext_offset <- sextE offset
  addr <- x_rs1 `addE` sext_offset
  m_byte  <- memReadWithRepr bRepr addr
  zext_byte <- zextE m_byte

  assignReg rd zext_byte
  incrPC


type CompOp arch fmt = BVExpr arch (ArchWidth arch)
                    -> BVExpr arch (ArchWidth arch)
                    -> FormulaBuilder arch fmt (BVExpr arch 1)

b :: KnownNat (ArchWidth arch) => CompOp arch 'B -> FormulaBuilder arch 'B ()
b cmp = do
  (rs1, rs2, offset) <- params

  x_rs1 <- regRead rs1
  x_rs2 <- regRead rs2
  cond  <- x_rs1 `cmp` x_rs2

  pc <- pcRead
  ib' <- instBytes
  ib <- zextE ib'
  sext_offset <- sextE offset
  pc_branch <- pc `addE` sext_offset
  pc_incr <- pc `addE` ib

  new_pc <- iteE cond pc_branch pc_incr
  assignPC new_pc


-- TODO: Why doesn't this work?
-- s :: KnownNat (ArchWidth arch) => NatRepr bytes -> FormulaBuilder arch 'S ()
-- s bRepr = do
--   (rs1, rs2, offset) <- params

--   x_rs1 <- regRead rs1
--   x_rs2 <- regRead rs2

--   x_rs2_byte <- extractEWithRepr (8 `natMultiply` bRepr) 0 x_rs2
--   sext_offset <- sextE offset
--   addr <- x_rs1 `addE` sext_offset

--   assignMem addr x_rs2_byte
