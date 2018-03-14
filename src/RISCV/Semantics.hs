{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module RISCV.Semantics where

import Data.Bits
import Data.BitVector.Sized
import Data.Parameterized
import Foreign.Marshal.Utils (fromBool)
import GHC.TypeLits

import RISCV.Instruction
import RISCV.Decode

type RegId = BitVector 5
type Addr = BitVector 32

class Monad s => RVSem32 s where
  getPC  :: s (BitVector 32)
  setPC  :: BitVector 32 -> s ()
  getReg :: RegId -> s (BitVector 32)
  setReg :: RegId -> BitVector 32 -> s ()
  getMem :: KnownNat w => Addr -> s (BitVector w)
  setMem :: Addr -> BitVector w -> s ()

rOp :: RVSem32 s
    => (BitVector 32 -> BitVector 32 -> BitVector 32)
    -> Operands 'R
    -> s ()
rOp op (ROperands rdID rs1ID rs2ID) = do
  rs1 <- getReg rs1ID
  rs2 <- getReg rs2ID
  setReg rdID (rs1 `op` rs2)

step :: RVSem32 s => s ()
step = do
  pc <- getPC
  inst <- getMem pc
  case decode inst of
    -- R type
    Some (Inst Add  operands) -> rOp (+) operands
    Some (Inst Sub  operands) -> rOp (-) operands
    Some (Inst And  operands) -> rOp (.&.) operands
    Some (Inst Or   operands) -> rOp (.|.) operands
    Some (Inst Xor  operands) -> rOp xor operands
    Some (Inst Slt  operands) -> rOp (\x y -> fromBool (x `bvLTS` y)) operands
    Some (Inst Sltu operands) -> rOp (\x y -> fromBool (x `bvLTU` y)) operands
    Some (Inst Sll  operands) ->
      rOp (\bv1 bv2 -> bv1 `bvShiftL` fromIntegral (bvIntegerU bv2)) operands
    Some (Inst Sra  operands) ->
      rOp (\bv1 bv2 -> bv1 `bvShiftRA` fromIntegral (bvIntegerU bv2)) operands
    Some (Inst Srl  operands) ->
      rOp (\bv1 bv2 -> bv1 `bvShiftRL` fromIntegral (bvIntegerU bv2)) operands

    -- I type
--    Some (Inst Jalr operands) -> undefined
    _ -> undefined
