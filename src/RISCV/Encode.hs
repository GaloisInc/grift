{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : RISCV.Encode
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

TODO module description
-}

module RISCV.Encode
  ( encode
  ) where

import Data.Word (Word32)

import RISCV.Instruction

-- placeBitsSigned :: Int -> String -> Int -> Int -> Integer -> Either ParseException Word16
-- placeBitsSigned ln lineStr lo hi w = do
--   let bits = hi - lo + 1
--   if w `fitsBitsSigned` bits
--     then return $ (fromIntegral w .&. (bit bits - 1)) `shiftL` lo
--     else Left $ OperandWidthError ln w bits lineStr

-- placeBitsUnsigned :: Int -> String -> Int -> Int -> Integer -> Either ParseException Word16
-- placeBitsUnsigned ln lineStr lo hi w = do
--   let bits = hi - lo + 1
--   if w `fitsBits` bits
--     then return $ (fromIntegral w .&. (bit bits - 1)) `shiftL` lo
--     else Left $ OperandWidthError ln w bits lineStr



-- | Encode an RV32I instruction as a 32-bit word.
encode :: forall (k :: Format). Instruction k -> Word32
encode = undefined

encodeOperands :: forall k. Operands k -> Word32
encodeOperands = undefined
--encodeOperands (ROperands rd rs1 rs2) =
