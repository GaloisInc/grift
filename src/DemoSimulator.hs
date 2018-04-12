{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : MainSimulator
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Tool for simulating RISC-V programs in the ELF executable format.
-}

module Main where

import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString as BS
import Data.Array.IArray
import Data.BitVector.Sized
import Data.Parameterized
import Numeric
import System.Environment

import RISCV.Encode
import RISCV.Extensions
import RISCV.Instruction
import RISCV.InstructionSet
import RISCV.Simulation
import RISCV.Simulation.STMachine
import RISCV.Types

type BaseArchType = RV32I
type ExtensionsType = Exts '(MYes, FDNo)

type ISetType = InstructionSet BaseArchType ExtensionsType

iwordBS :: BitVector 32 -> BS.ByteString
iwordBS bv = BS.pack $ fromIntegral <$> bvIntegerU <$>
  ([ bvExtract 0 bv
   , bvExtract 8 bv
   , bvExtract 16 bv
   , bvExtract 24 bv] :: [BitVector 8])

prog :: [Some (Instruction BaseArchType)]
prog =
  [ Some $ Inst Lui (UOperands 5 0xAAAAA)
  , Some $ Inst Ori (IOperands 5 5 0xAAA)
  , Some $ Inst Ecb (IOperands 0 0 0)
  ]

-- sum integers 1 to 10.
prog1 :: [Some (Instruction BaseArchType)]
prog1 =
  [ Some $ Inst Addi (IOperands 1 0 0xf5)  -- counter, init to 10
  , Some $ Inst Slli (IOperands 1 1 12)
--  , Some $ Inst
  , Some $ Inst Addi (IOperands 2 0 0)   -- sum, init to 0
    -- begin loop
  , Some $ Inst Add  (ROperands 2 2 1)    -- add counter to sum
  , Some $ Inst Addi (IOperands 1 1 (-1)) -- decrement counter
  , Some $ Inst Blt  (BOperands 0 1 (-8)) -- if 0 < counter, goto top of loop
    -- end loop
  , Some $ Inst Ecb (IOperands 0 0 0)    -- trap
  ]

-- factorial
prog2 :: [Some (Instruction BaseArchType)]
prog2 =
  [ Some $ Inst Addi (IOperands 1 0 4)   -- input
  , Some $ Inst Addi (IOperands 2 0 1)   -- product, init to 1
    -- begin loop
  , Some $ Inst Mul  (ROperands 2 2 1)    -- multiply sum by counter
  , Some $ Inst Addi (IOperands 1 1 (-1)) -- decrement counter
  , Some $ Inst Blt  (BOperands 0 1 (-8)) -- if 0 < counter, goto top of loop
    -- end loop
  , Some $ Inst Ecb (IOperands 0 0 0)    -- trap
  ]

progBytes :: BS.ByteString
progBytes = BS.concat $ iwordBS <$> viewSome (encode (knownISet :: ISetType)) <$> prog1

main :: IO ()
main = do
  let (pc, registers, _, steps) = runST $ do
        m <- mkSTMachine
          (knownRepr :: BaseArchRepr BaseArchType)
          (knownRepr :: ExtensionsRepr ExtensionsType)
          0x0
          0x10000
          [(0, progBytes)]
        execSTMachine (runRV 10000000) m
  putStrLn $ "Executed " ++ show steps ++ " instructions."
  putStrLn $ "Final PC: " ++ show pc
  putStrLn $ "Final register state:"
  forM_ (assocs registers) $ \(r, v) -> do
    putStrLn $ "  R[" ++ show r ++ "] = " ++ show v
