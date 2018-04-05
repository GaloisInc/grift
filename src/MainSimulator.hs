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
import System.Environment

import RISCV.Simulation
import RISCV.Simulation.STMachine
import RISCV.Types

truncBS :: BS.ByteString -> BS.ByteString
truncBS bs = BS.pack (chunks (BS.unpack bs))
  where chunks (a:b:c:d:rst) = a:b:c:d:chunks rst
        chunks _ = []

main :: IO ()
main = return ()
-- main = do
--   (progFile:[]) <- getArgs

--   progBytes <- truncBS <$> BS.readFile progFile
--   let (pc, registers, _) = runST $ do
--         m <- mkSTMachine RV32IRepr (ExtensionsRepr MNoRepr FDNoRepr) 0x10000 progBytes
--         execSTMachine (runRV 1000) m
--   putStrLn $ "Final PC: " ++ show pc
--   putStrLn $ "Final register state:"
--   forM_ (assocs registers) $ \(r, v) -> do
--     putStrLn $ "  R[" ++ show r ++ "] = " ++ show v


