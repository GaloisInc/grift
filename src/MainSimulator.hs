{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import           Control.Lens ( (^..) )
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.IArray
import           Data.Bits
import           Data.BitVector.Sized
import           Data.Bool
import qualified Data.ByteString as BS
import           Data.Monoid
import           Data.Parameterized
import           System.Environment
import           System.Exit
import           Data.ElfEdit
import           GHC.TypeLits

import           RISCV.Types
import           RISCV.Instruction
import           RISCV.InstructionSet
import           RISCV.Decode
import           RISCV.Extensions
import           RISCV.Simulation
import           RISCV.Simulation.IOMachine

type SimArch = RV64I
type SimExts = (Exts '(MYes, FDNo))

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ do
    putStrLn "Use: riscv-sim steps elfFile"
    exitFailure

  let [stepStr, fileName] = args
  stepsToRun <- return $ (read stepStr :: Int)

  fileBS <- BS.readFile fileName
  case parseElf fileBS of
    Elf64Res _err e -> do
      let byteStrings = elfBytes e
      (pc, registers, _, steps, err) <- do
        m :: IOMachine SimArch SimExts <- mkIOMachine
             0x1000000
             (fromIntegral $ elfEntry e)
             byteStrings
        execIOMachine (runRV stepsToRun) m
      case err of
        Nothing -> return ()
        Just err' -> putStrLn $ "Encountered exception: " ++ show err'
      putStrLn $ "Executed " ++ show steps ++ " instructions."
      putStrLn $ "Final PC: " ++ show pc
      putStrLn $ "Final register state:"
      forM_ (assocs registers) $ \(r, v) -> do
        putStrLn $ "  R[" ++ show r ++ "] = " ++ show v


-- | From an Elf file, get a list of the byte strings to load into memory along with
-- their starting addresses.
elfBytes :: (KnownNat w, ElfWidthConstraints w) => Elf w -> [(BitVector w, BS.ByteString)]
elfBytes e = pairWithAddr <$> filter memoryMapped sections
  where sections = e ^.. elfSections
        memoryMapped section = elfSectionAddr section > 0
        pairWithAddr section = (fromIntegral $ elfSectionAddr section, elfSectionData section)
