{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

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
import           Data.Array.IArray
import           Data.BitVector.Sized
import qualified Data.ByteString as BS
import           Data.IORef
import qualified Data.Map as Map
import           Data.Parameterized
import           System.Environment
import           System.Exit
import           Data.ElfEdit
import           GHC.TypeLits
import           System.FilePath.Posix

import           RISCV.Types
import           RISCV.Simulation.LogMachine

type SimExts = (Exts '(MYes, AYes, FDNo))

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ do
    putStrLn "Use: riscv-sim steps elfFile"
    exitFailure

  let [stepStr, fileName] = args
      stepsToRun = read stepStr :: Int
      log = replaceExtensions fileName "log"

  fileBS <- BS.readFile fileName
  case parseElf fileBS of
    Elf32Res _err e -> do
      let byteStrings = elfBytes e
      m :: LogMachine RV32 SimExts <-
        mkLogMachine 0x1000000 (fromIntegral $ elfEntry e) byteStrings

      stepsRan  <- runLogMachine stepsToRun m
      err       <- readIORef (ioException m)
      pc        <- readIORef (ioPC m)
      registers <- freezeRegisters m
      testMap   <- readIORef (ioTestMap m)

      case err of
        Nothing -> return ()
        Just err' -> putStrLn $ "Encountered exception: " ++ show err'
      putStrLn $ "Executed " ++ show stepsRan ++ " instructions."
      putStrLn $ "Final PC: " ++ show pc
      putStrLn "Final register state:"
      forM_ (assocs registers) $ \(r, v) ->
        putStrLn $ "  R[" ++ show r ++ "] = " ++ show v

      writeFile log "Opcode, Coverage\n"

      forM_ (Map.assocs testMap) $ \(opcode, variants@(v:_)) ->
        appendFile log $ show opcode ++ ", " ++ show (length variants) ++
        "/" ++ show (2^length v) ++ "\n"

    Elf64Res _err e -> do
      let byteStrings = elfBytes e
      m :: LogMachine RV64 SimExts <-
        mkLogMachine 0x1000000 (fromIntegral $ elfEntry e) byteStrings
      runLogMachine stepsToRun m

      err        <- readIORef (ioException m)
      stepsRan   <- readIORef (ioSteps m)
      pc         <- readIORef (ioPC m)
      registers  <- freezeRegisters m
      testMap   <- readIORef (ioTestMap m)

      case err of
        Nothing -> return ()
        Just err' -> putStrLn $ "Encountered exception: " ++ show err'
      putStrLn $ "Executed " ++ show stepsRan ++ " instructions."
      putStrLn $ "Final PC: " ++ show pc
      putStrLn "Final register state:"
      forM_ (assocs registers) $ \(r, v) ->
        putStrLn $ "  R[" ++ show r ++ "] = " ++ show v

      writeFile log "Opcode, Coverage\n"

      forM_ (Map.assocs testMap) $ \(opcode, variants@(v:_)) ->
        appendFile log $ show opcode ++ ", " ++ show (length variants) ++
        "/" ++ show (2^length v) ++ "\n"

-- | From an Elf file, get a list of the byte strings to load into memory along with
-- their starting addresses.
elfBytes :: (KnownNat w, ElfWidthConstraints w) => Elf w -> [(BitVector w, BS.ByteString)]
elfBytes e = pairWithAddr <$> filter memoryMapped sections
  where sections = e ^.. elfSections
        memoryMapped section = elfSectionAddr section > 0
        pairWithAddr section = (fromIntegral $ elfSectionAddr section, elfSectionData section)
