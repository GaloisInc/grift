{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Bits
import           Data.BitVector.Sized
import           Data.Bool
import qualified Data.ByteString as BS
import           Data.ElfEdit
import           Data.Monoid
import           Data.Parameterized
import           Data.STRef
import           System.Environment
import           System.Exit

import           RISCV

type W = ArchWidth 'RV32I

byteSize :: NatRepr 8
byteSize = knownRepr

data Machine s = Machine
  { stPC        :: STRef s (BitVector W)
  , stRegisters :: STArray s (BitVector 5) (BitVector W)
  , stMemory    :: STArray s (BitVector W) (BitVector 8)
  }

newtype MachineM s a = MachineM { runMachineM :: ReaderT (Machine s) (ST s) a }
  deriving (Functor, Applicative, Monad)

instance RVState (MachineM s) 'RV32I where
  getInstructionSet = return $ rv32i <> m32

  getPC = MachineM $ do
    pcRef <- stPC <$> ask
    pcVal <- lift $ readSTRef pcRef
    return pcVal
  getReg 0 = return 0 -- rid 0 is hardwired to the constant 0.
  getReg rid = MachineM $ do
    regArray <- stRegisters <$> ask
    regVal   <- lift $ readArray regArray rid
    return regVal
  getMem bRepr addr = MachineM $ do
    let numBytes = bitVector (natValue bRepr)
    memArray <- stMemory <$> ask
    bytes <- forM [addr .. addr + numBytes - 1] (lift . readArray memArray)
    return (bvConcatManyWithRepr (byteSize `natMultiply` bRepr) bytes)

  setPC pcVal = MachineM $ do
    pcRef <- stPC <$> ask
    lift $ writeSTRef pcRef pcVal
  setReg rid regVal = MachineM $ do
    regArray <- stRegisters <$> ask
    lift $ writeArray regArray rid regVal
  setMem _ 0 _ = return ()
  setMem bRepr addr bv = MachineM $ do
    let numBytes = natValue bRepr
        writes   = zip [ addr .. addr + (bitVector numBytes) - 1] (bvGetBytesU (fromIntegral numBytes) bv)
    memArray <- stMemory <$> ask
    forM_ writes (lift . uncurry (writeArray memArray))

main :: IO ()
main = do
  args <- getArgs
  when (length args==0) $ do
    putStrLn "Please supply a RISC-V ELF binary."
    exitFailure

  -- load in each file as a bytestring
  forM_ args $ \fileName -> do
    insts <- disFile fileName
    forM_ insts $ \(inst, instBV) -> do
      putStrLn $ show instBV ++ ": " ++ show inst

-- | Read in an ELF file, find the code sections, and write them to a file as raw
-- bytes
asmFile :: String -> [BitVector 32] -> IO ()
asmFile fileName bvs = do
  let bvToBytes bv = [ bvExtract 0  bv :: BitVector 8
                     , bvExtract 8  bv :: BitVector 8
                     , bvExtract 16 bv :: BitVector 8
                     , bvExtract 24 bv :: BitVector 8
                     ]
      bytes = BS.pack $ fromIntegral <$> bvIntegerU <$> concat (bvToBytes <$> bvs)
  BS.writeFile fileName bytes

disFile :: String -> IO [(Some (Instruction 'RV32I), BitVector 32)]
disFile fileName = do
  fileBS <- BS.readFile fileName
  case parseElf fileBS of
    Elf32Res _err e -> disElf e
    Elf64Res _err e -> disElf e
    ElfHeaderError _byteOffset _msg -> return []

disElf :: ElfWidthConstraints w => Elf w -> IO [(Some (Instruction 'RV32I), BitVector 32)]
disElf e = do
  let [textSection] = findSectionByName ".text" e
      bs  = elfSectionData textSection
      dis = disInstructions bs
  return dis

-- | Decode a single instruction from a bytestring and returns the instruction along
-- with the remaining bytes and the number of bytes consumed. Since we currently only
-- support RV32I, this function only decodes 4-byte words.
disInstruction :: BS.ByteString -> Maybe (Some (Instruction 'RV32I), BitVector 32, BS.ByteString, Int)
disInstruction bs =
  bool Nothing (Just (inst, instBV, rb, numBytes)) (BS.length bs >= 4)
  where (ib, rb) = BS.splitAt 4 bs
        (b0:b1:b2:b3:[]) = fromIntegral <$> BS.unpack ib :: [Integer]
        instWordI = b3 `shiftL` 24 .|.
                    b2 `shiftL` 16 .|.
                    b1 `shiftL` 8  .|.
                    b0
        instBV = bitVector instWordI
        inst = decode (rv32i <> m32 :: InstructionSet 'RV32I) instBV
        numBytes = 4

-- | Decode a bytestring as a RISC-V program
disInstructions :: BS.ByteString -> [(Some (Instruction 'RV32I), BitVector 32)]
disInstructions bs = case disInstruction bs of
  -- not enough bytes to disassemble an instruction. We should probably detect if
  -- length bs < 4 and give some kind of warning, but I'm not worried about that for now.
  Nothing -> []
  Just (inst, instBV, bsRst, _) -> (inst, instBV) : disInstructions bsRst
