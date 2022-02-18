{-
This file is part of GRIFT (Galois RISC-V ISA Formal Tools).

GRIFT is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GRIFT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero Public License for more details.

You should have received a copy of the GNU Affero Public License
along with GRIFT.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : GRIFT.Simulation.LogMachine
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

An IO-based simulation backend for RISC-V machines. We use the 'UnsignedBV' type
directly for the underlying values, which allows us to keep the architecture width
unspecified.

This variant of LogMachine runs slower because it also logs coverage statistics.
-}

-- TODO: Abstract out instruction logging mechanism to be user-supplied.

module GRIFT.Simulation.LogMachine
  ( LogMachine(..)
  , mkLogMachine
  , mkLogMachineWithCovMap
  , buildCTMap
  , LogMachineM
  , TrackedOpcode(..)
  , freezeGPRs
  , freezeFPRs
  , runLogMachine
  , runLogMachineLog
  , InstCTList
  , pPrintInstCTList
  , countInstCTList
  ) where

import           Control.Lens ((^.))
import           Control.Monad (forM_)
import           Control.Monad.Reader.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader hiding (ask)
import           Data.Array.IArray
import           Data.Array.IO
import           Data.BitVector.Sized hiding (concat)
import           Data.BitVector.Sized.Unsigned (UnsignedBV(..))
import qualified Data.ByteString as BS
import           Data.Char (chr)
import           Data.Foldable hiding (or)
import           Data.IORef
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Parameterized
import           Data.Parameterized.List
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Map (MapF)
import           Data.Traversable (for)
import           GHC.TypeLits
import           Numeric.Natural (Natural)
import           Prelude hiding ((<>), or)
import           Text.PrettyPrint.HughesPJ

import GRIFT.BitVector.BVApp (BVApp(..))
import GRIFT.BitVector.BVFloatApp (BVFloatApp(..))
import GRIFT.InstructionSet
import GRIFT.InstructionSet.Known
import GRIFT.Types
import GRIFT.Semantics hiding (concat)
import GRIFT.Semantics.Expand
import GRIFT.Semantics.Pretty
import GRIFT.Semantics.Utils
import GRIFT.Simulation

import Debug.Trace (traceM, trace)

data TrackedOpcode rv = NoOpcode | AllOpcodes | SomeOpcode (Some (Opcode rv))

deriving instance Show (TrackedOpcode rv)

instance ShowF TrackedOpcode where
  showF = show

-- TODO: Coverage map should also contain a single bit for each opcode tracking
-- whether that opcode was ever encountered at all.
-- | IO-based machine state.
data LogMachine (rv :: RV) = LogMachine
  { lmRV         :: RVRepr rv
  , lmPC         :: IORef (UnsignedBV (RVWidth rv))
  , lmGPRs  :: IOArray (UnsignedBV 5) (UnsignedBV (RVWidth rv))
  , lmFPRs :: IOArray (UnsignedBV 5) (UnsignedBV (RVFloatWidth rv))
  , lmMemory     :: IORef (Map (UnsignedBV (RVWidth rv)) (UnsignedBV 8))
--  , lmMemory     :: IOArray (UnsignedBV (RVWidth rv)) (UnsignedBV 8)
  , lmCSRs       :: IORef (Map (UnsignedBV 12) (UnsignedBV (RVWidth rv)))
  , lmPriv       :: IORef (UnsignedBV 2)
--  , lmMaxAddr    :: UnsignedBV (RVWidth rv)
  , lmHaltPC     :: IORef (Maybe (UnsignedBV (RVWidth rv)))
  , lmTrackedOpcode  :: IORef (TrackedOpcode rv)
  , lmCovMap     :: IORef (MapF (Opcode rv) (InstCTList rv))
  }

newtype InstCTList rv fmt = InstCTList [InstCT rv fmt]

writeBS :: (Enum i, Num i, Ix i) => i -> BS.ByteString -> IORef (Map i (UnsignedBV 8)) -> IO ()
writeBS ix bs mapRef = do
  case BS.null bs of
    True -> return ()
    _    -> do
      m <- readIORef mapRef
      let m' = Map.insert ix (fromIntegral (BS.head bs)) m
      writeIORef mapRef m'
      writeBS (ix+1) (BS.tail bs) mapRef
--      writeArray arr ix (fromIntegral (BS.head bs))
--      writeBS (ix+1) (BS.tail bs) arr

-- | Construct a complete, unvisited coverage map from an 'RVRepr'.
buildCTMap :: forall rv . RVRepr rv -> MapF (Opcode rv) (InstCTList rv)
buildCTMap rvRepr = withRVWidth (rvRepr) $
  let (InstructionSet _ _ semanticsMap) = knownISetWithRepr rvRepr
  in MapF.fromList (pairWithCT <$> MapF.keys semanticsMap)
  where
    pairWithCT :: (32 <= ArchWidth (RVBaseArch rv)) => Some (Opcode rv) -> Pair (Opcode rv) (InstCTList rv)
    pairWithCT (Some opcode) = Pair opcode (coverageTreeOpcode rvRepr opcode)

-- | Construct a 'LogMachine'.
mkLogMachine :: (32 <= ArchWidth (RVBaseArch rv))
             => RVRepr rv
             -> UnsignedBV (RVWidth rv)
             -> UnsignedBV (RVWidth rv)
             -> [(UnsignedBV (RVWidth rv), BS.ByteString)]
             -> Maybe (UnsignedBV (RVWidth rv))
             -> TrackedOpcode rv --Maybe (Some (Opcode rv))
             -> IO (LogMachine rv)
mkLogMachine rvRepr entryPoint sp byteStrings haltPC opcodeCov = withRV rvRepr $ do
  pc         <- newIORef entryPoint
  registers  <- newArray (1, 31) 0
  fregisters <- newArray (0, 31) 0
  memory     <- newIORef $ Map.fromList [ ]
  csrs       <- newIORef $ Map.fromList [ ]
  priv       <- newIORef 0b11 -- M mode by default.
  haltPCRef  <- newIORef haltPC
  opcodeRef  <- newIORef opcodeCov
  covMapRef  <- newIORef (buildCTMap rvRepr)

  -- set up stack pointer
  writeArray registers 2 sp

  forM_ byteStrings $ \(addr, bs) ->
    writeBS addr bs memory
  return (LogMachine
           rvRepr pc registers fregisters memory csrs
           priv haltPCRef opcodeRef covMapRef)

-- | Construct a 'LogMachine' with a given coverage map.
mkLogMachineWithCovMap :: RVRepr rv
                       -> UnsignedBV (RVWidth rv)
                       -> UnsignedBV (RVWidth rv)
                       -> [(UnsignedBV (RVWidth rv), BS.ByteString)]
                       -> Maybe (UnsignedBV (RVWidth rv))
                       -> TrackedOpcode rv --Maybe (Some (Opcode rv))
                       -> IORef (MapF (Opcode rv) (InstCTList rv))
                       -> IO (LogMachine rv)
mkLogMachineWithCovMap rvRepr entryPoint sp byteStrings haltPC opcodeCov covMapRef = withRV rvRepr $ do
  pc         <- newIORef entryPoint
  registers  <- newArray (1, 31) 0
  fregisters <- newArray (0, 31) 0
  memory     <- newIORef $ Map.fromList [ ]
  csrs       <- newIORef $ Map.fromList [ ]
  priv       <- newIORef 0b11 -- M mode by default.
  haltPCRef  <- newIORef haltPC
  opcodeRef  <- newIORef opcodeCov

  -- set up stack pointer
  writeArray registers 2 sp

  forM_ byteStrings $ \(addr, bs) ->
    withRV rvRepr $ writeBS addr bs memory
  return (LogMachine
           rvRepr pc registers fregisters memory csrs
           priv haltPCRef opcodeRef covMapRef)

-- | The 'LogMachineM' monad instantiates the 'RVState' monad type class, tying the
-- 'RVState' interface functions to actual transformations on the underlying mutable
-- state.
newtype LogMachineM (rv :: RV) a =
  LogMachineM { runLogMachineM :: ReaderT (LogMachine rv) IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (LogMachine rv)
           , MonadIO
           )

-- | Concatenate a list of 'UnsignedBV's into an 'UnsignedBV' of arbitrary width. The ordering is little endian:
--
-- >>> bvConcatMany' [0xAA :: UnsignedBV 8, 0xBB] :: UnsignedBV 16
-- 0xbbaa
-- >>> bvConcatMany' [0xAA :: UnsignedBV 8, 0xBB, 0xCC] :: UnsignedBV 16
-- 0xbbaa
--
-- If the sum of the widths of the input 'UnsignedBV's exceeds the output width, we
-- ignore the tail end of the list.
bvConcatMany' :: forall w. NatRepr w -> [UnsignedBV 8] -> UnsignedBV w
bvConcatMany' repr uBvs = UnsignedBV $ foldl' go (zero repr) (zip [0..] uBvs)
  where
    go :: BV w -> (Natural, UnsignedBV 8) -> BV w
    go acc (i, UnsignedBV bv) =
      let bvExt = zresize repr bv in
      let bvShifted = shl repr bvExt (i * 8) in
      or acc bvShifted

-- | Given a 'BV' of arbitrary length, decompose it into a list of bytes. Uses
-- an unsigned interpretation of the input vector, so if you ask for more bytes that
-- the 'BV' contains, you get zeros. The result is little-endian, so the first
-- element of the list will be the least significant byte of the input vector.
bvGetBytesU :: Int -> BV w -> [BV 8]
bvGetBytesU n _ | n <= 0 = []
bvGetBytesU n bv = map go [0..(n-1)]
  where
    go i = select' ((fromIntegral i) * 8) (knownNat @8) bv

instance (KnownRV rv) => RVStateM (LogMachineM rv) rv where
  getRV = lmRV <$> ask
  getPC = do
    pcRef <- lmPC <$> ask
    pcVal <- liftIO $ readIORef pcRef
    return pcVal
  getGPR rid = do
    regArray <- lmGPRs <$> ask
    regVal   <- liftIO $ readArray regArray rid
    return regVal
  getFPR rid = do
    regArray <- lmFPRs <$> ask
    regVal   <- liftIO $ readArray regArray rid
    return regVal
  getMem bytes addr = do
    memRef <- lmMemory <$> ask
    m <- liftIO $ readIORef memRef
    rv <- lmRV <$> ask
    withRV rv $ do
      let val = fmap (\a -> Map.findWithDefault 0 a m) [addr..addr+(fromIntegral (natValue bytes-1))]
      return (bvConcatMany' ((knownNat @8) `natMultiply` bytes) val)
  getCSR csr = do
    csrsRef <- lmCSRs <$> ask
    csrMap  <- liftIO $ readIORef csrsRef
    rv <- lmRV <$> ask
    let csrVal = case Map.lookup csr csrMap of
          Just val -> val
          Nothing  -> withRV rv 0 -- TODO: throw exception?
    return csrVal
  getPriv = LogMachineM $ do
    privRef <- lmPriv <$> ask
    privVal <- lift $ readIORef privRef
    return privVal

  setPC pcVal = do
    pcRef <- lmPC <$> ask
    liftIO $ writeIORef pcRef pcVal
  setGPR rid regVal = do
    regArray <- lmGPRs <$> ask
    liftIO $ writeArray regArray rid regVal
  setFPR rid regVal = do
    regArray <- lmFPRs <$> ask
    liftIO $ writeArray regArray rid regVal
  setMem bytes addr (UnsignedBV val)
    | addr == 100 && natValue bytes == 1 = do
        liftIO $ putChar (chr $ fromIntegral $ asUnsigned val)
  setMem bytes addr (UnsignedBV val) = do
    memRef <- lmMemory <$> ask
    m <- liftIO $ readIORef memRef
    rv <- lmRV <$> ask
    withRV rv $
      let addrValPairs = zip
            [addr..addr+(fromIntegral (natValue bytes-1))]
            (UnsignedBV <$> bvGetBytesU (fromIntegral (natValue bytes)) val)
          m' = foldr (\(a, byte) mem -> Map.insert a byte mem) m addrValPairs
      in liftIO $ writeIORef memRef m'
  setCSR csr csrVal = do
    csrsRef <- lmCSRs <$> ask
    csrMap  <- liftIO $ readIORef csrsRef
    case Map.member csr csrMap of
      True  -> liftIO $ writeIORef csrsRef (Map.insert csr csrVal csrMap)
      False -> -- TODO: throw exception in this case
        liftIO $ writeIORef csrsRef (Map.insert csr csrVal csrMap)
  setPriv privVal = do
    privRef <- lmPriv <$> ask
    liftIO $ writeIORef privRef privVal

  isHalted = do
    haltPCRef <- lmHaltPC <$> ask
    haltPC <- liftIO $ readIORef haltPCRef
    case haltPC of
      Nothing -> return False
        -- mcause <- getCSR (encodeCSR MCause)
        -- return (mcause == 11) -- halt on M-mode ecall by default
      Just addr -> do
        pc <- getPC
        return (pc == addr)

  logInstruction iset inst@(Inst opcode _) iw = do
    mCovMapRef <- lmCovMap <$> ask
    mCovMap <- liftIO $ readIORef mCovMapRef
    mTrackedOpcodeRef <- lmTrackedOpcode <$> ask
    mTrackedOpcode <- liftIO $ readIORef mTrackedOpcodeRef
    rvRepr <- lmRV <$> ask
    let InstCTList covTrees = case MapF.lookup opcode mCovMap of
          Just covTrees' -> covTrees'
          _ -> coverageTreeOpcode rvRepr opcode
    case mTrackedOpcode of
      AllOpcodes -> do
        covTrees' <- traverse (evalInstCT iset inst iw) covTrees
        liftIO $ writeIORef mCovMapRef
          (MapF.insert opcode (InstCTList covTrees') mCovMap)
      SomeOpcode (Some covOpcode) -> case covOpcode `testEquality` opcode of
        Just Refl -> do
          covTrees' <- traverse (evalInstCT iset inst iw) covTrees
          liftIO $ writeIORef mCovMapRef
            (MapF.insert opcode (InstCTList covTrees') mCovMap)
        _ -> return ()
      _ -> return ()

-- | Create an immutable copy of the register file.
freezeGPRs :: LogMachine rv
                -> IO (Array (UnsignedBV 5) (UnsignedBV (RVWidth rv)))
freezeGPRs = freeze . lmGPRs

-- | Create an immutable copy of the floating point register file.
freezeFPRs :: LogMachine rv
                 -> IO (Array (UnsignedBV 5) (UnsignedBV (RVFloatWidth rv)))
freezeFPRs = freeze . lmFPRs

-- | Run the simulator for a given number of steps.
runLogMachine :: KnownRV rv => Int -> LogMachine rv -> IO Int
runLogMachine steps m = flip runReaderT m $ runLogMachineM $ runRV steps

-- | Like runLogMachine, but log each instruction.
runLogMachineLog :: KnownRV rv => Int -> LogMachine rv -> IO Int
runLogMachineLog steps m = flip runReaderT m $ runLogMachineM $ runRVLog steps

-- | A 'CTNode' contains an expression and a flag indicating whether or not that
-- expression has been evaluated.
data CTNode (expr :: Nat -> *) (w :: Nat) = CTNode Bool Bool (expr w)

data CT (expr :: Nat -> *) = CT (CTNode expr 1) [CT expr] [CT expr] [CT expr]

sumPair (a, b) (c, d) = (a+c, b+d)

countCT :: CT expr -> (Int, Int)
countCT (CT (CTNode tTaken fTaken _) testCTs tCTs fCTs) =
  let ctsHit = testCTs ++ tCTs ++ fCTs
  in foldl sumPair ((if tTaken then 1 else 0) + (if fTaken then 1 else 0),2) (countCT <$> ctsHit)

-- | Evaluate a coverage tree and recursively flag all evaluated nodes.
evalCT :: (RVStateM m rv, KnownRV rv)
       => InstructionSet rv
       -> Instruction rv fmt
       -> Integer
       -> CT (InstExpr fmt rv)
       -> m (CT (InstExpr fmt rv))
evalCT iset inst iw (CT (CTNode t f testExpr) testTrees trueTrees falseTrees) = do
  testResult  <- evalInstExpr iset inst iw testExpr
  testTrees' <- traverse (evalCT iset inst iw) testTrees
  case testResult of
    0b1 -> do
      trueTrees' <- traverse (evalCT iset inst iw) trueTrees
      return $ CT (CTNode True f testExpr) testTrees' trueTrees' falseTrees
    _ -> do
      falseTrees' <- traverse (evalCT iset inst iw) falseTrees
      return $ CT (CTNode t True testExpr) testTrees' trueTrees falseTrees'

evalInstCT :: (RVStateM m rv, KnownRV rv)
           => InstructionSet rv
           -> Instruction rv fmt
           -> Integer
           -> InstCT rv fmt
           -> m (InstCT rv fmt)
evalInstCT iset inst iw (InstCT ct) = do
  ct' <- evalCT iset inst iw ct
  return (InstCT ct')

color2 :: Bool -> Bool -> Doc -> Doc
color2 True True = green
color2 True False = cyan
color2 False True = yellow
color2 False False = red

red :: Doc -> Doc
red doc = text "\x1b[31m" <> doc <> text "\x1b[0m"

cyan :: Doc -> Doc
cyan doc = text "\x1b[36m" <> doc <> text "\x1b[0m"

yellow :: Doc -> Doc
yellow doc = text "\x1b[33m" <> doc <> text "\x1b[0m"

green :: Doc -> Doc
green doc = text "\x1b[32m" <> doc <> text "\x1b[0m"

pPrintCT :: KnownRV rv
         => List OperandName (OperandTypes fmt)
         -> CT (InstExpr fmt rv)
         -> Doc
pPrintCT opNames (CT (CTNode t f e) [] [] []) = color2 t f (pPrintInstExpr opNames NoAbbrev True e)
pPrintCT opNames (CT (CTNode t f e) ts ls rs) = (color2 t f (pPrintInstExpr opNames NoAbbrev True e))
  $$ nest 2 (text "?>" <+> vcat (pPrintCT opNames <$> ts))
  $$ nest 2 (text "t>" <+> vcat (pPrintCT opNames <$> ls))
  $$ nest 2 (text "f>" <+> vcat (pPrintCT opNames <$> rs))

pPrintInstCT :: KnownRV rv
             => List OperandName (OperandTypes fmt)
             -> InstCT rv fmt
             -> Doc
pPrintInstCT opNames (InstCT ct) = pPrintCT opNames ct

pPrintInstCTList :: RVRepr rv
                 -> Opcode rv fmt
                 -> InstCTList rv fmt
                 -> [Doc]
pPrintInstCTList rvRepr opcode (InstCTList instCTs) =
  case MapF.lookup opcode (isSemanticsMap $ knownISetWithRepr rvRepr) of
    Nothing -> []
    Just sem -> withRV rvRepr $ pPrintInstCT (getOperandNames sem) <$> instCTs

-- Semantic coverage
coverageTreeLocApp :: KnownRV rv => LocApp (InstExpr fmt rv) rv w -> [CT (InstExpr fmt rv)]
coverageTreeLocApp (GPRApp _ e) = coverageTreeInstExpr e
coverageTreeLocApp (FPRApp _ e) = coverageTreeInstExpr e
coverageTreeLocApp (MemApp _ e) = coverageTreeInstExpr e
coverageTreeLocApp (ResApp e) = coverageTreeInstExpr e
coverageTreeLocApp (CSRApp _ e) = coverageTreeInstExpr e
coverageTreeLocApp _ = []

coverageTreeStateApp :: KnownRV rv => StateApp (InstExpr fmt rv) rv w -> [CT (InstExpr fmt rv)]
coverageTreeStateApp (LocApp e) = coverageTreeLocApp e
coverageTreeStateApp (AppExpr e) = coverageTreeBVApp e
coverageTreeStateApp (FloatAppExpr e) = coverageTreeBVFloatApp e

coverageTreeInstExpr :: KnownRV rv => InstExpr fmt rv w -> [CT (InstExpr fmt rv)]
coverageTreeInstExpr (InstStateApp e) = coverageTreeStateApp e
coverageTreeInstExpr (InstAbbrevApp abbrevApp) =
  coverageTreeInstExpr (expandAbbrevApp abbrevApp)
coverageTreeInstExpr _ = []

coverageTreeBVApp :: KnownRV rv => BVApp (InstExpr fmt rv) w -> [CT (InstExpr fmt rv)]
coverageTreeBVApp (IteApp _ t l r) =
  [CT (CTNode False False t) (coverageTreeInstExpr t) (coverageTreeInstExpr l) (coverageTreeInstExpr r)]
coverageTreeBVApp app = foldMapFC coverageTreeInstExpr app

coverageTreeBVFloatApp :: KnownRV rv => BVFloatApp (InstExpr fmt rv) w -> [CT (InstExpr fmt rv)]
coverageTreeBVFloatApp app = foldMapFC coverageTreeInstExpr app

coverageTreeStmt :: KnownRV rv => Stmt (InstExpr fmt) rv -> [CT (InstExpr fmt rv)]
coverageTreeStmt (AssignStmt loc e) = coverageTreeLocApp loc ++ coverageTreeInstExpr e
coverageTreeStmt (AbbrevStmt abbrevStmt) = coverageTreeStmt `concatMap` expandAbbrevStmt abbrevStmt
coverageTreeStmt (BranchStmt t l r) =
  let tTrees = coverageTreeInstExpr t
      lTrees = concat $ toList $ coverageTreeStmt <$> l
      rTrees = concat $ toList $ coverageTreeStmt <$> r
  in [CT (CTNode False False t) tTrees lTrees rTrees]

newtype InstCT rv fmt = InstCT (CT (InstExpr fmt rv))

coverageTreeSemantics :: KnownRV rv => InstSemantics rv fmt -> [InstCT rv fmt]
coverageTreeSemantics (InstSemantics sem _) =
  let stmts = sem ^. semStmts
      trees = concat $ toList $ coverageTreeStmt <$> stmts
  in InstCT <$> trees

coverageTreeOpcode :: 32 <= ArchWidth (RVBaseArch rv)
                   => RVRepr rv
                   -> Opcode rv fmt
                   -> InstCTList rv fmt
coverageTreeOpcode rvRepr opcode =
  let iset = knownISetWithRepr rvRepr
  in case MapF.lookup opcode (isSemanticsMap iset) of
    Nothing -> InstCTList []
    Just sem -> withRV rvRepr $ InstCTList (coverageTreeSemantics sem)

countInstCT :: InstCT rv fmt -> (Int, Int)
countInstCT (InstCT ct) = countCT ct

countInstCTList :: InstCTList rv fmt -> (Int, Int)
countInstCTList (InstCTList covTrees) = foldl sumPair (0,0) (countInstCT <$> covTrees)
