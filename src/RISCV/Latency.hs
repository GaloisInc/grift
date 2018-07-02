{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-|
Module      : RISCV.Latency
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Experimental -- we are attaching cycle counts to each instruction, which will be
computed during simulation.
-}

module RISCV.Latency where

import qualified Data.Parameterized.Map as Map
import Data.Parameterized

import RISCV.Semantics
import RISCV.Types

newtype Flip t a b = Flip (t b a)

type LatencyMap arch exts = Map.MapF (Opcode arch) (Flip (Expr arch) (ArchWidth arch))

baseLatency :: KnownArch arch => LatencyMap arch exts
baseLatency = Map.fromList
  [ -- RV32I
    -- R type
    Pair Add  (Flip $ litBV 1)
  , Pair Sub  (Flip $ litBV 1)
  , Pair Sll  (Flip $ litBV 1)
  , Pair Slt  (Flip $ litBV 1)
  , Pair Sltu (Flip $ litBV 1)
  , Pair Xor  (Flip $ litBV 1)
  , Pair Srl  (Flip $ litBV 1)
  , Pair Sra  (Flip $ litBV 1)
  , Pair Or   (Flip $ litBV 1)
  , Pair And  (Flip $ litBV 1)

  -- I type
  , Pair Jalr   (Flip $ litBV 1)
  , Pair Lb     (Flip $ litBV 1)
  , Pair Lh     (Flip $ litBV 1)
  , Pair Lw     (Flip $ litBV 1)
  , Pair Lbu    (Flip $ litBV 1)
  , Pair Lhu    (Flip $ litBV 1)
  , Pair Addi   (Flip $ litBV 1)
  , Pair Slti   (Flip $ litBV 1)
  , Pair Sltiu  (Flip $ litBV 1)
  , Pair Xori   (Flip $ litBV 1)
  , Pair Ori    (Flip $ litBV 1)
  , Pair Andi   (Flip $ litBV 1)
  , Pair Slli   (Flip $ litBV 1)
  , Pair Sri    (Flip $ litBV 1)
  , Pair Fence  (Flip $ litBV 1)
  , Pair FenceI (Flip $ litBV 1)
  , Pair Ecb    (Flip $ litBV 1)
  , Pair Csrrw  (Flip $ litBV 1)
  , Pair Csrrs  (Flip $ litBV 1)
  , Pair Csrrc  (Flip $ litBV 1)
  , Pair Csrrwi (Flip $ litBV 1)
  , Pair Csrrsi (Flip $ litBV 1)
  , Pair Csrrci (Flip $ litBV 1)

  -- S type
  , Pair Sb (Flip $ litBV 1)
  , Pair Sh (Flip $ litBV 1)
  , Pair Sw (Flip $ litBV 1)

  -- B type
  , Pair Beq  (Flip $ litBV 1)
  , Pair Bne  (Flip $ litBV 1)
  , Pair Blt  (Flip $ litBV 1)
  , Pair Bge  (Flip $ litBV 1)
  , Pair Bltu (Flip $ litBV 1)
  , Pair Bgeu (Flip $ litBV 1)

  -- U type
  , Pair Lui   (Flip $ litBV 1)
  , Pair Auipc (Flip $ litBV 1)

  -- J type
  , Pair Jal (Flip $ litBV 1)

  -- X type
  , Pair Illegal (Flip $ litBV 1)
  ]

base64Latency :: (KnownArch arch, 64 <= ArchWidth arch) => LatencyMap arch exts
base64Latency = Map.fromList
  [ Pair Addw  (Flip $ litBV 1)
  , Pair Subw  (Flip $ litBV 1)
  , Pair Sllw  (Flip $ litBV 1)
  , Pair Srlw  (Flip $ litBV 1)
  , Pair Sraw  (Flip $ litBV 1)
  , Pair Lwu   (Flip $ litBV 1)
  , Pair Ld    (Flip $ litBV 1)
  , Pair Addiw (Flip $ litBV 1)
  , Pair Slliw (Flip $ litBV 1)
  , Pair Sriw  (Flip $ litBV 1)
  , Pair Sd    (Flip $ litBV 1)
  ]

mLatency :: (KnownArch arch) => LatencyMap arch exts
mLatency = Map.fromList
  [ Pair Mul    (Flip $ litBV 1)
  , Pair Mulh   (Flip $ litBV 1)
  , Pair Mulhsu (Flip $ litBV 1)
  , Pair Mulhu  (Flip $ litBV 1)
  , Pair Div    (Flip $ litBV 1)
  , Pair Divu   (Flip $ litBV 1)
  , Pair Rem    (Flip $ litBV 1)
  , Pair Remu   (Flip $ litBV 1)
  ]

-- m64Latency :: (KnownArch arch, 64 <= ArchWidth arch) => LatencyMap arch exts
-- m64Latency = Map.fromList
--   [ Pair Mulw  (Flip $ litBV 1)
--   , Pair Divw  (Flip $ litBV 1)
--   , Pair Divuw (Flip $ litBV 1)
--   , Pair Remw  (Flip $ litBV 1)
--   , Pair Remuw (Flip $ litBV 1)
--   ]

-- aLatency :: KnownArch arch => LatencyMap arch exts
-- aLatency = Map.fromList
--   [ Pair Lrw      (Flip $ litBV 1)
--   , Pair Scw      (Flip $ litBV 1)
--   , Pair Amoswapw (Flip $ litBV 1)
--   , Pair Amoaddw  (Flip $ litBV 1)
--   , Pair Amoxorw  (Flip $ litBV 1)
--   , Pair Amoandw  (Flip $ litBV 1)
--   , Pair Amoorw   (Flip $ litBV 1)
--   , Pair Amominw  (Flip $ litBV 1)
--   , Pair Amomaxw  (Flip $ litBV 1)
--   , Pair Amominuw (Flip $ litBV 1)
--   , Pair Amomaxuw (Flip $ litBV 1)
--   ]

-- a64Latency :: (KnownArch arch, 64 <= ArchWidth arch) => LatencyMap arch exts
-- a64Latency = Map.fromList
--   [ Pair Lrd      (Flip $ litBV 1)
--   , Pair Scd      (Flip $ litBV 1)
--   , Pair Amoswapd (Flip $ litBV 1)
--   , Pair Amoaddd  (Flip $ litBV 1)
--   , Pair Amoxord  (Flip $ litBV 1)
--   , Pair Amoandd  (Flip $ litBV 1)
--   , Pair Amoord   (Flip $ litBV 1)
--   , Pair Amomind  (Flip $ litBV 1)
--   , Pair Amomaxd  (Flip $ litBV 1)
--   , Pair Amominud (Flip $ litBV 1)
--   , Pair Amomaxud (Flip $ litBV 1)
--   ]
