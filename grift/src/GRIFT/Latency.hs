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

{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-|
Module      : GRIFT.Latency
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Experimental -- we are attaching cycle counts to each instruction, which will be
computed during simulation.
-}

module GRIFT.Latency where

import qualified Data.Parameterized.Map as Map
import Data.Parameterized

import GRIFT.Semantics
import GRIFT.Types

newtype Flip t a b c = Flip (t c a b)

type LatencyMap arch exts = Map.MapF (Opcode arch exts) (Flip InstExpr arch (ArchWidth arch))

knownLatencyMap :: forall arch exts
                   . (KnownArch arch, KnownExtensions exts)
                => LatencyMap arch exts
knownLatencyMap = base `Map.union` m `Map.union` a `Map.union` f
  where archRepr = knownRepr :: BaseArchRepr arch
        ecRepr = knownRepr :: ExtensionsRepr exts
        base = case archRepr of
          RV32Repr -> baseLatency
          RV64Repr -> baseLatency `Map.union` base64Latency
          RV128Repr -> error "RV128 not yet supported"
        m = case (archRepr, ecRepr) of
          (RV32Repr, ExtensionsRepr _ MYesRepr _ _) -> mLatency
          (RV64Repr, ExtensionsRepr _ MYesRepr _ _) -> mLatency `Map.union` m64Latency
          _ -> Map.empty
        a = case (archRepr, ecRepr) of
          (RV32Repr, ExtensionsRepr _ _ AYesRepr _) -> aLatency
          (RV64Repr, ExtensionsRepr _ _ AYesRepr _) -> aLatency `Map.union` a64Latency
          _ -> Map.empty
        f = case ecRepr of
          ExtensionsRepr _ _ _ FDNoRepr -> Map.empty
          _ -> error "Floating point not yet supported"

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
  , Pair Srli   (Flip $ litBV 1)
  , Pair Srai   (Flip $ litBV 1)
  , Pair Fence  (Flip $ litBV 1)
  , Pair FenceI (Flip $ litBV 1)
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

  -- P type
  , Pair Ecall  (Flip $ litBV 1)
  , Pair Ebreak (Flip $ litBV 1)

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
  , Pair Srliw (Flip $ litBV 1)
  , Pair Sraiw (Flip $ litBV 1)
  , Pair Sd    (Flip $ litBV 1)
  ]

mLatency :: (KnownArch arch, KnownExtensions exts, MExt << exts) => LatencyMap arch exts
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

m64Latency :: (KnownArch arch, 64 <= ArchWidth arch, MExt << exts) => LatencyMap arch exts
m64Latency = Map.fromList
  [ Pair Mulw  (Flip $ litBV 1)
  , Pair Divw  (Flip $ litBV 1)
  , Pair Divuw (Flip $ litBV 1)
  , Pair Remw  (Flip $ litBV 1)
  , Pair Remuw (Flip $ litBV 1)
  ]

aLatency :: (KnownArch arch, AExt << exts) => LatencyMap arch exts
aLatency = Map.fromList
  [ Pair Lrw      (Flip $ litBV 1)
  , Pair Scw      (Flip $ litBV 1)
  , Pair Amoswapw (Flip $ litBV 1)
  , Pair Amoaddw  (Flip $ litBV 1)
  , Pair Amoxorw  (Flip $ litBV 1)
  , Pair Amoandw  (Flip $ litBV 1)
  , Pair Amoorw   (Flip $ litBV 1)
  , Pair Amominw  (Flip $ litBV 1)
  , Pair Amomaxw  (Flip $ litBV 1)
  , Pair Amominuw (Flip $ litBV 1)
  , Pair Amomaxuw (Flip $ litBV 1)
  ]

a64Latency :: (KnownArch arch, 64 <= ArchWidth arch, AExt << exts) => LatencyMap arch exts
a64Latency = Map.fromList
  [ Pair Lrd      (Flip $ litBV 1)
  , Pair Scd      (Flip $ litBV 1)
  , Pair Amoswapd (Flip $ litBV 1)
  , Pair Amoaddd  (Flip $ litBV 1)
  , Pair Amoxord  (Flip $ litBV 1)
  , Pair Amoandd  (Flip $ litBV 1)
  , Pair Amoord   (Flip $ litBV 1)
  , Pair Amomind  (Flip $ litBV 1)
  , Pair Amomaxd  (Flip $ litBV 1)
  , Pair Amominud (Flip $ litBV 1)
  , Pair Amomaxud (Flip $ litBV 1)
  ]
