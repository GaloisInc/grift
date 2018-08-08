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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : RISCV.Extensions
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : AGPLv3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

Extensions for RISC-V.
-}

module RISCV.Extensions
  ( -- * RISC-V Base ISA and extensions
    knownISet
  ) where

import Data.Monoid
import Data.Parameterized

import RISCV.InstructionSet
import RISCV.Extensions.Base
import RISCV.Extensions.A
import RISCV.Extensions.M
import RISCV.Extensions.Priv
import RISCV.Types

-- | Infer the current instruction set from a context in which the 'BaseArch' and
-- 'Extensions' are known.
knownISet :: forall rv . KnownRV rv => InstructionSet rv
knownISet = case knownRepr :: RVRepr rv of
  RVRepr archRepr ecRepr ->
    let baseset = case archRepr of
          RV32Repr -> base32
          RV64Repr -> base64
          RV128Repr -> error "RV128 not yet supported"
        privset = privm -- TODO
        mset = case (archRepr, ecRepr) of
          (RV32Repr, ExtensionsRepr _ MYesRepr _ _) -> m32
          (RV64Repr, ExtensionsRepr _ MYesRepr _ _) -> m64
          _ -> mempty
        aset = case (archRepr, ecRepr) of
          (RV32Repr, ExtensionsRepr _ _ AYesRepr _) -> a32
          (RV64Repr, ExtensionsRepr _ _ AYesRepr _) -> a64
          _ -> mempty
        fset = case ecRepr of
          ExtensionsRepr _ _ _ FDNoRepr -> mempty
          _ -> error "Floating point not yet supported"
    in baseset <> privset <> mset <> aset <> fset
