{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : RISCV.Extensions
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
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
import RISCV.Extensions.M
import RISCV.Types

-- | Infer the current instruction set from a context in which the 'BaseArch' and
-- 'Extensions' are known.
knownISet :: forall arch exts
             .  (KnownArch arch, KnownExtensions exts)
          => InstructionSet arch exts
knownISet = baseset <> mset <> fset
  where  archRepr = knownRepr :: BaseArchRepr arch
         ecRepr = knownRepr :: ExtensionsRepr exts
         baseset = case archRepr of
           RV32IRepr -> base32
           RV32ERepr -> base32
           RV64IRepr -> base64
           RV128IRepr -> error "RV128I not yet supported"
         mset = case (archRepr, ecRepr) of
           (RV32IRepr, ExtensionsRepr MYesRepr _) -> m32
           (RV32ERepr, ExtensionsRepr MYesRepr _) -> m32
           (RV64IRepr, ExtensionsRepr MYesRepr _) -> m64
           _ -> mempty
         fset = case ecRepr of
           ExtensionsRepr _ FDNoRepr -> mempty
           _ -> error "Floating point not yet supported"
