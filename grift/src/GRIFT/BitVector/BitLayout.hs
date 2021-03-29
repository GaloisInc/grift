{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

-- |
-- Module      : Data.BitVector.Sized.BitLayout
-- Copyright   : (c) Galois Inc. 2018
-- License     : BSD-3
-- Maintainer  : benselfridge@galois.com
-- Stability   : experimental
-- Portability : portable
--
-- This module defines a 'BitLayout' datatype which encodes a chunk-to-chunk mapping (no
-- overlaps) from a smaller bit vector into a larger one. 'BitLayout's are especially
-- useful for defining the encoding and decoding of opcodes/operands in an instruction.
module GRIFT.BitVector.BitLayout
  ( -- * Chunk
    Chunk (..),
    chunk,

    -- * BitLayout
    BitLayout,
    empty,
    singleChunk,
    (<:),
    inject,
    extract,

    -- * Lenses
    layoutLens,
    layoutsLens,

    -- * Utilities
    bitLayoutAssignmentList,
  )
where

import Control.Lens (Lens', lens)
import qualified Data.BitVector.Sized as BV
import Data.Foldable (Foldable (toList))
import qualified Data.Functor.Product as P
import Data.Parameterized
  ( type (+),
    LeqProof (LeqProof),
    NatRepr (..),
    addNat,
    intValue,
    knownNat,
    leqTrans,
    withKnownNat,
  )
import Data.Parameterized.List (List, ifoldr, imap, izipWith)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import GHC.Natural (Natural)
import GHC.TypeLits (KnownNat, Nat, type (<=))
import Text.PrettyPrint.HughesPJClass (Pretty (..), text)

import GRIFT.BitVector.BVApp ( zextOrId, ashr' )
import GRIFT.Types ( SizedBV(SizedBV) )

-- | 'Chunk' type, parameterized by chunk width. The internal 'Int' is the
-- position of the least significant bit of the chunk, and the type-level nat 'w' is
-- the width of the chunk.
--
-- >>> chunk 2 :: Chunk 5
-- Chunk 5 2
--
-- Intuitively, the above chunk index captures the notion of /embedding/ a
-- 'BitVector' @5@ (bit vector of width 5) into a larger 'BitVector' at index 2,
-- preserving the order of the input bits. So an 5-bit input like @10011@ would map
-- to some larger bit vector containing the input starting at position 2, like
-- @000001001100@.
--
-- Multiple 'Chunk's comprise a 'BitLayout'; see below.
data Chunk (w :: Nat) (o :: Nat) :: * where
  Chunk ::
    NatRepr w -> -- width of range
    NatRepr o -> -- index of range start
    Chunk w o

data SomeChunk = forall w o. SomeChunk (Chunk w o)

-- | Construct a 'Chunk' in a context where the chunk width is known at compile
-- time.
chunk ::
  forall w o.
  KnownNat w =>
  KnownNat o =>
  Chunk w o
chunk = Chunk (knownNat @w) (knownNat @o)

deriving instance Show (Chunk w o)

-- instance ShowF Chunk where
--   showF = show

instance Pretty (Chunk w o) where
  pPrint (Chunk w s)
    | width > 0 =
      text $
        "[" ++ show (start + width - 1) ++ "..." ++ show start ++ "]"
    | otherwise = text $ "[" ++ show start ++ "]"
    where
      start = intValue s
      width = intValue w

instance Pretty (ValidChunkFor t s) where
  pPrint (ValidChunk chk) = pPrint chk

-- instance Pretty (Some Chunk) where
--   pPrint (Some chk) = pPrint chk

-- | BitLayout type, parameterized by target width and source width. @t@ is the
-- target width, @s@ is the source width. @s@ should always be less than or equal to
-- @t@.
--
-- To construct a 'BitLayout', use the 'empty' constructor and the '<:' operator,
-- like so:
--
-- >>> empty :: BitLayout 32 0
-- BitLayout 32 0 (fromList [])
-- >>> let layout = (chunk 25 :: Chunk 7) <: (chunk 7 :: Chunk 5) <: (empty :: BitLayout 32 0)
-- >>> layout
-- BitLayout 32 12 (fromList [Chunk 5 7,Chunk 7 25])
-- >>> :type it
-- it :: BitLayout 32 12
--
-- In the above example @bitLayout@ defines a chunk-by-chunk mapping from a bit
-- vector of width 12 to one of width 32. We imagine the input vector of width 12
-- listed like so:
--
-- @
-- 0bAXXXXXBCXXXD
--   |-----||---|
--      7     5
-- @
--
-- Here, bits @A@, @B@, @C@, and @D@ are just labeled as such to illustrate their
-- place after the mapping. The @BitLayout 32 12@ defined above as the @layout@
-- variable would map that 12-bit vector to the following 32-bit vector:
--
-- @
--      (Bit 25)          (Bit 5)
--         |                 |
--         |                 |
--         v                 v
-- 0bAXXXXXB0000000000000CXXXD0000000
--   |-----|             |---|
--      7                  5
-- @
--
-- To use a 'BitLayout' to achieve a bidirectional mapping like the one described
-- above, you can either use the 'Lens' interface or the functions 'inject' and
-- 'extract', which give an explicit setter and getter, respectively.
--
-- Example use of @inject@/@extract@:
--
-- >>> let bl = (chunk 25 :: Chunk 7) <: (chunk 7 :: Chunk 5) <: (empty :: BitLayout 32 0)
-- >>> let sVec = bitVector 0b111111100001 :: BitVector 12
-- >>> sVec
-- 0xfe1
-- >>> inject bl (bitVector 0) (bitVector 0b111111100001)
-- 0xfe000080
-- >>> extract bl $ inject bl (bitVector 0) (bitVector 0b111111100001)
-- 0xfe1
data BitLayout (t :: Nat) (s :: Nat) :: * where
  BitLayout ::
    -- Having the BitLayout record its proof that 's <= t' is useful in some key
    -- places, such as in the definition of 'extract', so as to not pollute all
    -- callers with 's <= t' constraints.
    s <= t =>
    NatRepr t -> NatRepr s -> Seq (ValidChunkFor t s) -> BitLayout t s

data ValidChunkFor (t :: Nat) (s :: Nat) =
  forall w o. (w <= s, w + o <= t) => ValidChunk (Chunk w o)

deriving instance Show (ValidChunkFor t s)

instance Pretty (BitLayout t s) where
  pPrint (BitLayout _ _ chks) = text $ show (pPrint <$> reverse $ toList chks)

deriving instance Show (BitLayout t s)

-- | Construct an empty 'BitLayout'.
empty :: KnownNat t => BitLayout t 0
empty = BitLayout knownNat knownNat S.empty

-- | Construct a 'BitLayout' with one chunk.
singleChunk ::
  forall o t s.
  KnownNat o =>
  KnownNat t =>
  KnownNat s =>
  s + o <= t =>
  BitLayout t s
singleChunk = chunk @s @o <: empty

-- TODO: Should this be in Maybe?

-- | Add a 'Chunk' to a 'BitLayout'. If the 'Chunk' does not fit, either because the
-- resulting 'BitLayout' would be too long or because it would overlap with a 'Chunk'
-- that is already in the 'BitLayout', we throw an error.
(<:) ::
  forall w o s t.
  w + o <= t =>
  w + s <= t =>
  -- | chunk to add
  Chunk w o ->
  -- | layout we are adding the chunk to
  BitLayout t s ->
  BitLayout t (w + s)
chk@(Chunk w _) <: bl@(BitLayout t s chunks) =
  if chk `chunkFits` bl
    then BitLayout t (w `addNat` s)
         ((validChunkForLargerLayout <$> chunks) S.|> ValidChunk chk)
    else
      error $
        "chunk " ++ show chk ++ " does not fit in layout of size "
          ++ show (natValue t)
          ++ ": "
          ++ show bl

-- TODO: check precedence (associativity is correct)
infixr 6 <:

chunkFits :: Chunk w o -> BitLayout t s -> Bool
chunkFits chk@(Chunk (natValue -> r) (natValue -> start))
          (BitLayout (natValue -> t) (natValue -> s) chunks) =
  (r + s <= t)
  && (start + r <= t) -- widths are ok
  && noOverlaps chk (toList chunks)

noOverlaps :: Chunk w o -> [ValidChunkFor t s] -> Bool
noOverlaps chk = all (chunksDontOverlap (SomeChunk chk))

chunksDontOverlap :: SomeChunk -> ValidChunkFor t s -> Bool
chunksDontOverlap (SomeChunk (Chunk chunkRepr1 start1)) (ValidChunk (Chunk chunkRepr2 start2)) =
  if natValue start1 <= natValue start2
    then natValue start1 + fromInteger (intValue chunkRepr1) <= natValue start2
    else natValue start2 + fromInteger (intValue chunkRepr2) <= natValue start1

-- | Insert (via "or") a smaller 'BitVector' @s@ within a larger 'BitVector' @t@
-- at a given position.
bvOrAt ::
  KnownNat s =>
  KnownNat t =>
  -- 1 <= t =>
  s <= t =>
  Natural -> BV.BV s -> BV.BV t -> BV.BV t
bvOrAt start s =
  BV.or (ashr' (zextOrId s) start)

-- | Given a list of 'Chunk's, inject each chunk from a source 'BitVector' @s@ into a
-- target 'BitVector' @t@.
bvOrAtAll ::
  forall s t.
  KnownNat s =>
  KnownNat t =>
  -- 1 <= t =>
  s <= t =>
  [SomeChunk] ->
  BV.BV s ->
  BV.BV t
bvOrAtAll [] _ = BV.mkBV knownNat 0
bvOrAtAll (SomeChunk (Chunk (natValue -> w) (natValue -> o)) : chunks) sVec =
  bvOrAt o (BV.truncBits w sVec)
    (bvOrAtAll chunks (BV.shl knownNat sVec (- w)))

-- | Use a 'BitLayout' to inject a smaller vector into a larger one.
inject ::
  -- 1 <= t =>
  -- | The layout
  BitLayout t s ->
  -- | The larger vector to inject into
  SizedBV t ->
  -- | The smaller vector to be injected
  SizedBV s ->
  SizedBV t
inject (BitLayout _ _ chunks) (SizedBV t tVec) (SizedBV s sVec) =
  SizedBV t (withKnownNat s (withKnownNat t (bvOrAtAll (toList (asSomeChunk <$> chunks)) sVec)) `BV.or` tVec)

asSomeChunk :: ValidChunkFor t s -> SomeChunk
asSomeChunk (ValidChunk c) = SomeChunk c

-- First, extract the appropriate bits as a BitVector t, where the relevant bits
-- start at the LSB of the vector (so, mask and shiftL). Then, truncate to a
-- BitVector s, and shift into the starting position.
extractChunk ::
  w <= s =>
  w + o <= t =>
  -- | width of output
  NatRepr s ->
  -- | where to place the chunk in the result
  Natural ->
  -- | location/width of chunk in the input
  Chunk w o ->
  -- | input vector
  BV.BV t ->
  BV.BV s
extractChunk sRepr sStart (Chunk chunkRepr chunkStart) tVec =
  BV.shl sRepr extractedChunk sStart
  where
    extractedChunk =
      withKnownNat chunkRepr
        (withKnownNat sRepr
          (zextOrId (BV.select chunkStart chunkRepr tVec)))

validChunkForLargerLayout :: forall t s l.
  s <= t =>
  s <= l =>
  ValidChunkFor t s -> ValidChunkFor t l
validChunkForLargerLayout (ValidChunk chk) = stillValid chk
  where
    stillValid :: forall w o. (w <= s, w + o <= t) => Chunk w o -> ValidChunkFor t l
    stillValid chk' =
      case leqTrans (LeqProof :: LeqProof w s) (LeqProof :: LeqProof s l) of
        LeqProof -> ValidChunk chk'

extractAll ::
  forall t s.
  s <= t =>
  -- | width of input vector
  NatRepr t ->
  -- | determines width of output vector
  NatRepr s ->
  -- | current position in output vector
  Natural ->
  -- | list of remaining chunks to place in output vector
  [ValidChunkFor t s] ->
  -- | input vector
  BV.BV t ->
  BV.BV s
extractAll _ s _ [] _ = BV.mkBV s 0
extractAll t s outStart (ValidChunk chk@(Chunk w _) : chunks) v =
  extractChunk s outStart chk v
  `BV.or` extractAll t s (outStart + natValue w) chunks v

-- | Use a 'BitLayout' to extract a smaller vector from a larger one.
extract ::
  -- | The layout
  BitLayout t s ->
  -- | The larger vector to extract from
  SizedBV t ->
  SizedBV s
extract (BitLayout t s chunks) (SizedBV _ bv) =
  SizedBV s (extractAll t s 0 (toList chunks) bv)

-- | Lens for a 'BitLayout'.
layoutLens ::
  KnownNat s =>
  KnownNat t =>
  -- s + 1 <= t =>
  BitLayout t s -> Lens' (SizedBV t) (SizedBV s)
layoutLens layout = lens (extract layout) (inject layout)

-- | Lens for a parameterized 'List' of 'BitLayout's.
layoutsLens :: forall ws. List (BitLayout 32) ws -> Lens' (SizedBV 32) (List SizedBV ws)
layoutsLens layouts =
  lens
    (\bv -> imap (const $ flip extract bv) layouts)
    ( \bv bvFlds ->
        ifoldr
          (\_ (P.Pair fld layout) bv' -> inject layout bv' fld)
          bv
          (izipWith (const P.Pair) bvFlds layouts)
    )

-- | From a `BitLayout`, get a list representing the position of each bit from the
-- source to the target. The list
--
-- @
-- [3,4,5,10,11,12,13]
-- @
--
-- means that bit 0 of the source is placed in bit 3 of the target, bit 1 of the
-- source is placed in bit 4 of the target, etc.
bitLayoutAssignmentList :: BitLayout t s -> [Integer]
bitLayoutAssignmentList (BitLayout _ _ chunks) =
  reverse (bitLayoutAssignmentList' (toList chunks))

bitLayoutAssignmentList' :: [ValidChunkFor t s] -> [Integer]
bitLayoutAssignmentList' [] = []
bitLayoutAssignmentList' (ValidChunk (Chunk wRepr start) : rst) =
  reverse [start' .. start' + w -1] ++ bitLayoutAssignmentList' rst
  where
    start' = intValue start
    w = fromIntegral (natValue wRepr)
