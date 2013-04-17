{-# LANGUAGE TypeFamilies, GADTs #-}

{-# OPTIONS_GHC -Wall #-}
-- | The RingBuffer interface.
--
-- A RingBuffer is a fixed-length buffer that supports lookups anywhere in
-- the structure and pushing new elements onto the front.  When a new value is
-- pushed, the oldest value will be dropped.
--
-- This module provides an implementation based on
-- 'Data.Vector.Unboxed.Vector', with O(1) lookups and O(n) pushes.  Other
-- modules provide implementations with different performance profiles.
module Data.RingBuffer.Class (
  El
 ,RingBuffer (..)
 ,Initializable (..)
)

where

import Prelude hiding (length)
import qualified Data.Vector.Unboxed as V

type family El c :: *

-- | Create a new 'Initializable' with the given value at every position.
-- Essentially a generalized 'Prelude.replicate'
class Initializable c where
  newInit :: El c -> Int -> c

-- | The RingBuffer interface.
--
-- Instances must define 'length', 'push', and '(!)'.  Instances may define
-- 'slice' for better performance.
class RingBuffer c where
  length    :: c -> Int
  push      :: c -> El c -> c
  (!)       :: c -> Int  -> El c
  slice     :: c -> Int  -> Int -> [El c]
  {-# INLINE slice #-}
  slice c start num = [ c ! ix | ix <- [start .. start+num]]


type instance El (V.Vector el) = el

instance V.Unbox el => Initializable (V.Vector el) where
  {-# INLINE newInit #-}
  newInit = flip V.replicate

instance V.Unbox el => RingBuffer (V.Vector el) where
  {-# INLINE length #-}
  length      = V.length
  {-# INLINE (!) #-}
  (!)         = (V.!)
  {-# INLINE push #-}
  push vec el = V.cons el $ V.unsafeInit vec
  {-# INLINE slice #-}
  slice vec start num = V.toList $ V.slice start num vec
