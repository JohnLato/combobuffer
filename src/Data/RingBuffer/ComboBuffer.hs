{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}
-- | Generally the best-performing implementation in this package.
--
-- 'ComboBuffer' supports amortized O(1) push and O(1) length.  lookups at 'ix
-- >= n/2' and 'ix=0' are O(1), while lookups at '0 <= ix < n/2' are no more
-- than O(n).  If you need to do many lookups in the first half, and
-- particularly around n/4, another structure may be a better choice.
module Data.RingBuffer.ComboBuffer (
  ComboBuffer (..)
)

where

import           Prelude hiding (length)

import           Data.RingBuffer.Class
import           Data.RingBuffer.Chord
import qualified Data.Vector.Unboxed as V

data ComboBuffer a =
   CB {-# UNPACK #-} !Int          -- Half Size
      {-# UNPACK #-} !Int          -- buffer position
                     !(V.Vector a) -- newer vector
                     !(V.Vector a) -- older vector
      (Chord a)                    -- updates
 | CBOdd {-# UNPACK #-} !Int          -- Half (Size + 1)
         {-# UNPACK #-} !Int          -- buffer position
                        !(V.Vector a) -- newer vector
                        !(V.Vector a) -- older vector
         (Chord a)                    -- updates
 deriving (Eq, Ord, Show)

type instance El (ComboBuffer a) = a

instance V.Unbox a => Initializable (ComboBuffer a) where
  {-# INLINE newInit #-}
  newInit = newInit'

newInit' :: V.Unbox a => a -> Int -> ComboBuffer a
newInit' el sz
  | sz <= 0 = error "Can't initialize ComboBuffer with size <= 0"
  | sz `rem` 2 == 0 = let half = sz `div` 2
                          iv   = V.replicate half el
                      in  CB half 0 iv iv (emptyChord el)
  | otherwise       = let half = (1+sz) `div` 2
                          iv   = V.replicate half el
                      in  CBOdd half 0 iv iv (emptyChord el)
{-# INLINE newInit' #-}

instance V.Unbox a => RingBuffer (ComboBuffer a) where
  {-# INLINE length #-}
  length (CB sz _ _ _ _)    = 2*sz
  length (CBOdd sz _ _ _ _) = 2*sz-1
  {-# INLINE (!) #-}
  (!) = at
  {-# INLINE push #-}
  push = pushB
  {-# INLINE slice #-}
  slice = sliceB

at :: V.Unbox a => ComboBuffer a -> Int -> a
at (CB sz pos v1 v2 upds) ix
  | ix < 0    = error $ "ComboBuffer: index out of range: " ++ show ix
  | ix < pos  = upds ! ix
  | ix < 2*sz = let ix' = ix - pos in if ix' < sz
                  then v1 `V.unsafeIndex` ix'
                  else v2 `V.unsafeIndex` (ix' - sz)
  | otherwise = error $ "ComboBuffer: index out of range: " ++ show (ix,2*sz)
at (CBOdd sz pos v1 v2 upds) ix
  | ix < 0      = error $ "ComboBuffer: index out of range: " ++ show ix
  | ix < pos    = upds ! ix
  | ix < 2*sz-1 = let ix' = ix - pos in if ix' < sz
                    then v1 `V.unsafeIndex` ix'
                    else v2 `V.unsafeIndex` (ix' - sz)
  | otherwise = error $ "ComboBuffer: index out of range: " ++ show (ix,2*sz-1)
{-# INLINE at #-}

sliceB :: V.Unbox a => ComboBuffer a -> Int -> Int -> [a]
sliceB (CB sz pos v1 v2 upds) start num
  = let ix' = start - pos
        fstTake = sz - (ix'+num)
    in if ix' < sz
         then slice upds start num
              ++ V.toList (V.slice ix' (min 0 fstTake) v1)
              ++ V.toList (V.slice 0 (min 0 (negate fstTake)) v2)
         else V.toList (V.slice (ix'-sz) num v2)
sliceB (CBOdd sz pos v1 v2 upds) start num
  = let ix' = start - pos
        fstTake = sz - (ix'+num)
    in if ix' < sz
         then slice upds start num
              ++ V.toList (V.slice ix' (min 0 fstTake) v1)
              ++ V.toList (V.slice 0 (min 0 (negate fstTake)) v2)
         else V.toList (V.slice (ix'-sz) num v2)
{-# INLINE sliceB #-}

pushB :: V.Unbox a => ComboBuffer a -> a -> ComboBuffer a
pushB (CB sz pos v1 v2 upds) el
  | pos == sz-1 = CB sz 0 (cToVec $ push upds el) v1
                          (emptyChord el)
  | otherwise   = CB sz (pos+1) v1 v2 (push upds el)
pushB (CBOdd sz pos v1 v2 upds) el
  | pos == sz-1 = CBOdd sz 0 (cToVec $ push upds el) v1
                           (emptyChord el)
  | otherwise   = CBOdd sz (pos+1) v1 v2 (push upds el)
{-# INLINE pushB #-}
