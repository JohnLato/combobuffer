{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}
module Data.RingBuffer.Chord
(
  Chord
, emptyChord
, cToVec
)

where

import Prelude hiding (length)
import Data.RingBuffer.Class
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector, Unbox)

-- A structure for pushing elements
-- params:
--   partial chunk fill
--   partial chunk
--   full chunk quantity
--   full chunks
data Chord a = Chord !Int [a] !Int [Vector a]
  deriving (Show, Eq, Ord)

-- | an empty chord.
emptyChord :: a -> Chord a
emptyChord _ = Chord 0 [] 0 []

cToVec :: Unbox a => Chord a -> Vector a
cToVec (Chord pf pc _ cs) = V.concat (V.fromListN pf pc : cs)
{-# INLINE cToVec #-}

type instance (El (Chord a)) = a

instance V.Unbox a => RingBuffer (Chord a) where
  {-# INLINE length #-}
  length (Chord pfill _ c _) = 32*c + pfill
  {-# INLINE push #-}
  push   (Chord 31 pc n cs) el =
     let new = V.fromListN 32 (el:pc)
     in  new `seq` Chord 0 [] (n+1) (new:cs)
  push   (Chord pf pc n cs) el =
     Chord (pf+1) (el : pc) n cs
  {-# INLINE (!) #-}
  (Chord pf pc _ cs) ! ix
     | ix <= pf  = pc !! ix
     | otherwise = let (major,minor) = (ix - pf) `divMod` 32
                   in (cs !! major) `V.unsafeIndex` minor
