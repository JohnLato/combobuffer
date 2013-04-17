{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wall #-}
-- | A 'RingBuffer' implementation based on IntMaps.  Operations have the same
-- complexity as the underlying IntMap
module Data.RingBuffer.MapBuffer (
  Initializable (..)
 ,RingBuffer (..)
 ,MapBuffer (..)
)

where

import           Prelude hiding (length)

import           Data.RingBuffer.Class
import           Data.IntMap (IntMap)
import qualified Data.IntMap as M

data MapBuffer a = MB {-# UNPACK #-} !Int !(IntMap a) 
  deriving (Eq, Show, Ord)

type instance El (MapBuffer el) = el

instance Initializable (MapBuffer el) where
  {-# INLINE newInit #-}
  newInit el sz = MB 0 $ M.fromDistinctAscList $ map (,el) [0..sz-1]

instance RingBuffer (MapBuffer el) where
  {-# INLINE length #-}
  length (MB _ vec) = M.size vec
  {-# INLINE (!) #-}
  (MB pos vec) ! ix = vec M.! ((pos-ix-1) `mod` M.size vec)
  {-# INLINE push #-}
  push = pushE
  {-# INLINE slice #-}
  slice = sliceB

pushE :: MapBuffer a -> a -> MapBuffer a
pushE (MB pos vec) el = 
  let newPos = if pos == M.size vec - 1 then 0 else pos + 1
      vec' = M.insert pos el vec
  in  MB newPos vec'
{-# INLINE pushE #-}

sliceB :: MapBuffer a -> Int -> Int -> [a]
sliceB (MB pos vec) start num = -- V.toList $ V.slice (pos+start) num vec
  let ix1 = pos+start
      top = snd $ M.split (ix1-1) vec
  in  map snd . M.toAscList . fst $ M.split (ix1+num) top
{-# INLINE sliceB #-}
