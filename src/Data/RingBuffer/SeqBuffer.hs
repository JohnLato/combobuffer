{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}
-- | A 'RingBuffer' based on 'Data.Sequence.Seq'.  Asymptotic behavior is quite
-- good in all cases, but constant factors are very high.
module Data.RingBuffer.SeqBuffer (
  SeqBuffer
 ,RingBuffer (..)
 ,new
)

where

import           Prelude hiding (length, (!!))
import           Data.RingBuffer.Class

import qualified Data.Sequence as S
import           Data.Foldable

newtype SeqBuffer a = RB (S.Seq a) deriving (Eq, Ord, Show)

type instance El (SeqBuffer a) = a

instance Initializable (SeqBuffer a) where
  {-# INLINE newInit #-}
  newInit = newInit'

instance RingBuffer (SeqBuffer a) where
  {-# INLINE length #-}
  length  = length'
  {-# INLINE push #-}
  push    = push'
  {-# INLINE (!) #-}
  (!)     = (!!)
  {-# INLINE slice #-}
  slice (RB sq) start num = toList . S.take num $ S.drop start sq

-- | Create a new SeqBuffer, initialized to all 0's, of the given size
new :: (Num a) => Int -> SeqBuffer a
new = newInit' 0
{-# INLINE new #-}

-- | Create a new SeqBuffer from a given initial value
newInit' :: a -> Int -> SeqBuffer a
newInit' _ sz | sz <= 0 = error "can't make empty ringbuffer"
newInit' i sz           = RB (S.replicate sz i)
{-# INLINE newInit' #-}

-- | Get the total size of a SeqBuffer.
length' :: SeqBuffer a -> Int
length' (RB vec) = S.length vec
{-# INLINE length' #-}

-- | Look up a value in a SeqBuffer.
(!!) :: SeqBuffer a -> Int -> a
(!!) (RB vec) = S.index vec
{-# INLINE (!!) #-}

-- | Push a new value into a SeqBuffer.  The following will hold:
--     NewSeqBuffer ! 0 === added element
--     NewSeqBuffer ! 1 === OldSeqBuffer ! 0
push' :: SeqBuffer a -> a -> SeqBuffer a
push' (RB vec) el   = case S.viewr vec of
  v' S.:> _ -> RB $ el S.<| v'
  _         -> error "internal error"
{-# INLINE push' #-}
