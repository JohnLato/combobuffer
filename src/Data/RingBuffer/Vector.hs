{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -funbox-strict-fields #-}

-- | A vector-based 'RingBuffer'implementation
module Data.RingBuffer.Vector (
  VBuffer
)

where

import Prelude hiding (length)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Exception
import Control.Monad
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Data.RingBuffer.Class

data VBuffer el = VBuffer
    { size       :: !Int
    , offset     :: !Int
    , fullBuffer :: !(V.Vector el)
    , partial    :: !(V.Vector el)
    , stale      :: IORef Bool
    } deriving (Eq)

{-
class RingBuffer c where
  length    :: c -> Int
  push      :: c -> El c -> c
  (!)       :: c -> Int  -> El c
  slice     :: c -> Int  -> Int -> [El c]
  {-# INLINE slice #-}
  slice c start num = [ c ! ix | ix <- [start .. start+num]]
-}

type instance El (VBuffer el) = el

instance V.Unbox el => Initializable (VBuffer el) where
  {-# INLINE newInit #-}
  newInit val size = unsafePerformIO $ do
      -- we need to make sure that the two vectors aren't shared, and CSE may
      -- common them up if we just do (V.replicate size val)
      fullBuffer <- V.unsafeFreeze =<< VM.replicate size val
      partial <- V.unsafeFreeze =<< VM.replicate size val
      stale <- newIORef False
      let offset = 0
      return $ VBuffer { size, offset, fullBuffer, partial, stale }

instance V.Unbox el => RingBuffer (VBuffer el) where
  {-# INLINE length #-}
  length      = size
  {-# INLINE (!) #-}
  (!)         = index
  {-# INLINE push #-}
  push = pushBuf
  -- {-# INLINE slice #-}
  -- slice vec start num = V.toList $ V.slice start num vec

index :: (V.Unbox el) => VBuffer el -> Int -> el
index VBuffer{..} ix =
    if ix < offset
        then partial `V.unsafeIndex` ((offset-ix)-1)
        else fullBuffer `V.unsafeIndex` (size + offset - ix - 1)

pushBuf :: (V.Unbox el) => VBuffer el -> el -> VBuffer el
pushBuf VBuffer{..} el = unsafePerformIO $ do
    isStale <- atomicModifyIORef stale (True,)
    when isStale (throwIO $ ErrorCall "VBuffer: attempt to push to stale buffer")
    if offset < size
        then do
            v <- V.unsafeThaw partial
            VM.unsafeWrite v offset el
            newPartial <- V.unsafeFreeze v
            stale' <- newIORef False
            return $ VBuffer { size, offset=offset+1, fullBuffer
                             , partial=newPartial
                             , stale=stale' }
        else do
            stale' <- newIORef False
            return $ VBuffer { size, offset=1
                             , fullBuffer=partial
                             , partial = V.replicate size el
                             , stale = stale'}
