{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFoldable #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Strict vectors of doubles to length == 30
module Data.RingBuffer.SVec

where

import Prelude hiding (length)
import Data.RingBuffer.Class
import Data.RingBuffer.TGen
import Language.Haskell.TH (Type(..),mkName, TyVarBndr(..))

import Data.Foldable

$(mkVecFromTo 1 30 (ConT ''Double) [] "")

-- | Generate a 32-element polymorphic vector, maybe I should just use
-- Data.Vector?
$(mkVecFromTo 32 32 (VarT (mkName "a")) [PlainTV (mkName "a")] "Vec")

deriving instance Show a => Show (TVec32 a)
deriving instance Eq a => Eq (TVec32 a)
deriving instance Ord a => Ord (TVec32 a)
deriving instance Foldable (TVec32)
