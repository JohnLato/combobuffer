{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Template Haskell splices to create  constant-sized vectors and RingBuffer
-- instances for them
module Data.RingBuffer.TGen (
  mkVecFromTo
 ,mkVec
)

where

import Prelude hiding (length)
import Data.RingBuffer.Class

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Applicative
import Control.Monad

mkVecFromTo start stop elname binders prefix =
  concat <$> mapM (mkVec elname binders prefix) [start .. stop]

mkVec elname binders prefix sz = do
  let nm = mkName $ 'T':prefix ++ show sz
  let tname = case binders of
        []   -> ConT nm
        [PlainTV b1] -> AppT (ConT nm) (VarT b1)
        _    -> error "can't handle types with more than 1 type variable, or non-* kinded types"
  d1 <- decTN sz nm elname binders
  d2 <- mkElInst tname elname
  d3 <- mkInitInst sz nm (return tname)
  d4 <- mkRbInst sz nm (return tname)
  return $ concat [d1,d2,d3,d4]

decTN sz nm elname binders =
  let fields = replicate sz (IsStrict, elname)
  in return [DataD [] nm binders [NormalC nm fields] []]

#if MIN_VERSION_template_haskell(2,9,0)
mkElInst tname elname = return [TySynInstD ''El $ TySynEqn [tname] (elname) ]
#else
mkElInst tname elname = return [TySynInstD ''El [tname] (elname) ]
#endif

mkInitInst vsz nm tname = let nmStr = show nm in [d| instance Initializable $(tname) where {-# INLINE newInit #-}; newInit el sz | sz >= 0 && sz <= vsz = $(appsE $ conE nm:replicate vsz [| el |]) ; newInit el sz = error ("cannot initialize " ++ nmStr ++ " with size: " ++ show sz) |]

mkRbInst vsz nm tname = [d| instance RingBuffer $(tname) where {-# INLINE length #-}; length = const vsz; {-# INLINE (!) #-}; (!) = $(mkLookup vsz nm); {-# INLINE push #-}; push = $(mkPush vsz nm) |]

mkLookup vsz nm = do
  nms <- mapM (newName . ('v':) . show) [1 .. vsz]
  ixNm <- newName "ix"
  let lhs1 = conP nm (map varP nms)
      lhs2 = varP ixNm
      matches = map (\ix -> match (litP $ integerL (fromIntegral ix)) 
                                (normalB $ varE (nms !! ix) )
                                [] )
                    [0..vsz-1]
                ++ [match (varP (mkName "ix"))
                          (normalB [| error ("TGen: index out of bounds: " ++ show $(varE $ mkName "ix")) |])
                          [] ]
      rhs  = caseE (varE ixNm) matches
  lamE [lhs1,lhs2] rhs

mkPush vsz nm = do
  nms <- mapM (newName . ('v':) . show) [1 .. vsz]
  elNm <- newName "el"
  let lhs1 = conP nm (map varP nms)
      lhs2 = varP elNm
      rhs  = appsE $ conE nm : varE elNm : map varE (init nms)
  lamE [lhs1, lhs2] rhs
