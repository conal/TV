{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.OFun
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- 'Output' transformations, as a "deep arrow".
----------------------------------------------------------------------

module Interface.TV.OFun (OX,OFun,wrapO,wrapAO) where

import Control.Arrow

import Data.FunArr
import Control.Arrow.DeepArrow

import Interface.TV.Output
import Interface.TV.Input

type OX (~>) a b = Output (~>) a -> Output (~>) b

newtype OFun (~>) a b = OFun (OX (~>) a b)

-- TODO: consider generalizing from "->" in IFun?

instance Arrow (OFun (~>)) where
  -- (a->b) -> OFun (~>) a b
  arr = error "Interface.TV.OFun: no 'arr' method"
        -- We could use the following definition instead.
        -- const $ OFun (const OEmpty)

  -- OFun (~>) a b -> OFun (~>) b c -> OFun (~>) a c
  OFun f >>> OFun g = OFun (f >>> g)

  -- OFun (~>) a a' -> OFun (~>) (a,b) (a',b)
  first (OFun f) = OFun (firstO f)

  -- OFun (~>) b b' -> OFun (~>) (a,b) (a,b')
  second (OFun f) = OFun (secondO f)
  -- Or, as recommended in DeepArrow.
  -- second f = swapA >>> first f >>> swapA
  -- As recommended
  f &&& g = dupA >>> f *** g

instance DeepArrow (OFun (~>)) where
  idA      = OFun id
  dupA     = postFun "dup" dupO
  fstA     = postFun "first half" fstO
  sndA     = postFun "second half" sndO
  funF     = postFun "funF" funFO
  funS     = postFun "funS" funSO
  funR     = postFun "funR" funRO
  swapA    = postFun "swapped" swapO
  curryA   = postFun "curried" curryO
  uncurryA = postFun "uncurried" uncurryO

  result   (OFun ox) = OFun (resultO   ox)
  -- argument (OFun ox) = OFun (argumentO ox)

instance FunArr (OFun (~>)) (Output (~>)) where
  toArr ofun = OFun (applyO ofun)
  OFun ox $$ o = ox o

-- Alter any outer titles before transforming
retitle :: (String -> String) -> OX (~>) a b -> OX (~>) a b
retitle re ofun (OTitle str o) = OTitle (re str) (retitle re ofun o)
retitle _  ofun o = ofun o

-- Alter by appending a comment
posttitle :: String -> OX (~>) a b -> OX (~>) a b
posttitle post = retitle (++ (" -- " ++ post))

-- Convenient wrappers
-- reFun :: (String->String) -> OX (~>) a b -> OFun (~>) a b
-- reFun re = OFun . retitle re

postFun :: String -> OX (~>) a b -> OFun (~>) a b
postFun post = OFun . posttitle post

-- Why don't we do postFun for 'result'?  Because we get funny titles when
-- composing (like "reverse -- result" for reverse composed with itself).
-- Maybe more experience will tell that automatic retitling is a bad idea
-- in general.  Wait & see.

---- Output transformers

resultO :: OX (~>) b b' -> OX (~>) (a->b) (a->b')
resultO ox ab = OLambda a (ox b)
  where (a,b) = asOLambda ab

-- argumentO :: OX (~>) a' a -> OX (~>) (a->b) (a'->b)
-- argumentO ox ab = 

applyO :: Output (~>) (a->b) -> OX (~>) a b
applyO o = const b where (_,b) = asOLambda o

firstO :: OX (~>) a c -> OX (~>) (a,b) (c,b)
firstO f ab = OPair (f a) b where (a,b) = asOPair ab

secondO :: OX (~>) b c -> OX (~>) (a,b) (a,c)
secondO f ab = OPair a (f b) where (a,b) = asOPair ab

dupO :: OX (~>) a (a,a)
dupO a = OPair a a

fstO :: OX (~>) (a,b) a
fstO ab = a where (a,_) = asOPair ab

sndO :: OX (~>) (a,b) b
sndO ab = b where (_,b) = asOPair ab

funFO :: OX (~>) (c->a,b) (c->(a,b))
funFO gb = OLambda c (OPair a b)
  where
    (g,b) = asOPair gb
    (c,a) = asOLambda g

-- Try this style
-- funFO' :: OX (~>) (c->a,b) (c->(a,b))
-- funFO' gb = OLambda c (OPair a b)
--   where
--     ((c,a),b) = first asOLambda (asOPair gb)

funSO :: OX (~>) (a,c->b) (c->(a,b))
funSO ag = OLambda c (OPair a b)
  where
    (a,g) = asOPair ag
    (c,b) = asOLambda g

funRO :: OX (~>) (a->b->c) (b->a->c)
funRO abc = OLambda b (OLambda a c) 
 where
   (a,bc) = asOLambda abc
   (b,c)  = asOLambda bc

swapO :: OX (~>) (a,b) (b,a)
swapO ab = OPair b a where (a,b) = asOPair ab

curryO :: OX (~>) ((a,b)->c) (a->b->c)
curryO o = OLambda a (OLambda b c)
  where (ab,c) = asOLambda o
        (a ,b) = asIPair ab

uncurryO :: OX (~>) (a->b->c) ((a,b)->c)
uncurryO o = OLambda (IPair a b) c
  where (a,bc) = asOLambda o
        (b, c) = asOLambda bc

-- For now leave lAssocA and rAssocA as default.


---- Other functions

-- | Like @wrapF@, but for outputs and reversed orientation.
-- Specialization of 'wrapAO'.
wrapO :: Arrow (~>) => (b'->b) -> (a->a') -> OX (~>) (a->b) (a'->b')
wrapO g f = wrapAO (pure g) (pure f)

-- | Generalizes 'wrapO' for arbitrary arrows.
wrapAO :: Arrow (~>) =>
          (b' ~> b) -> (a ~> a') -> OX (~>) (a->b) (a'->b')
wrapAO outer inner ab = OLambda (ia `iCompose` inner) (outer `oCompose` ob)
 where
   (ia,ob) = asOLambda ab


-- Then we get an elegant definition of @interactRSOut@:

-- interactRSOut dflt = wrapO show (readD dflt) interactLine


