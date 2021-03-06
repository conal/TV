{-# LANGUAGE GADTs, KindSignatures, MultiParamTypeClasses #-}
----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.Output
-- Copyright   :  (c) Conal Elliott 2006-2013
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  GADTs
-- 
-- Outputs (interfaces) -- means of presenting values
----------------------------------------------------------------------

module Interface.TV.Output
  (
  -- * Output data type
    Output(..), output
  -- * Output functions
  -- ** Builders
  , oPrim, oLambda, oPair, oTitle
  -- ** Canonicalizers
  , asOLambda, asOPair
--   -- ** Type specialization
--   , Output', oPrim'
  ) where

import Control.Compose (ContraFunctor(..))
import Data.Pair (Pair(..))
import Data.Lambda (Lambda(..))
import Data.Title (Title_f(..))

import Interface.TV.Input

{----------------------------------------------------------
    Output data type
----------------------------------------------------------}

-- | An /Output/ describes a way to present a functional value, perhaps
-- interactively.  It is the user-interface half of a tangible value.
-- The constructors: primitive, function, pairing, and title.
data Output :: (* -> *) -> (* -> *) -> * -> * where
  -- -- | Output primitive
  OPrim :: snk a -> Output src snk a
  -- -- | Visualize a function.  Akin to /lambda/
  OLambda :: Input src  a -> Output src snk b -> Output src snk (a->b)
  -- -- | Visualize a pair
  OPair :: Output src snk a -> Output src snk b -> Output src snk (a,b)
  -- -- | Massage via an arrow value (like contraFmap)
  -- OCompose :: src (a -> b) -> Output src snk b -> Output src snk a
  -- -- | Title/label an output
  OTitle :: String -> Output src snk a -> Output src snk a


-- As of 2010-09-10, Haddock still doesn't support doc comments for GADT
-- constructors.  See <http://trac.haskell.org/haddock/ticket/43>.
-- When that bug gets fixed, fix the doc strings above.


instance Title_f (Output src snk) where title_f = OTitle

instance Show (Output src snk a) where
  -- show OEmpty          = "OEmpty"
  show (OPrim _)       = "(OPrim _)"
  show (OLambda i o)   = "(Lambda "++show i++" "++show o++")"
  show (OPair oa ob)   = "(OPair "++show oa++" "++show ob++")"
  -- show (OCompose _ b)  = "(ICompose _ "++show b++")"
  show (OTitle str o)  = "(OTitle "++show str++" "++show o++")"


output :: (Pair src, Pair snk, Lambda src snk, Title_f src, Title_f snk) =>
          Output src snk t -> snk t

output (OPrim snk)    = snk
output (OPair   a b)  = pair   (output a) (output b)
output (OLambda i o)  = lambda (input  i) (output o)
output (OTitle str t) = title_f str (output t)




{----------------------------------------------------------
    Canonicalizers
----------------------------------------------------------}

-- | Dissect a function-valued output into input & output.  Loses outer 'oTitle's.
-- Must be a (possibly titled) pair-style input.
asOLambda :: Output src snk (a->b) -> (Input src a, Output src snk b)
asOLambda (OLambda a b) = (a,b)
asOLambda (OTitle _ ab) = asOLambda ab
asOLambda o             = error ("asOLambda of non-OLambda "++show o)

--asOLambda _             = (iEmpty, oEmpty)


-- Alternatively, transform titles
-- asOLambda (OTitle  s ab) = ( ITitle ("input of " ++s) a
--                            , OTitle ("output of "++s) b )
--  where
--    (a,b) = asOLambda ab

asOPair :: Output src snk (a,b) -> (Output src snk a, Output src snk b)
asOPair (OPair  a b ) = (a,b)
asOPair (OTitle _ ab) = asOPair ab
asOPair o             = error ("asOPair of non-OPair "++show o)

-- asOPair _             = (oEmpty, oEmpty)

-- Alternatively:
-- asOPair (OTitle s ab) = ( OTitle ("first of " ++s) a
--                         , OTitle ("second of "++s) b )
--  where
--    (a,b) = asOPair ab



{----------------------------------------------------------
    Output functions
----------------------------------------------------------}

-- These functions just rename the constructors.  Maybe eliminate.
-- Keep for now, since Haddock can't digest the constructor declarations.

-- | Output primitive
oPrim :: snk a -> Output src snk a
oPrim = OPrim

-- | Visualize a function.  Akin to /lambda/
oLambda :: Input src  a -> Output src snk b -> Output src snk (a->b)
oLambda = OLambda

-- | Visualize a pair
oPair :: Output src snk a -> Output src snk b -> Output src snk (a,b)
oPair = OPair


instance Pair (Output src snk) where pair = oPair

instance Lambda (Input src) (Output src snk) where lambda = oLambda


-- | Title (label) an output
oTitle :: String -> Output src snk a -> Output src snk a
oTitle = OTitle


-- I specialized oCompose to contraFmapO.  I don't think there's enough
-- machinery to do oCompose.

contraFmapO :: ContraFunctor snk => (a -> b) -> Output src snk b -> Output src snk a
f `contraFmapO` OPrim ranb    = OPrim (contraFmap f ranb)
f `contraFmapO` OTitle str ab = OTitle str (f `contraFmapO` ab)
_ `contraFmapO` o             = error ("contraFmapO given non-OPrim: "++show o)

instance ContraFunctor snk => ContraFunctor (Output src snk) where
  contraFmap = contraFmapO

