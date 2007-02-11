{-# OPTIONS -fglasgow-exts -cpp #-}

----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.Output
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module defines an 'Output' type constructor, for presenting
-- typed values to a user.
----------------------------------------------------------------------

module Interface.TV.Output
  (
  -- * Output data type
    Output(..)
  -- * Output functions
  -- ** General
  {-, oEmpty-}, oPrim, oLambda, oPair, oCompose, oTitle
  -- ** Canonicalizers
  , asOLambda, asOPair
  ) where

import Control.Arrow

import Interface.TV.Input
import Interface.TV.Misc (Cofunctor(..))


{----------------------------------------------------------
    Output data type
----------------------------------------------------------}

-- | An /Output/ describes a way to present a functional value, perhaps
-- interactively.  It is the user-interface half of a tangible value.
#ifdef __HADDOCK__
data Output (~>) a
#else
data Output (~>) :: * -> * where
  -- | When we don't know what output to use.  I might remove this constructor.
  -- OEmpty :: Output (~>) a
  -- | Output primitive
  OPrim :: a ~> () -> Output (~>) a
  -- | Visualize a function.  Akin to /lambda/
  OLambda :: Input (~>)  a -> Output (~>) b -> Output (~>) (a->b)
  -- | Visualize a pair
  OPair :: Output (~>) a -> Output (~>) b -> Output (~>) (a,b)
  -- | Massage via an arrow value (like cofmap)
  -- OCompose :: a ~> b -> Output (~>) b -> Output (~>) a
  -- | Title/label an output
  OTitle :: String -> Output (~>) a -> Output (~>) a
#endif

-- See 'OEmpty' for note about eliminating OEmpty.

instance Show (Output (~>) a) where
  -- show OEmpty          = "OEmpty"
  show (OPrim _)       = "(OPrim _)"
  show (OLambda i o)   = "(Lambda "++show i++" "++show o++")"
  show (OPair oa ob)   = "(OPair "++show oa++" "++show ob++")"
  -- show (OCompose _ b)  = "(ICompose _ "++show b++")"
  show (OTitle str o)  = "(OTitle "++show str++" "++show o++")"


{----------------------------------------------------------
    Canonicalizers
----------------------------------------------------------}

-- | Dissect a pair-valued input into two inputs.  Loses outer 'oTitle's.
-- Yields empty inputs when not a (possibly titled) pair-style input.
asOLambda :: Output (~>) (a->b) -> (Input (~>) a, Output (~>) b)
asOLambda (OLambda a b) = (a,b)
asOLambda (OTitle _ ab) = asOLambda ab
asOLambda o             = error ("asOLambda of non-OLambda "++show o)

--asOLambda _             = (iEmpty, oEmpty)


-- Alternatively, transform titles
-- asOLambda (OTitle  s ab) = ( ITitle ("input of " ++s) a
--                            , OTitle ("output of "++s) b )
--  where
--    (a,b) = asOLambda ab

asOPair :: Output (~>) (a,b) -> (Output (~>) a, Output (~>) b)
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

{-
-- | An empty (invisible) output for when we don't know what else to do.
oEmpty :: Output (~>) a
oEmpty = OEmpty
-}

-- Alternatively, eliminate OEmpty and define

-- oEmpty = oPrim (arr $ const ())


-- | Output primitive
oPrim :: a ~> () -> Output (~>) a
oPrim = OPrim

-- | Visualize a function.  Akin to /lambda/
oLambda :: Input (~>)  a -> Output (~>) b -> Output (~>) (a->b)
oLambda = OLambda

-- | Visualize a pair
oPair :: Output (~>) a -> Output (~>) b -> Output (~>) (a,b)
oPair = OPair

-- | Massage via an arrow value (like cofmap)
oCompose :: Arrow (~>) => a ~> b -> Output (~>) b -> Output (~>) a
arr `oCompose` OPrim put = OPrim (arr >>> put)
_   `oCompose` o         = error ("oCompose given non-OPrim: "++show o)

-- oCompose = OCompose

-- | Title (label) an output
oTitle :: String -> Output (~>) a -> Output (~>) a
oTitle = OTitle

-- | Handy specialization of 'oCompose'
instance Arrow (~>) => Cofunctor (Output (~>)) where
  cofmap f input = pure f `oCompose` input
