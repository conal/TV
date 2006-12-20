{-# OPTIONS -fglasgow-exts #-}

{- Module      :  Graphics.UI.TV.Output
   Copyright   :  (c) Conal Elliott 2006
   License     :  LGPL

   Maintainer  :  conal@conal.net
   Stability   :  provisional
   Portability :  portable
-}

-- | This module defines an 'Output' type constructor, for presenting
-- typed values to a user.
module Graphics.UI.TV.Output
  (
  -- * Output data type
    Output(..)
  -- * Output functions
  -- ** General
  , oPrim, oLambda, oPair
  ) where

import Control.Arrow

import Graphics.UI.TV.Input


{----------------------------------------------------------
    Output data type
----------------------------------------------------------}

-- | An /Output/ describes a way to present a functional value, perhaps
-- interactively.  It is the user-interface half of a tangible value.
data Output arr :: * -> * where
  -- | Output primitive
  OPrim   :: arr a () -> Output arr a
  -- | Visualize a function.  Akin to /lambda/
  OLambda :: Input arr  a -> Output arr b -> Output arr (a->b)
  -- | Visualize a pair
  OPair   :: Output arr a -> Output arr b -> Output arr (a,b)

instance Show (Output arr a) where
  show (OPrim p)     = "(OPrim _)"
  show (OLambda i o) = "(Lambda "++show i++" "++show o++")"
  show (OPair oa ob) = "(OPair "++show oa++" "++show ob++")"


{----------------------------------------------------------
    Output functions
----------------------------------------------------------}

-- | Output primitive
oPrim :: arr a () -> Output arr a
oPrim = OPrim

-- | Visualize a function.  Akin to /lambda/
oLambda :: Input arr  a -> Output arr b -> Output arr (a->b)
oLambda = OLambda

-- | Visualize a pair
oPair :: Output arr a -> Output arr b -> Output arr (a,b)
oPair = OPair


---- Canonicalizers (currently unused)

-- asOLambda :: Output arr (a->b) -> Maybe (Input arr a, Output arr b)
-- asOLambda (OLambda a b) = Just (a,b)
-- asOLambda f             = Nothing

-- asOPair :: Output arr (a,b) -> Maybe (Output arr a, Output arr b)
-- asOPair (OPair a b) = Just (a,b)
-- asOPair p           = Nothing

