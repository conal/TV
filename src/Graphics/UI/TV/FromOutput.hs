{-# OPTIONS -fglasgow-exts #-}

{- Module      :  Graphics.UI.TV.FromOutput
   Copyright   :  (c) Conal Elliott 2006
   License     :  LGPL

   Maintainer  :  conal@conal.net
   Stability   :  provisional
   Portability :  portable
-}

-- | Convert 'Input's and 'Output's to arrow values
module Graphics.UI.TV.FromOutput
  (
   fromOutput, LayoutArr(..), ToIO(..)
  ) where

import Control.Arrow

import Graphics.UI.Phooey (UI)
import Graphics.UI.TV.Input
import Graphics.UI.TV.Output

-- Hmm.  Too bad we need this class.  Come up with a better name than "layout"
class Arrow arr => LayoutArr arr where
  layoutPair, layoutLambda :: arr a b -> arr a b

-- Find a better place for this one
class ToIO arr where
  toIO :: arr () () -> IO ()

-- | Convert an 'Input' into a 'UI'
fromInput :: LayoutArr arr => Input arr a -> arr () a

fromInput (IPrim p) = p

fromInput (IPair ia ib) = layoutPair (fromInput ia &&& fromInput ib)

{- Or, using arrow notation (not supported by Haddock).

fromInput (IPair ia ib) = layoutPair $
                          proc () -> do
                            a <- ua -< ()
                            b <- ub -< ()
                            returnA -< (a,b)
  where ua = fromInput ia
        ub = fromInput ib

-}

-- | Convert an 'Output' into a 'UI'
fromOutput :: LayoutArr arr => Output arr a -> arr a ()

fromOutput (OPrim p) = p

fromOutput (OPair oa ob) =  layoutPair $
                              (fromOutput oa *** fromOutput ob)  >>>
                              pure (const ())

fromOutput (OLambda i o) =  layoutLambda $
                              pure (\ f -> (f, ()))  >>>
                              second (fromInput i)   >>>
                              pure (uncurry ($))     >>>
                              fromOutput o

{- Or, using arrow notation (not supported by Haddock).

fromOutput (OPair oa ob) =  layoutPair $
                              proc (a,b) -> do
                                ua -< a
                                ub -< b
 where ua = fromOutput oa
       ub = fromOutput ob

fromOutput (OLambda i o) =  layoutLambda $
                              proc f -> do
                                a <- ui -< ()
                                uo -< f a
  where ui = fromInput  i
        uo = fromOutput o

-}
