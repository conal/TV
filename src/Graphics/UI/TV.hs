{-# OPTIONS -fglasgow-exts #-}

{- Module      :  Graphics.UI.TV
   Copyright   :  (c) Conal Elliott 2006
   License     :  LGPL

   Maintainer  :  conal@conal.net
   Stability   :  provisional
   Portability :  portable
-}


-- | A library of /tangible values/, which combine compositionality and
-- user-friendliness.

module Graphics.UI.TV
  (
  -- * Tangible values
   TV(..), tv, runTV
  -- * 'Input's
  , Input, iPrim, iPair, islider
  -- * 'Output's
  , Output, oPrim, oLambda, oPair, stringDisplay, showDisplay
  ) where

import Control.Arrow

import Graphics.UI.Phooey (UI,runUI)

import Graphics.UI.TV.Input
import Graphics.UI.TV.Output
import Graphics.UI.TV.FromOutput
import Graphics.UI.TV.UI


-- | Tangible values (TVs)
data TV arr a = TV (Output arr a) a

-- | Make a 'TV'
tv :: Output arr a -> a -> TV arr a
tv = TV

-- | Run a 'TV'
runTV :: (ToIO arr, LayoutArr arr) => TV arr a -> IO ()
runTV (TV o a) = toIO (pure (const a) >>> fromOutput o)
