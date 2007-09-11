{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.UI
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Graphical 'UI' instances of TV classes 'Present', 'ToIO', and
-- 'CommonInsOuts'
----------------------------------------------------------------------

module Interface.TV.UI
  (
   -- * Input
     islider
   -- * Disambiguator
   , runUI
  ) where

import Control.Arrow (pure,(>>>))

import Graphics.UI.Phooey hiding (runUI,islider)
import qualified Graphics.UI.Phooey as Ph (islider)

import Interface.TV.Input (Input,iPrim)
import Interface.TV.Present (Present(..))
import Interface.TV.Common (CommonInsOuts(..))
import Interface.TV.Tangible (RunTV,runTV)
-- import Interface.TV.Misc (ToIO(..))


{----------------------------------------------------------
    Instances
----------------------------------------------------------}

instance Present UI where
  presentPair    = fromLeft
  presentLambda  = fromTop
  presentTitle   = title
  -- presentCompose = id

-- For the Eros version, I'll want presentCompose to replace all inner
-- handles with one outer handle.

instance ToIO UI where toIO = runNamedUI "TV + Phooey"

instance CommonInsOuts UI where
  putString = stringDisplay
  getString = textEntry


{----------------------------------------------------------
    Input
----------------------------------------------------------}

islider :: Int -> (Int,Int) -> Input UI Int
islider initial bounds =
  iPrim (pure (const bounds) >>> Ph.islider initial)


{----------------------------------------------------------
    Disambiguator
----------------------------------------------------------}

-- | Many TVs work for all 'CommonInsOuts' arrows.  Applying 'runTV' is
-- then ambiguous.  This type specialization disambiguates.
runUI :: RunTV UI
runUI = runTV
