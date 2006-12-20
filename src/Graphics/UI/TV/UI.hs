{-# OPTIONS -fglasgow-exts #-}
{- Module      :  Graphics.UI.TV.UI
   Copyright   :  (c) Conal Elliott 2006
   License     :  LGPL

   Maintainer  :  conal@conal.net
   Stability   :  provisional
   Portability :  portable
-}

-- | 
module Graphics.UI.TV.UI
  (
  -- * Input widgets
    islider
  -- * Output Widgets
  , stringDisplay, showDisplay
  ) where

import Control.Arrow

import Graphics.UI.TV.Input (Input,iPrim)
import Graphics.UI.TV.Output (Output,oPrim)
import Graphics.UI.TV.FromOutput (LayoutArr(..),ToIO(..))

import Graphics.UI.Phooey (UI,fromLeft,fromTop,runUI)
import qualified Graphics.UI.Phooey as Phooey


instance LayoutArr UI where
  layoutPair   = fromLeft
  layoutLambda = fromTop

instance ToIO UI where toIO = runUI

-- | Integer slider
islider :: String -> Int -> (Int,Int) -> Input UI Int
islider label init bounds =
  primIn (pure (const bounds) >>> Phooey.islider init) label

-- Wrap up a Phooey.UI and a label (private)
primIn :: UI () a -> String -> Input UI a
primIn ui label = iPrim (Phooey.title label ui)

-- | Display a string
stringDisplay :: String -> Output UI String
stringDisplay = primOut Phooey.stringDisplay

-- | Display a showable value
showDisplay :: Show a => String -> Output UI a
showDisplay = primOut Phooey.showDisplay

-- Wrap up a UI and a label (private)
primOut :: UI a () -> String -> Output UI a
primOut ui label = oPrim (Phooey.title label ui)
