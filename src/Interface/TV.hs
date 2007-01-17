{-# OPTIONS -fglasgow-exts #-}

{- |
   Module      :  Interface.TV
   Copyright   :  (c) Conal Elliott 2006
   License     :  LGPL

   Maintainer  :  conal@conal.net
   Stability   :  experimental
   Portability :  portable

-}

module Interface.TV
  (
  -- * Tangible values
   TV, CTV, tv, unTv, runTV
  -- * 'Input'
  , Input, iEmpty, iPrim, iPair, iTitle
  -- * 'Output'
  , Output, oEmpty, oPrim, oLambda, oPair, oCompose, oTitle
  -- * Common ins & outs
  , Common, CInput, COutput
  , stringIn, readIn{-, intIn-}, stringOut, showOut, interactLine, interactRSOut
  -- * Default ins & outs
  , DefaultIn(..), DefaultOut(..)
  -- * Kleisli arrows
  , kIn, kOut
  -- ** IO-based
  , KIO, contentsIn, fileIn, interactOut, runIO
  -- * UI
  , UI, islider, runUI
  -- * Misc
  , wrapF, Cofunctor(..)
  ) where

import Graphics.UI.Phooey (UI)

import Interface.TV.Input
import Interface.TV.Output
import Interface.TV.Common
import Interface.TV.Defaults
import Interface.TV.UI
import Interface.TV.Kleisli
import Interface.TV.IO
import Interface.TV.Misc
import Interface.TV.Tangible
