{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Package up the various TV modules into one.  Also re-exports
-- DeepArrow modules.
----------------------------------------------------------------------

module Interface.TV
  (
  -- | Inputs -- means of obtaining values
    module Interface.TV.Input
  -- | Outputs (interfaces) -- means of presenting values.
  , module Interface.TV.Output
  -- | Tangible values -- interface (output) and value, combined & separable
  , module Interface.TV.Tangible
  -- | Convert inputs and outputs to arrow values
  , module Interface.TV.Present
  -- | Output transformations, as a deep arrow.
  , module Interface.TV.OFun
  -- | Some common interaction vocabulary 
  , module Interface.TV.Common
  -- | Default inputs & outputs
  , module Interface.TV.Defaults
  -- | Monadic TVs, via Kleisli arrows
  , module Interface.TV.Kleisli
  -- | IO-based instances of TV classes
  , module Interface.TV.IO
  -- | Miscellaneous helpers
  , module Interface.TV.Misc
  -- | Re-exports from DeepArrow
  , module Control.Arrow.DeepArrow
  , module Data.FunArr

  ) where

-- import Graphics.UI.Phooey (UI)

import Interface.TV.Input
import Interface.TV.Output
import Interface.TV.Tangible
import Interface.TV.Present
import Interface.TV.OFun
import Interface.TV.Common
import Interface.TV.Defaults
import Interface.TV.Kleisli
import Interface.TV.IO
import Interface.TV.Misc

import Control.Arrow.DeepArrow
import Data.FunArr
