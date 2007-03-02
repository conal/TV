{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.Present
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Convert inputs and outputs to arrow values
----------------------------------------------------------------------

module Interface.TV.Present
  (
   present, accept, Present(..)
  ) where

import Control.Arrow

import Interface.TV.Input
import Interface.TV.Output
-- import Interface.TV.Misc (ToIO(..))


-- | 'present' requires a bit of arrow-specific help.  For instance, in
-- UIs, pairs are presented horizontally, lambdas vertically, and titles
-- with a labeled box.  I'm not really comfortable with this part of the
-- design, especially the specificity of addressing titles here.  All of
-- the methods here have do-nothing defaults, so you can simply say
-- @instance Present arr@ for your arrow @arr@.
class Arrow (~>) => Present (~>) where
  presentPair     :: a ~> b -> a ~> b
  presentPair     =  id
  presentLambda   :: a ~> b -> a ~> b
  presentLambda   =  id
  presentTitle    :: String -> a ~> b -> a ~> b
  presentTitle _  =  id
--   presentCompose  :: a ~> b -> a ~> b
--   presentCompose  =  id

-- | Convert an 'Input' into an arrow value.
accept :: Present (~>) => Input (~>) b -> () ~> b

-- accept IEmpty          = arr $ const $ error "cannot get value from empty input"

accept (IPrim p)       = p

accept (IPair ia ib)   = presentPair (accept ia &&& accept ib)

accept (ITitle str i)  = presentTitle str (accept i)

-- accept (ICompose a ab) = presentCompose (accept a >>> ab)

-- | Convert an 'Output' into an arrow value
present :: Present (~>) => Output (~>) a -> a ~> ()

-- present OEmpty = arr $ const ()
--                  -- presentEmpty

present (OPrim p) = p

present (OPair oa ob) =  presentPair $
                              (present oa *** present ob)  >>>
                              pure (const ())

present (OLambda i o) =  presentLambda $
                              pure (\ f -> (f, ()))  >>>
                              second (accept i)      >>>
                              pure (uncurry ($))     >>>
                              present o

-- present (OCompose ab b) = presentCompose $
--                                ab >>> present b

present (OTitle str o)  = presentTitle str (present o)
