{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.IO
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- IO-based instances of TV classes 'PresentM', 'ToIOM', and
-- 'CommonInsOuts'
----------------------------------------------------------------------

module Interface.TV.IO
  (
  -- * Types
   KIO
  -- * Inputs
  , contentsIn, fileIn
  -- * Outputs
  , interactOut, interactRS, fileOut
  -- * TVs
  , fromFile, toFile
  -- * Disambiguator
  , runIO
  ) where

import Control.Arrow

import Interface.TV.Input
import Interface.TV.Output
import Interface.TV.Tangible (tv,TV,RunTV,runTV)
import Interface.TV.Kleisli
import Interface.TV.Common


-- | Convenient shorthand for 'Kleisli IO'
type KIO = Kleisli IO

-- There's not much to do here, given Kleisli

instance PresentM IO where
  presentTitleM str io = putStr (str ++ suffix) >> io
   where
     suffix | null str              = ""
            | last str `elem` ".?:" = " "
            | otherwise             = ": "

instance ToIOM IO where toIOM = id

instance CommonInsOuts KIO where
  putString = Kleisli putStrLn          -- or putStr?
  getString = Kleisli (const getLine)

--  getInt    = getIntKIO

{-

getIntKIO :: Int -> (Int,Int) -> KIO () Int
-- Get a string, read, and check bounds.
-- TODO: If the read or bounds check fails, output an error message and
-- keep asking.
getIntKIO dflt (lo,hi) = getString >>> pure (check . readD dflt)
 where
   check val | lo <= val && val <= hi = val
             | otherwise = dflt

-}

{----------------------------------------------------------
    Inputs
----------------------------------------------------------}

-- | 'Input' version of 'getContents'
contentsIn :: Input KIO String
contentsIn = kIn getContents

-- | 'Input' version of 'readFile'
fileIn :: FilePath -> Input KIO String
fileIn name = kIn (readFile name)

{----------------------------------------------------------
    Outputs
----------------------------------------------------------}

-- | Equivalent of 'interact'.  See also 'Interface.TV.interactLine'
interactOut :: Output KIO (String -> String)
interactOut = oLambda contentsIn stringOut

-- | Read+Show of 'interact'
interactRS :: (Read a, Show b)
           => a                         -- ^ default, if read fails
           -> Output KIO (a -> b)
interactRS = readShow interactOut

-- | 'Output' version of 'writeFile'
fileOut :: FilePath -> Output KIO String
fileOut name = kOut (writeFile name)


{----------------------------------------------------------
    TVs
----------------------------------------------------------}

fromFile :: FilePath -> TV KIO (String->String)
fromFile name = tv (oLambda (fileIn name) stringOut) id

toFile :: FilePath -> TV KIO (String->String)
toFile name = tv (oLambda stringIn (fileOut name)) id

{----------------------------------------------------------
    Disambiguator
----------------------------------------------------------}

-- | Many TVs work for all 'CommonInsOuts' arrows.  Applying 'runTV' is
-- then ambiguous.  This type specialization disambiguates.
runIO :: RunTV KIO
runIO = runTV
