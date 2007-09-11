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
-- Make IO play with TV
----------------------------------------------------------------------

module Interface.TV.IO
  (
  -- * Inputs
    contentsIn, fileIn
  -- * Outputs
  , interactOut, interactRS, fileOut
  -- * TVs
  , fromFile, toFile
  -- * Disambiguator
  , runIO
  ) where

-- import Control.Arrow

import Control.Compose (OI,Flip(..))
-- import Data.Title (Title_f)
-- import Data.Fun (Fun)
import Control.Instances () -- for Monoid/IO instance

import Interface.TV.Input
import Interface.TV.Output
import Interface.TV.Tangible (tv,TV,RunTV,runTV)
import Interface.TV.Common (stringIn,stringOut,readShow)


-- There's not much left to do here, given the IO instances for Title,
-- Pair & Fun, CommonIns, CommonOuts.

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
contentsIn :: Input IO String
contentsIn = iPrim getContents

-- | 'Input' version of 'readFile'
fileIn :: FilePath -> Input IO String
fileIn name = iPrim (readFile name)

{----------------------------------------------------------
    Outputs
----------------------------------------------------------}

-- | Equivalent of 'interact'.  See also 'Interface.TV.interactLine'
interactOut :: Output IO OI (String -> String)
interactOut = oLambda contentsIn stringOut

-- | Read+Show of 'interact'
interactRS :: (Read a, Show b)
           => a                         -- ^ default, if read fails
           -> Output IO OI (a -> b)

interactRS = readShow interactOut

-- | 'Output' version of 'writeFile'
fileOut :: FilePath -> Output IO OI String
fileOut name = oPrim (Flip (writeFile name))

{----------------------------------------------------------
    TVs
----------------------------------------------------------}

-- | Identity function, with 'fileIn' and 'stringOut'
fromFile :: FilePath -> TV IO OI (String->String)
fromFile name = tv (oLambda (fileIn name) stringOut) id

-- | Identity function, with stringIn' and 'fileOut'
toFile :: FilePath -> TV IO OI (String->String)
toFile name = tv (oLambda (stringIn "") (fileOut name)) id

{----------------------------------------------------------
    Disambiguator
----------------------------------------------------------}

-- | Type-disambiguating alias for 'runTV'
runIO :: RunTV IO OI
runIO = runTV
