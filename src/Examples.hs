{-# LANGUAGE OverlappingInstances, UndecidableInstances
           , IncoherentInstances, FlexibleContexts #-}
-- For ghc 6.6 compatibility
-- {-# OPTIONS -fglasgow-exts #-}

---- Some TV examples.  See also GuiTV.

module Examples where

import Data.List (sort)
import Control.Compose (cofmap,OI)
import Control.Arrow.DeepArrow ((->|))

-- TypeCompose
import Data.Title

import Interface.TV

-- To pick up the FunArr instance for OFun.  GHC bug.
import Interface.TV.OFun()

-- Try out these the examples with runIO.  For the explicitly IO-typed examples, you
-- can use either runIO or runTV.

reverseT :: CTV (String -> String)
reverseT = tv (title "reverse" defaultOut) reverse

-- I don't know why this more general version fails.
-- 
--   reverseT :: ( Read a, Show a , CommonIns src , CommonOuts snk ) =>
--               TV src snk ([a] -> [a])
--
-- GHC wants to use the [a] rather than the String instances of DefaultIn
-- and DefaultOut.  I thought the rule is most specific instance wins.

--  This one reverses twice
revTwice :: CTV (String -> String)
revTwice = reverseT ->| reverseT

-- Same problem with more general type:
--   revTwice :: (Read a, Show a, CommonIns src, CommonOuts snk) =>
--               TV src snk ([a] -> [a])



---- IO examples

testO :: Output IO OI (String -> String)
testO = oLambda (fileIn "test.txt") defaultOut

-- Apply a function on the lines or on the words of a string.
onLines, onWords :: ([String] -> [String]) -> (String -> String)
onLines f = unlines . f . lines
onWords f = unwords . f . words

perLine,perWord :: (String -> String) -> (String -> String)
perLine f = onLines (map f)
perWord f = onWords (map f)

-- io3, ... :: TV KIO (String -> String)

io3 = tv testO (onLines reverse)            -- reverse the lines
io4 = tv testO (onWords reverse)            -- reverse words
io5 = tv testO (perLine (onWords reverse))  -- reverse words on each line

io3' = tv testO (perLine reverse)           -- reverse each line
io4' = tv testO (perWord reverse)           -- reverse each word
io5' = tv testO (perLine (perWord reverse)) -- reverse each word, leaving lines


-- Find lines with 0 < length < n
short :: Int -> [String] -> [String]
short n = filter (tween 1 n . length)
  where tween lo hi i = lo <= i && i < hi

-- Extract lines from a file before a function and combine lines after.
shortO :: FilePath -> Output IO OI ([String] -> [String])
shortO path = oLambda (fmap lines (fileIn path)) (cofmap unlines defaultOut)

-- Nearly equivalent, but retains more Output structure:
-- shortO path = cofmap (wrapF unlines lines) (oLambda (fileIn path) defaultOut)

shortT :: FilePath -> Int -> TV IO OI ([String] -> [String])
shortT path n = tv (shortO path) (sort . short n)

-- Example: runTV (shortT "c:/conal/Misc/quotes.tw" 100)
