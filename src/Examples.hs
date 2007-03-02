{-# OPTIONS -fglasgow-exts #-}

-- Some TV examples

module Examples where

import Data.List (sort)

import Interface.TV

-- To pick up the FunArr instance for OFun.  GHC bug?
import Interface.TV.OFun()

-- Try out these the examples with runIO.  For the explicitly IO-typed examples, you
-- can use runIO or runTV.

reverseT :: DefaultOut ([a] -> [a]) => CTV ([a] -> [a])
reverseT = tv (oTitle "reverse" defaultOut) reverse

--  This one reverses twice
revTwice :: CTV (String -> String)
revTwice = reverseT ->| reverseT

---- IO examples

testO :: Output KIO (String -> String)
testO = oLambda (fileIn "test.txt") defaultOut

-- Apply a function on the lines or on the words of a string.
onLines, onWords :: ([String] -> [String]) -> (String -> String)
onLines = wrapF unlines lines
onWords = wrapF unwords words

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
shortO :: FilePath -> Output KIO ([String] -> [String])
shortO path = oLambda (fmap lines (fileIn path)) (cofmap unlines defaultOut)

-- Nearly equivalent, but retains more Output structure:
-- shortO path = cofmap (wrapF unlines lines) (oLambda (fileIn path) defaultOut)

shortT :: FilePath -> Int -> TV KIO ([String] -> [String])
shortT path n = tv (shortO path) (sort . short n)

-- Example: runTV (shortT "c:/conal/Misc/quotes.tw" 100)
