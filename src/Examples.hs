{-# OPTIONS -fglasgow-exts #-}

module Examples where

import Data.List (sort)

import Control.Arrow.DeepArrow
import Data.FunArr
import Interface.TV

main = runBoth shopping


-- Run both UI and IO flavors
runBoth :: CTV a -> IO ()
runBoth tv = runUI tv >> runIO tv

tv0 :: CTV String
tv0 = tv (oTitle "message" stringOut) "Hello World!"

tv1 :: CTV Int
tv1 = tv (oTitle "answer" showOut) (42 :: Int)

reverseT :: DefaultOut ([a] -> [a]) => CTV ([a] -> [a])
reverseT = tv (oTitle "reverse" defaultOut) reverse

--  This one reverses twice
revTwice :: CTV (String -> String)
revTwice = reverseT ->| reverseT

apples, bananas :: CInput Int
apples  = iTitle "apples"  defaultIn
bananas = iTitle "bananas" defaultIn

total :: Show a => COutput a
total = oTitle "total" showOut

shoppingO :: COutput (Int -> Int -> Int)
shoppingO = oTitle "shopping list" $
            oLambda apples (oLambda bananas total)

shopping :: CTV (Int -> Int -> Int)
shopping = tv shoppingO (+)

shoppingPr :: CTV ((Int,Int) -> Int)
shoppingPr = tv ( oTitle "shopping list -- curried" $ 
                  oLambda (iPair apples bananas) total )
                (uncurry (+))

shoppingPr' :: CTV ((Int,Int) -> Int)
shoppingPr' = uncurryA $$ shopping


applesU, bananasU :: Input UI Int
applesU  = iTitle "apples"  (islider 3 (0,10))
bananasU = iTitle "bananas" (islider 7 (0,10))

shoppingUO :: Output UI (Int -> Int -> Int)
shoppingUO = oTitle "shopping list" $
             oLambda applesU (oLambda bananasU total)

shoppingU :: TV UI (Int -> Int -> Int)
shoppingU = tv shoppingUO (+)

shoppingPrU :: TV UI ((Int,Int) -> Int)
shoppingPrU = uncurryA $$ shoppingU


-- This one is polymorphic in value, so say something like
-- "runBoth (sortT :: CTV ([String] -> [String]))".  If you leave out the type
-- annotation, a will default to Int.
sortT :: (Read a, Show a, Ord a) => CTV ([a] -> [a])
sortT = tv (oTitle "sort" $ interactLineRS []) sort


---- Composition.

-- Idea: unwords, sort, words

instance DefaultOut [String] where defaultOut = showOut
instance DefaultIn  [String] where defaultIn  = readIn []


wordsT :: CTV (String -> [String]) 
wordsT = tv ( oTitle "function: words" $
              oLambda (iTitle "sentence in" defaultIn)
                      (oTitle "words out"   defaultOut))
            words

unwordsT :: CTV ([String] -> String) 
unwordsT = tv ( oTitle "function: unwords" $
                oLambda (iTitle "words in"     defaultIn)
                        (oTitle "sentence out" defaultOut))
              unwords

sortWordsT :: CTV (String -> String)
sortWordsT = wordsT ->| sortT ->| unwordsT


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
