{-# OPTIONS -fglasgow-exts #-}

module Examples where

import Data.Map (empty,keys,insertWith,findWithDefault)
import Data.List (sort)
import Text.Printf

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

-- shoppingUO :: Output UI (Int -> Int -> Int)
-- shoppingUO = 
--              oTitle "shopping list" $
--              oLambda applesU (oLambda bananasU total)

shoppingU :: TV UI (Int -> Int -> Int)
shoppingU = tv ( oTitle "shopping list" $
                 oLambda applesU (oLambda bananasU total) )
               (+)

shoppingPrU :: TV UI ((Int,Int) -> Int)
shoppingPrU = uncurryA $$ shoppingU


-- This one is polymorphic in value, so say something like
-- "runBoth (sortT :: CTV ([String] -> [String]))".  If you leave out the type
-- annotation, a will default to Int.
sortT :: (Read a, Show a, Ord a) => CTV ([a] -> [a])
sortT = tv (oTitle "sort" $ interactRSOut []) sort


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

--  :: TV KIO (String -> String)

io3 = tv testO (onLines reverse)            -- reverse the lines
io4 = tv testO (onWords reverse)            -- reverse words
io5 = tv testO (perLine (onWords reverse))  -- reverse words on each line

io3' = tv testO (perLine reverse)           -- reverse each line
io4' = tv testO (perWord reverse)           -- reverse each word
io5' = tv testO (perLine (perWord reverse)) -- reverse each word, leaving lines

-- From http://cgi.cse.unsw.edu.au/~dons/blog/2006/12/16
grades0 = do
    src <- readFile "tasks"
    let pairs  = map (split.words) (lines src)
        grades = foldr insert empty pairs
    mapM_ (draw grades) (sort (keys grades))
  where
    insert (s, g) = insertWith (++) s [g]
    split [name,mark] = (name, read mark)
    draw g s = printf "%s\t%s\tAverage: %f\n" s (show marks) avg
      where
        marks = findWithDefault (error "No such student") s g
        avg   = sum marks / fromIntegral (length marks) :: Double

-- To better separate IO from functional, have draw return a string to be
-- concat'd.

grades1 = readFile "tasks" >>= return . gradingStr >>= putStr

type GradingStr = String -> String

gradingStr :: GradingStr
gradingStr src = concatMap (draw grades) (sort (keys grades))
  where
    pairs  = map (split.words) (lines src)
    grades = foldr insert empty pairs
    
    insert (s, g) = insertWith (++) s [g]
    split [name,mark] = (name, read mark)
    draw g s = printf "%s\t%s\tAverage: %f\n" s (show marks) avg
      where
        marks = findWithDefault (error "No such student") s g
        avg   = sum marks / fromIntegral (length marks) :: Double

-- Or in TV style:

tasksStrIn   = fileIn "tasks"
gradesStrOut = oTitle "Grades" stringOut

o6 = oLambda tasksStrIn gradesStrOut

io6 = tv o6 gradingStr


-- The string representations are also aspects of "I/O" (input from file
-- storage and output to human reader).  To factor out these I/O aspects,
-- we'll re-express gradingStr as a composition of parsing, grading, and
-- unparsing: 
-- 
--   gradingStr = unparseSummaries . grading . parseTasks
--
-- Since this sort of wrapping (of grading) is common, we have a name for
-- it (wrapF in Interface.TV.Misc).

gradingStr' :: GradingStr
gradingStr' = wrapG grading

wrapG :: ([Task] -> [Grade]) -> String -> String
wrapG = wrapF unparseGrades parseTasks


-- Define the data types and conversion:

type Task    = (String,Double)          -- ^ name and task score
type Grade   = (String,[Double],Double) -- ^ name, scores, average
type Grading = [Task] -> [Grade]

parseTasks    :: String -> [Task]
unparseGrades :: [Grade] -> String
grading       :: Grading

parseTasks src = map (split.words) (lines src)
 where
   split [name,mark] = (name, read mark)

unparseGrades = concatMap draw
 where
   draw (s,marks,avg) = printf "%s\t%s\tAverage: %f\n" s (show marks) avg

grading tasks = map (summary grades) (sort (keys grades))
 where
   grades = foldr insert empty tasks
   insert (s, g) = insertWith (++) s [g]
   summary g s = (s,marks,avg)
     where
       marks = findWithDefault (error "No such student") s g
       avg   = sum marks / fromIntegral (length marks) :: Double


-- Now we can make a Grading output that parses on the way in and
-- unparses on the way out.
o7 :: Output KIO Grading
o7 = oLambda (fmap parseTasks tasksStrIn)
             (cofmap unparseGrades gradesStrOut)

-- Or define o7 via o6, dual to formulation of gradingStr via grading:
o7' :: Output KIO Grading
o7' = cofmap wrapG o6

io7 = tv o7' grading
