! Introduction /% -*-Twee-*- (http://www.gimcrackd.com/etc/src/twee.el) %/
This post illustrates an approach to separating logic and IO using the [[TV| http://haskell.org/haskellwiki/TV]] library.  The example is [[from Don Stewart's blog| http://cgi.cse.unsw.edu.au/~dons/blog/2006/12/16#programming-haskell-intro]]. My thanks to Don, as this example inspired me to play with alternatives, which led to generalizing TV from GUIs to a more general notion of "interfaces". The TV approach clarifies the pure logic part and the IO part and is more conveniently composable than the mixed logic/IO formulation.

! What's this?
This post is a literate Haskell program, as well as a TiddlyWiki passage (tiddler).  To run the program, double-click on the entry, copy the markup into file called "Grading.lhs", install [[TV| http://haskell.org/haskellwiki/TV]] 0.1 or later (and the libraries TV [[depends on| http://darcs.haskell.org/packages/TV/TV.cabal]]).

In the text below, you can click boxed ">" symbols to show more detail and click boxed "<" symbols to show less detail.  For example, we begin with a module header. +++
\begin{code}
module Grading where

import Data.List (sort)
import Data.Map (Map,empty,keys,insertWith,findWithDefault)
import Text.Printf

import Control.Arrow.DeepArrow
import Interface.TV -- 0.1
\end{code}
=== 

! Original example
Here's the original version that mixes logic and I/O:
\begin{code}
grades = do
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
\end{code}

! Separating logic and interface
!! A bit of massaging
As a first step, have @draw@ yield a string to be concat'd, rather than performing a side-effect. Then we can easily isolate the pure code and separate out an @IO@ driver. +++
\begin{code}
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
\end{code}
=== 
\begin{code}
type GradingStr = String -> String

gradingStr :: GradingStr

grades_2 = readFile "tasks" >>= return . gradingStr >>= putStr
\end{code}

Let's look at the driver definition (@grades_2@).  It contains three parts: external input (@readFile "tasks"@), logic (@gradingStr@), and external output (@putStr@).  By "external", I mean external to the world of pure values.

!! Declarative interface specification
This plumbing pattern used for @grades_2@ can be abstracted out and used with any pure function.  That style of abstraction would be very useful, but not universal, because it only applies to functions.

A more general approach is to split @grades_2@ into //two// parts (rather than three). One is the pure value (the @gradingStr@ function in this case) and the other is a way to "output" that value, i.e., an //interface//. One way to output a function is to combine a way to input arguments and a way to output results.
\begin{code}
gradingStrOut = oLambda (fileIn "tasks") stringOut
\end{code}
The TV functions @fileIn@ and @stringOut@ are trivial wrappers around
@readFile@ and @putStr@.

Combining this interface with the pure value, we get a "TV" (tangible value):
\begin{code}
gradingStrT :: TV KIO GradingStr
gradingStrT = tv gradingStrOut gradingStr
\end{code}

We can re-express @grades@ simply as running this TV. +++
{{{
*Grading> grades_3
Alonzo	[70.0,99.0]	Average: 84.5
Bob	[80.0,90.0]	Average: 85.0
Don	[69.0,97.0]	Average: 83.0
Henk	[79.0,89.0]	Average: 84.0
Oleg	[77.0,85.0]	Average: 81.0
Simon	[94.0,45.0]	Average: 69.5
}}}
=== 
\begin{code}
grades_3 = runTV gradingStrT
\end{code}

To see the input file, we can use @gradingStrOut@ (defined above) as an interface to the identity function. +++
{{{
*Grading> runTV (tv gradingStrOut id)
Simon	94
Henk	79
Alonzo	70
Don	69
Bob	80
Oleg	77
Henk	89
Bob	90
Simon	45
Alonzo	99
Oleg	85
Don	97
}}}
This pattern of using an identity function and a file is generally useful, so it has a name, "[[fromFile| http://darcs.haskell.org/packages/TV/doc/html/Interface-TV-IO.html#fromFile]]".  The command above is equivalent to "@runTV (fromFile "tasks")@".  (There is also "[[toFile| http://darcs.haskell.org/packages/TV/doc/html/Interface-TV-IO.html#toFile]]".)
=== 

! Deeper interfaces
The string representations used in @grading@ are also aspects of "interfacing" (to file representation and human reader).  To factor out these aspects, re-express @gradingStr@ as a composition of parsing, grading, and unparsing: 
\begin{code}
gradingStr_2 = unparseGrades . grading . parseTasks
\end{code}

Since this sort of wrapping (of @grading@) is common, we have a name for it (@wrapF@ in [[Interface.TV.Misc| http://darcs.haskell.org/packages/TV/doc/html/Interface-TV-Misc.html]]).
\begin{code}
gradingStr_3 :: GradingStr
gradingStr_3 = wrapF unparseGrades parseTasks grading
\end{code}

The data types and conversions result from refactoring @gradingStr@. +++
\begin{code}
type Task    = (String,Double)          -- name and task score
type Grade   = (String,[Double],Double) -- name, scores, average
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
   grades       = foldr insert empty tasks
   insert (s,g) = insertWith (++) s [g]
   summary g s  = (s,marks,avg)
     where
       marks = findWithDefault (error "No such student") s g
       avg   = sum marks / fromIntegral (length marks) :: Double
\end{code}
=== 

Now we can make a @Grading@ output that parses on the way in and unparses on the way out.
\begin{code}
gradingOut = oLambda (fmap parseTasks (fileIn "tasks"))
                     (cofmap unparseGrades stringOut)
\end{code}

Better, define @gradingOut@ via @gradingStrOut@, dual to the formulation of @gradingStr@ via @grading@:
\begin{code}
gradingOut_2 :: Output KIO Grading
gradingOut_2 = wrapO unparseGrades parseTasks gradingStrOut
\end{code}

Finally, combine the interface and the logic:
\begin{code}
gradingT :: TV KIO Grading
gradingT = tv gradingOut_2 grading
\end{code}

\begin{code}
grades_4 = runTV gradingT
\end{code}

! Composition
Above we've made TVs directly, from interface and value.  We can also compose them from simpler TVs.  Here's a simple decomposition of @grading@ into two phases: coalesce tasks for each student, and compute average grades.
\begin{code}
type Coalesced = Map String [Double]

coalesce  :: [Task]    -> Coalesced
summarize :: Coalesced -> [Grade]

grading_2 :: Grading
grading_2 = summarize . coalesce
\end{code}
The implementation is just a refactoring of @grading@. +++
\begin{code}
coalesce tasks = foldr insert empty tasks
 where
   insert (s,g) = insertWith (++) s [g]

summarize grades = map (summary grades) (sort (keys grades))
 where
   summary g s = (s,marks,avg)
     where
       marks = findWithDefault (error "No such student") s g
       avg   = sum marks / fromIntegral (length marks) :: Double
\end{code}
=== 

Let's give each of these pieces its own interface.
\begin{code}
coalesceT = tv (oLambda (fmap parseTasks (fileIn "tasks")) showOut)
	       coalesce

summarizeT = tv (oLambda (readIn empty)
	                 (cofmap unparseGrades defaultOut))
		summarize
\end{code}

We can run these TVs independently.  First @coalesceT@, with input from the tasks file:
{{{
*Grading> runTV coalesceT
fromList [("Alonzo",[70.0,99.0]),("Bob",[80.0,90.0]),("Don",[69.0,97.0]),("Henk",[79.0,89.0]),("Oleg",[77.0,85.0]),("Simon",[94.0,45.0])]
}}}
Running the second phase (pasting in the first phase output as second phase input) gives our summary output. +++
{{{
*Grading> runTV summarizeT
fromList [("Alonzo",[70.0,99.0]),("Bob",[80.0,90.0]),("Don",[69.0,97.0]),("Henk",[79.0,89.0]),("Oleg",[77.0,85.0]),("Simon",[94.0,45.0])]
Alonzo	[70.0,99.0]	Average: 84.5
Bob	[80.0,90.0]	Average: 85.0
Don	[69.0,97.0]	Average: 83.0
Henk	[79.0,89.0]	Average: 84.0
Oleg	[77.0,85.0]	Average: 81.0
Simon	[94.0,45.0]	Average: 69.5
}}}
=== 

We can also compose the two TVs and run the result.
\begin{code}
gradingT_2 = coalesceT ->| summarizeT

grades_5 = runTV gradingT_2
\end{code}
This composition does less work than running each phase separately.  The matching input and output vanish during composition, eliminating conversion of the map to and from a string representation, as well as the writing and reading of those strings.  This same elimination applies to other kinds of interfaces as well, such as GUIs.
