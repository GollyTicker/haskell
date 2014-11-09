
module NNproblemsThird (
                        combinations
                          )
                      where

import Test.QuickCheck
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import Debug.Trace
import Test.HUnit
import Control.Applicative
import System.Random

-- Utilities 000000000000000000000000000000000

rtests :: IO Counts
rtests = runTestTT $ TestList unitsProblemTests

unitsProblemTests :: [Test]
unitsProblemTests = [
                       "testInsertAt" ~:  testInsertAt
                     , "testCombinations" ~:  testCombinations
                     , "testGroup3" ~:  testGroup3
                     , "testGroups" ~:  testGroups
                     , "testLsort" ~:  testLsort
                     , "testLsortPermu" ~:  testLsortPermu
                     , "testLsortPermuModel" ~:  testLsortPermuModel
                     , "testlFreqSort" ~:  testlFreqSort
                    ]
listLimit = 144

-- (<<) is a infix short of map -- should have a low precedence
infixr 5 <<
(<<) :: (a -> b) -> [a] -> [b]
f << ls = map f ls
-- Usage: 
-- *main> (*21) << [-2..6]
-- [-42,-21,0,21,42,63,84,105,126]

infix 1 <<<
infix 1 >>>
(<<<), printLs :: Show b => (a -> b) -> [] a -> IO ()
(>>>) :: Show b => [] a -> (a -> b) -> IO ()
printLs f = mapM_ (print . f)
f <<< ls = printLs f ls
ls >>> f = printLs f ls
-- Sample usage:
-- *main> (\n -> combinations n "acbd") <<< [0..4]
-- [""]
-- ["a","c","b","d"]
-- ["ac","ab","ad","cb","cd","bd"]
-- ["acb","acd","abd","cbd"]
-- ["acbd"]
-- Or: *main> [0..4] >>> \n -> combinations n "acbd"

main :: IO ()
main = do
        putStrLn ">> Running HUnit tests..."
        counts <- rtests
        putStrLn ">> Finished HUnit Tests."
        if failures counts /= 0
            then putStrLn ">> Tests failed."
            else putStrLn ">> All ok."

-- 000000000000000000000000000000000000000000000

-- Problem 21
-- insertAt (one-based indices)
-- inserts the element at the nth place in the list.
-- If the given position is less tahn 1
-- or higher than the # of elements in the list
-- the element is inserted at the beginning/end.
insertAt :: a -> Int -> [] a -> [] a
insertAt elem n ls
             | n <= 1 = elem:ls
             | null ls = [elem]
             | otherwise = x:insertAt elem (n-1) xs
            where
                (x:xs) = ls

testInsertAt :: Test
testInsertAt = TestList [
                          "0abcdef" ~=? insertAt '0' (-1) "abcdef"
                         ,"0abcdef" ~=? insertAt '0' 0 "abcdef"
                         ,"0abcdef" ~=? insertAt '0' 1 "abcdef"
                         ,"a0bcdef" ~=? insertAt '0' 2 "abcdef"
                         ,"ab0cdef" ~=? insertAt '0' 3 "abcdef"
                         ,"abcde0f" ~=? insertAt '0' 6 "abcdef"
                         ,"abcdef0" ~=? insertAt '0' 7 "abcdef"
                         ,"abcdef0" ~=? insertAt '0' 8 "abcdef"
                        ]
--

-- Problem 22
-- range. build a range of all the Ints form a to b. (including both)
range :: Integral a => a -> a -> [] a
range a b = List.unfoldr (increaseUntil b) a

increaseUntil :: Integral a => a -> a -> Maybe (a, a)
increaseUntil b = \accu -> 
               if b < accu then Nothing
               else Just (accu, accu+1)
-- moved the creating fucniton into the top level
-- because internal type signatures have problems with
-- other tyoes of values coming from a clojure.
-- (try moving increase until into a where block and avoid
-- giving the b as an argument and give type signratures for it.
-- tahts the problem I mean.)

-- Solutions: range l r = scanl (+) l (replicate (l - r) 1)
-- seems like unfoldr is too strong for this problem

-- first solution
range' a b
       | a > b = []
range' a b = a:range (succ a) b


qcRange :: (Integral a, Enum a) => a -> a -> Bool
qcRange a b = [a..b] == range a b

qcRange' :: (Integral a, Enum a) => a -> a -> Bool
qcRange' a b = range a b == range' a b

--

-- Problem 23
-- rndSelect. Extract n randomly selected elements form a list.
rndSelect :: Int -> [] a -> IO [a]
rndSelect n ls = do
                    gen <- newStdGen -- getStdGen
                    let len = length ls
                        rnds :: [Int]
                        rnds = (\n -> n `mod` len) << (take n $ randoms gen)
                        selected = (ls !!) << rnds
                    return (selected)

--
-- Problem 24
-- lotto. select n different randoms numbers from 1 to m
lotto :: Int -> Int -> IO [Int]
lotto n m = rndSelect n [1..m]
--

-- Problem 25
-- rndPermutation. a random permutation of a the given list
rndPermutation :: [] a ->  IO [a]
rndPermutation ls = do
                      gen <- newStdGen -- getStdGen
                      return ( fst $ shiftUsingIndices (getRands (length ls) gen) )
                          where
                              shiftUsingIndices rnds = foldl shiftOne ([], ls) rnds
                              
                              getRands :: Int -> StdGen -> [Int]
                              getRands len gen = take len $ randoms gen
                              
                              -- shiftOne shifts a random element from right to left
                              shiftOne :: ([] a, [] a) -> Int -> ([] a, [] a)
                              shiftOne (permu, usable) rnd = (permu', usable')
                                  where
                                      idx = rnd `mod` (length usable)
                                      permu' = usable !! idx : permu
                                      usable' = removeAt (idx+1) usable
removeAt :: Int -> [] a -> [] a
removeAt _ [] = []
removeAt 1 (_:ls) = ls
removeAt n ls@(x:xs)
            | n < 0 = ls
            | otherwise = x:removeAt (n-1) xs
-- from previous questions

-- *NNproblemsThird> rndPermutation "abcdef"
-- "cfbead"
-- *NNproblemsThird> rndPermutation "abcdef"
-- "febacd"
-- *NNproblemsThird> rndPermutation "abcdef"
-- "aecdfb"
-- *NNproblemsThird> rndPermutation "abcdef"
-- "dcafeb"
-- *NNproblemsThird> rndPermutation "abcdef"
-- "ecadbf"

-- Problem 26
-- combinations
-- returns the C(n, k) many picks of k elements from a pool of n elements

combinations :: Int -> [] a -> [] [a]
combinations n ls
        | n <= 0 = [[]]
        | n >= length ls = [ls]
        | otherwise = do 
                       pivot <- pred << [1..length ls]
                       let fromPivot = drop (pivot + 1) ls
                           n' = n - 1
                           rightCombs = combinations n' fromPivot
                       lowerComb <- filter ( ( n'==) . length ) rightCombs
                       return ( ls !! pivot : lowerComb )

testCombinations :: Test
testCombinations = TestList [
                          [""] ~=? combinations 1 ""
                         ,[""] ~=? combinations 0 ""
                         
                         ,[""] ~=? combinations (-1) "abc"
                         ,[""] ~=? combinations 0 "abc"
                         ,["a", "b", "c"] ~=? combinations 1 "abc"
                         ,["ab", "ac", "bc"] ~=? combinations 2 "abc"
                         ,["abc"] ~=? combinations 3 "abc"
                         ,["abc"] ~=? combinations 4 "abc"
                         
                         ,[""] ~=? combinations 0 "abcd"
                         ,["a","b","c","d"] ~=? combinations 1 "abcd"
                         ,[
                            "ab","ac","ad",
                            "bc","bd",
                            "cd"
                          ] ~=? combinations 2 "abcd"
                         ,[
                            "abc","abd",
                            "acd",
                            "bcd"
                          ] ~=? combinations 3 "abcd"
                         ,["abcd"] ~=? combinations 4 "abcd"
                        ]
--

-- Problem 27
-- (A)
-- Given 9 Persons return a list of 3 disjoint subgroups
-- with 2,3 and 4 people.
{-
Example: (the single-quotes ommited)
main> group3 [a,b,c,d,e,f,g,h,i]
[
    [ [a,b], [c,d,e], [f,g,h,i] ]
   ,[ [a,b], [c,d,e], [f,g,h,i] ]
]

permutations inside the same group are considered same.
[[a,b], [c,d]] is the same as [[b,a], [c,d]]

But permutations of the groups to each others are considered different:
[[a,b], [c,d]] is different than [[c,d], [a,b]]

-}

nineppl = ['a'..'i']

-- group3 <2, 3, 4> -- group3 assumes, that all the chars are unique
group3 :: Eq a => [a] -> [] [[a]]
group3 ls
        | length ls /= 9 = error "should have 9 ppl"
        | otherwise = do
                        let usableForFst = ls
                        fstGroup <- combinations 2 usableForFst
                        let usableForSnd = deleteAll fstGroup usableForFst
                        sndGroup <- combinations 3 usableForSnd
                        let usableForThr = deleteAll sndGroup usableForSnd
                            thrGroup = usableForThr  -- there is only one combination now
                        -- thrGroup <- combinations 4 usableForThr
                        return [fstGroup, sndGroup, thrGroup]
--



deleteAll :: Eq a => [] a -> [] a -> [] a
deleteAll dels ls = foldl (\ys x -> List.delete x ys) ls dels

--
testGroup3 :: Test
testGroup3 = TestList [
                       1260 ~=? (length . group3 $ nineppl)
                      ]
--

-- groups <a, b, c>
groups :: Eq a => [Int] -> [a] -> [] [[a]]
groups ns ls
        | not checkPre = error "Group sizes don't sum up to the #elements"
        | otherwise = do
                        let usableForFst = ls
                            (fstN:sndNs) = ns
                        fstGroup <- combinations fstN usableForFst
                        
                        let usableForSnd = deleteAll fstGroup usableForFst
                            (sndN:thrNs) = sndNs
                        sndGroup <- combinations sndN usableForSnd
                        
                        let usableForThr = deleteAll sndGroup usableForSnd
                            (thrN:_) = thrNs
                        thrGroup <- combinations thrN usableForThr
                        
                        return [fstGroup, sndGroup, thrGroup]
                        
                        -- How to refactor this repetition?
        where
            checkPre = (sum ns == length ls
                        && length ns == 3)   -- beacuse its not generalized yet
--
{-
-- Applicatives should work.
ghci> (,) <$> [1..21] $ [3..4]
[(1,3), (1,4), ....]
            
-}

-- REFACTORING DONE IN groups-without-do.notation.hs


tst, tst2 :: Enum a => [] a -> [] a

tst ls = ls >>= (\x -> [x,succ x])

tst2 ls = do
            x <- ls
            elem <- [x, succ x]
            return elem
--

re = let (a,b) = (\ls -> (tst ls, tst2 ls)) [1..5]
       in print a >> print b

testGroups = [
               1260 ~=? (length $ groups [2,3,4] nineppl)
              ,1260 ~=? (length $ groups [2,4,3] nineppl)
              ,1260 ~=? (length $ groups [3,2,4] nineppl)
              ,group3 nineppl ~=? groups [2,3,4] nineppl
              , 756 ~=? (length $ groups [2,2,5] nineppl)
             ]
---

-- Problem 28 (A)
-- sort by length
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = let smaller = lsort $ filter (gtLength x) xs
                   bigger = lsort $ filter (not . gtLength x) xs
               in smaller ++ [x] ++ bigger
                where
                    gtLength :: [a] -> [a] -> Bool
                    gtLength ls1 ls2 = length ls1 > length ls2

-- model implementetion for testing
lsortModel ls = List.sortBy (\xs ys -> length xs `compare` length ys) ls

qcLsort ls = length ls == (length . lsort $ ls)
qcLsortModel ls = lsort ls == lsortModel ls

numList = [[2,2], [4,4,4,4], [3,3,3], [2,2], [1], [2,2]]
numListExpected = [[1], [2,2], [2,2], [2,2], [3,3,3], [4,4,4,4]]


testLsort = TestList [
                        [""] ~=? lsort [[]]
                       ,numListExpected ~=? lsort numList
                       
                       -- testing stability
                       ,lsort [[2,2], [4,4,4,4], [3,3,3], [2,3], [1], [2,4]]
                            ~?= [[1], [2,2], [2,3], [2,4], [3,3,3], [4,4,4,4]]
                      ]
--
testLsortPermu = TestList [numListExpected ~=? lsort permu |
                                permu <- permute numList]
--
testLsortPermuModel = TestList [numListExpected ~=? lsortModel permu |
                                    permu <- permute numList]
--

permute :: Show a => [a] -> [[a]]
permute [] = [[]]
permute (x:xs) = [insertAt x n permu | permu <- permute xs
                                      ,n <- [1..length permu + 1]]
--

-- Problem 28 (B)
-- sort by length frequency
numListFreq = [ [2,2], [4,4,4,4], [3,3,3], [3,3,4], [1], [2,3] ]
numListFreqExpected = [ [1], [4,4,4,4], [2,2], [2,3], [3,3,3], [3,3,4]]

--
lFreqSort :: [[a]] -> [[a]]
lFreqSort ls =
            let groups = List.groupBy (\a b -> length a == length b) . lsortModel $ ls
                groupFreqs = map (\x -> (x, length x)) groups
                sortedByFreq = List.sortBy (\a b -> snd a `compare` snd b) groupFreqs
                extracted = concat. map (\(x,_) -> x) $ sortedByFreq
            in extracted

testlFreqSort = TestList [numListFreqExpected ~=? lFreqSort numListFreq]
