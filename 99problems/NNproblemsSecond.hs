
module NNproblemsSecond (
                          --  encode
                           removeAt
                          )
                      where

import Test.QuickCheck
import qualified Data.Set as Set
import qualified Data.List as List
import Debug.Trace
import Test.HUnit
import NNproblemsFirst

-- Utilities 000000000000000000000000000000000

applyJoin :: (a -> b) -> (b -> b -> c) -> a -> a -> c
applyJoin f g a b = g (f a) (f b)

appliedJoin :: (a -> b) -> (a -> b) -> (b -> b -> c) -> a -> c
appliedJoin f g h a = h (f a) (g a)

clampTo :: Int -> Int -> Int
clampTo listLimit n
        | n <= 0 = 0
        | n >= listLimit = listLimit
        | otherwise = n

rtests :: IO Counts
rtests = runTestTT $ TestList unitsProblemTests

unitsProblemTests :: [Test]
unitsProblemTests = [
                       "testsP11" ~: testsP11
                     , "testsP13" ~:  testsP13
                     , "testsDupli" ~:  testsDupli
                     , "testsRepli" ~:  testsRepli
                     , "testsRepli'" ~:  testsRepli'
                     , "testsDropEvery" ~:  testsDropEvery
                     , "testSplit" ~:  testSplit
                     , "testSlice" ~:  testSlice
                     , "testRotate" ~:  testRotate
                    ]

listLimit = 144

main :: IO ()
main = do
        putStrLn ">> Running HUnit Tests..."
        rtests
        putStrLn ">> Finished HUnit Tests."
        putStrLn ">> All ok."


-- 000000000000000000000000000000000000000000000

testEncodes :: ([Char] -> [Occurrence Char]) -> Test
testEncodes f = TestList [
              [] ~?= ""
            , [Sgl 'a'] ~?= f "a"
            , [Mlt 3 'a'] ~?= f "aaa"
            , [Mlt 3 'a', Sgl 'b'] ~?= f "aaab"
            , [Mlt 2 'b', Mlt 3 'a', Sgl 'b'] ~?= f "bbaaab"
            , [Mlt 2 'b', Sgl 'a', Sgl 'b'] ~?= f "bbab"
           ]

-- Problem 11
-- encodeModified
data Occurrence a = Sgl a | Mlt Int a deriving (Eq, Show)

encodeModified :: (Eq a) => [a] -> [Occurrence a]
encodeModified = map (tupleToOccurrence) . encode
            where
                tupleToOccurrence (n, a)
                            | n==1 = Sgl a
                            | otherwise = Mlt n a
--
testsP11 = "P11:" ~: testEncodes encodeModified


-- Problem 12
-- decodeModified
decodeModified :: (Eq a) => [Occurrence a] -> [a]
decodeModified = concatMap runDecode
                where
                    runDecode occ = case occ of (Mlt n a) -> take n $ repeat a
                                                (Sgl a)   -> [a]

testEnDeCodeModified :: Eq a => [a] -> Bool
testEnDeCodeModified = appliedJoin (decodeModified . encodeModified) id (==)


-- Problem 13
-- encodeDirect
encodeDirect :: (Eq a) => [a] -> [Occurrence a]
encodeDirect [] = []
encodeDirect ls = snd $ foldl addEach (fstChar , [Sgl fstChar]) $ tail ls
            where
                fstChar = head ls
                addEach (currCh, occs) char
                            | currCh == char = (char, addedSame)
                            | otherwise = (char, startedNew)
                        where
                            addedSame = (init occs) ++ [addIntoOcc $ last occs]
                            addIntoOcc (Mlt n _) = Mlt (n+1) char
                            addIntoOcc (Sgl _) = Mlt 2 char
                            startedNew = occs ++ [Sgl char]

testsP13 = "P13:" ~: testEncodes encodeDirect

testQCp13 :: Eq a => [a] -> Bool
testQCp13 = appliedJoin (decodeModified . encodeDirect) id (==)


-- Problem 14
-- dupli
dupli :: [a] -> [a]
-- dupli = foldr f []
    -- where
        -- f :: a -> [a] -> [a]            -- embedded typesigns!
        -- f x = concatN 2 (x:) -- means (x:) . (x:) => \accu -> x:x:accu
dupli = repli' 2

testsDupli :: Test
testsDupli = TestList [
                          "" ~=? dupli "" 
                        , "aa" ~=? dupli "a"
                        , "aabb" ~=? dupli "ab"
                        , "aaccbb" ~=? dupli "acb"
                        , "ccccbb" ~=? dupli "ccb"
                      ]
testQCpDupli :: [a] -> Bool
testQCpDupli = appliedJoin ((2*) . length) (length . dupli) (==)


-- Problem 15
-- repli
repli :: Int -> [a] -> [a]
repli n = concatMap (replicate n)

repli' :: Int -> [a] -> [a]
repli' n = foldr f []
    where
        f :: a -> [a] -> [a]
        f x = concatN n (x:)

concatN :: Int -> (a -> a) -> a -> a
concatN n f
        | n == 0 = id
        | n > 0 = f . (concatN (n-1) f)

concatN _ _ = error "Cannot repeat negative number of times."

testsRepli :: Test
testsRepli = TestList [
                          "" ~=? repli 0 "" 
                        , "" ~=? repli 1 ""
                        , "" ~=? repli 0 "c"
                        , "c" ~=? repli 1 "c"
                        , "cccc" ~=? repli 4 "c"
                        , "bbbbbbccc" ~=? repli 3 "bbc"
                      ]

testsRepli' :: Test
testsRepli' = TestList [
                          "" ~=? repli' 0 "" 
                        , "" ~=? repli' 1 ""
                        , "" ~=? repli' 0 "c"
                        , "c" ~=? repli' 1 "c"
                        , "cccc" ~=? repli' 4 "c"
                        , "bbbbbbccc" ~=? repli' 3 "bbc"
                      ]

testQCpRepli1 :: Int -> [a] -> Bool
testQCpRepli1 n ls = let n' = clampTo listLimit n
                in appliedJoin ((n'*) . length) (length . (repli n')) (==) ls


testQCpRepli2 :: Eq a => [a] -> Bool
testQCpRepli2 = appliedJoin dupli (repli 2) (==)

testQCpRepli' :: Eq a => Int -> [a] -> Bool
testQCpRepli' n = let n' = clampTo listLimit n
                in appliedJoin (repli n') (repli' n') (==)

-- Problem 16
-- dropEvery
dropEvery :: Show a => Int -> [a] -> [a]
dropEvery n ls
            | length ls < n = ls
            | n > 0 = beg ++ dropEvery n end
            where
                (beg, _:end) = splitAt (n - 1) ls
dropEvery _ _ = error "Can't drop every 0th element!"

dropEvery' n ls = let len = length ls
            in [ls !! (idx - 1) | idx <- [1..len],
                            idx `mod` n /= 0
                ]

testQCp16 :: (Show a, Eq a) => Int -> [a] -> Property
testQCp16 n ls = (n' /= 0) ==> appliedJoin (dropEvery n') (dropEvery' n') (==) ls
                        where
                            n' = n `mod` listLimit
testsDropEvery :: Test
testsDropEvery = TestList [
                               "" ~=? dropEvery' 1 ""
                            ,  "" ~=? dropEvery' 1 "absckjdf"
                            ,  "abdeghjk" ~=? dropEvery' 3 ['a'..'k']
                            ,  ['a'..'k'] ~=? dropEvery' 120 ['a'..'k']
                            , map (pred . (*2)) [1..6] ~=? dropEvery' 2 [1..11]
                          ]
testQCp16' :: (Show a, Eq a) => Int -> [a] -> Property
testQCp16' n ls = (n' /= 0) ==> dropEvery' n' ls == ls
                        where
                            n' = listLimit + (n `mod` listLimit)
--

-- Problem 17
-- split
split :: Int -> [a] -> ([a], [a])
split _ [] = ([], [])
split n ls
        | n <= 0 = ([], ls)
split n ls = (beg, end)
            where
                beg = take n ls
                end = (concatN n (\x -> if null x then [] else tail x)) ls
--  the expression of end is like drop



testQCpSplit :: Int -> Bool
testQCpSplit n = (([],[])::([Int], [Int])) == split n []

testQCpSplit' :: Eq a => Int -> [] a -> Bool
testQCpSplit' splitN ls = splitAt splitN' ls == split splitN' ls
                        where
                            splitN' = splitN `mod` listLimit

testQCpSplit'' :: Eq a => Int -> [] a -> Bool
testQCpSplit'' splitN ls = ([], ls) == split splitN' ls
                        where
                            splitN' = negate $ splitN `mod` listLimit

testSplit :: Test
testSplit = TestList [
                         ("","") ~=? split 2 ""
                       , ([1..3],[4..15]) ~=? split 3 [1..15]
                       , ([],[1..15]) ~=? split (-2) [1..15]
                       , ([],[1..15]) ~=? split 0 [1..15]
                       , ([1..5],[]) ~=? split 11 [1..5]
                       , ([1..5],[]) ~=? split 11 [1..5]
                     ]


-- Problem 18
-- slice
slice :: Int -> Int -> [a] -> [a]
slice beg end ls
            | beg > end = []
            | otherwise = let (_, tl) = split (beg - 1) ls
                            in fst $ split (end - beg + 1) tl

testSliceQC :: Int -> Int -> Int -> Bool
testSliceQC n1 n2 len = slice beg end [1..len'] == [beg..end]
                    where
                        len' = succ $ (abs len) `mod` listLimit
                        n1' = succ (mod n1 len')
                        n2' = succ (mod n2 len')
                        beg = min n1' n2'
                        end = max n1' n2'

testSlice :: Test
testSlice = TestList [
                         "a" ~=? slice 1 1 "a"
                       , "" ~=? slice 1 0 "aasd"
                       , "b" ~=? slice 2 2 "abc"
                       , "b" ~=? slice 2 2 "abcasd"
                       , "bcas" ~=? slice 2 5 "abcasd"
                       , "ab" ~=? slice (-2) 2 "ab"
                       , "de" ~=? slice 4 7 "abcde"
                       , "" ~=? slice 6 7 "abcde"
                     ]
--


-- Problem 19
-- rotate
rotate :: Int -> [] a -> [] a
rotate n ls = do 
                let len = length ls
                idx <- [0.. len - 1]
                let idx' = (idx + n) `mod` len
                return $ ls !! idx'
-- monadic sugar 

{-
Haskell Wiki Solutions:

rotate xs n = take len . drop (n `mod` len) . cycle $ xs
    where len = length xs

-}

testRotate :: Test
testRotate = TestList [
                         [4,5,1,2,3] ~=? rotate (-7) [1..5]
                       , [3,4,5,1,2] ~=? rotate (-3) [1..5]
                       , [5,1,2,3,4] ~=? rotate (-1) [1..5]
                       , [1,2,3,4,5] ~=? rotate 0 [1..5]
                       , [2,3,4,5,1] ~=? rotate 1 [1..5]
                       , [1,2,3,4,5] ~=? rotate 5 [1..5]
                       , [2,3,4,5,1] ~=? rotate 6 [1..5]
                      ]
-- Problem 20
-- removeAt: remove the nth element in the list (that means one-based indices)
removeAt :: Int -> [] a -> [] a
removeAt _ [] = []
removeAt 1 (_:ls) = ls
removeAt n ls@(x:xs)
            | n < 0 = ls
            | otherwise = x:removeAt (n-1) xs

testRemoveAt :: Test
testRemoveAt = TestList [
                         [-2..5] ~=? removeAt 0 [-2..5]
                       , [-2..5] ~=? removeAt (-3) [-2..5]
                       , [-2..5] ~=? removeAt 9 [-2..5]
                       , [-1..5] ~=? removeAt 1 [-2..5]
                       , [-1..5] ~=? removeAt 3 [-2..5]
                       , (-2):[-1,1..5] ~=? removeAt 1 [-2..5]
                       , [-2..3] ++ [5] ~=? removeAt 7 [-2..5]
                       , [-2..4] ~=? removeAt 8 [-2..5]
                      ]














