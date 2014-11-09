
module NNproblemszFourth (
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
                       "testIsPrime" ~:  testIsPrime
                     , "testIsPrime2" ~:  testIsPrime2
                     , "testGCD" ~:  testGCD
                     , "testGCDModel" ~:  testGCDModel
                     , "testCoprime" ~:  testCoprime
                     , "testPhi" ~:  testPhi
                     , "testFactors" ~:  testFactors
                     , "testFactors2" ~:  testFactors2
                     , "testFactorsMult" ~:  testFactorsMult
                     , "testFactorsMult2" ~:  testFactorsMult2
                     , "testPhi'" ~:  testPhi'
                     , "testPrimeR" ~:  testPrimeR
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

-- Helpful for drawing on console
draw :: Integral a => a -> String
draw n = replicate (fromIntegral n) '*'

main :: IO ()
main = do
        putStrLn ">> Running HUnit tests..."
        counts <- rtests
        putStrLn ">> Finished HUnit Tests."
        if failures counts /= 0
            then putStrLn ">> Tests failed."
            else putStrLn ">> All ok."
--
mapForTest :: (Show a, Eq a) => [(a,a)] -> Test
mapForTest = TestList . map (\(was,exp) -> was ~=? exp)

mapTestVal :: (Show a, Eq a, Show b, Eq b) => [(b,a,a)] -> Test
mapTestVal = TestList . map (\(val, was,exp) -> (val,was) ~=? (val, exp))
-- 000000000000000000000000000000000000000000000

-- Problem 31
-- isPrime
isPrime :: Integral a => a -> Bool
isPrime p
        | p <= 1 = False
        | otherwise = all ((0/=) . mod p) [2 .. upperLim p] -- 2.59 secs
        -- | otherwise = all ((0/=) . mod p) . filter isPrime $ [2 .. upperLim p]
                                -- too long -_-
        where
            upperLim :: Integral a => a -> a
            upperLim = floor . sqrt . fromIntegral
--
-- the time behind the versions say how much time they needed
-- for ghci> take 15000 primes

-- first 31 primes
samplePrimes = [  2,  3,  5,  7, 11, 13, 17,
                 19, 23, 29, 31, 37, 41, 43,
                 47, 53, 59, 61, 67, 71, 73,
                 79, 83, 89, 97,101,103,107,
                109,113,127]
--
primes :: Integral a => [a] -- needs 2 as start value
primes = 2:filter isPrime [3..]

testIsPrime = TestList . map (\p -> (p,True) ~=? (p, isPrime p)) $ samplePrimes

testIsPrime2 = samplePrimes ~=? take (length samplePrimes) primes



-- Problem 32
-- greatest common divisor using Euklids Algorithm
-- gcd

-- From English Wikipedia: http://en.wikipedia.org/wiki/Euclidean_algorithm
-- function gcd(a, b)
    -- while b ≠ 0
       -- t := b
       -- b := a mod b
       -- a := t
    -- return a
--

-- Translated into recursion
{-
gcd(a,b) = { a, a mod b = 0
           { gcd(b, a mod b), else  -- not quite right.....
-}
mygcd :: Integral a => a -> a -> a
mygcd a b
      | rest == 0 = min a b
      | otherwise = gcd b rest
      where
        rest = a `mod` b
--
--
testGCD = mapForTest
                [    (mygcd 36 63, 9)
                    ,(mygcd (-3) (-6), 3)
                    ,(mygcd (-3) 6, 3)
                ]
--
-- <$> is fmap.
-- first it maked functions out of the raw values in the first int-list
-- and then <*> applies them all on the second int-list
testGCDModel = TestList $ map gcdIsSame gcdTestValues

gcdTestValues :: Integral a => [(a, a)]
gcdTestValues = (,) <$> [1..14] <*> [1..14]

gcdIsSame :: (Show a, Integral a) => (a, a) -> Test
gcdIsSame (a, b) = (a, b, gcd a b) ~=? (a, b, mygcd a b)

-- Problem 33
-- coprime
coprime :: Integral a => a -> a -> Bool
coprime = ((==1) .) . gcd   -- this means just piping the output into gcd
--                  -- this has to be written so because
                    -- more than just one argument is being piped
-- generalization: http://stackoverflow.com/questions/20279306/what-does-f-g-mean-in-haskell
testCoprime = mapForTest
                [
                  (coprime 2 3, True)
                 ,(coprime 2 5, True)
                 ,(coprime 6 3, False)
                 ,(coprime 24 22, False)
                 ,(coprime 5 22, True)
                ]
--

-- Problem 34
-- totient phi(n) = #coprimes of n
phi :: Integral a => a -> Int
phi n = length . filter (coprime n) $ [1..n]

samplePhiValues :: Integral a => [(a,a)]
samplePhiValues = zip [1..] [
            --  +0 	+1 	+2 	+3 	+4 	+5 	+6 	+7 	+8 	+9
                    1, 	1, 	2, 	2, 	4, 	2, 	6, 	4, 	6,
    {-10+-} 	4, 	10, 4, 	12,	6,	8,	8,	16,	6,	18,
    {-20+-} 	8,	12, 10,	22,	8,	20,	12,	18,	12,	28,
    {-30+-} 	8, 	30, 16,	20,	16,	24,	12,	36,	18,	24,
    {-40+-} 	16, 40, 12,	42,	20,	24,	22,	46,	16,	42,
    {-50+-} 	20, 32, 24,	52,	18,	40,	24,	36,	28,	58
            ]
-- from: https://en.wikipedia.org/wiki/Euler%27s_totient_function

phiValues :: [(Int, Int)]
phiValues = zip [1..] $ map phi [1..]

-- visualize:
visualizePhi :: Int -> IO ()
visualizePhi n = take n phiValues >>> \(a,b) -> (a,map (const '*') [1..b])

testPhi :: Test
testPhi = mapForTest . map (\(n,x) -> (phi n,x)) $ samplePhiValues 


-- Problem 35
-- Primer factorization
factors :: Integral a => a -> [a]
factors 1 = []
factors n
        | n `elem` (takeWhile (< n) primes) = [n]
        | otherwise = fstFactor : factors (n `div` fstFactor)
            where
                dividedBy = (0==) . mod n
                fstFactor = head . filter dividedBy $ primes
--

testFactors = mapForTest [
                           ((factors 1), [])
                          ,((factors 2), [2])
                          ,((factors 3), [3])
                          ,((factors 4), [2,2])
                          ,((factors 17), [17])
                          ,((factors 18), [2,3,3])
                         ]
--
testFactors2 = mapForTest . map (\p -> (factors p, [p])) . take 150 $ primes
--

-- Problem 36
-- Primer factorization with multiplicity
factorsMultipicity :: Integral a => a -> [(a,Int)]
factorsMultipicity = map (\x -> (head x, length x)) . List.group . factors
--

testFactorsMult = mapForTest [
                           ((factorsMultipicity 1), [])
                          ,((factorsMultipicity 2), [(2,1)])
                          ,((factorsMultipicity 3), [(3,1)])
                          ,((factorsMultipicity 4), [(2,2)])
                          ,((factorsMultipicity 17), [(17,1)])
                          ,((factorsMultipicity 18), [(2,1),(3,2)])
                         ]
--
testFactorsMult2 = mapTestVal . map (\p -> (p, factorsMultipicity p, [(p,1)])) . take 150 $ primes
--

-- Problem 37
-- Euler's totient function phi(m) - improved
-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
         -- (p2 - 1) * p2 ** (m2 - 1) * 
         -- (p3 - 1) * p3 ** (m3 - 1) * ...
phi' :: Int -> Int
phi' = foldl (flip f) 1 . factorsMultipicity
        where
            f (p, m) = (*) $ (p - 1) * p ^ (m - 1)
--

testPhi' :: Test
testPhi' = mapForTest . map (\(n,x) -> (phi' n,x)) $ samplePhiValues 

qcPhi' :: Int -> Bool
qcPhi' n = phi' n' == phi n'
            where
                n' = 1+ abs n `mod` 10000
--

-- Problem 38
-- Compare both Euler totient functions:
-- Example: 10090
{-
My first implementation of the improved phi
is slower than the original phi.
-}

-- Problem 39
-- primeR
primeR :: Int -> Int -> [] Int
primeR n m = takeWhile (<=m) . dropWhile (<n) $ primes

testPrimeR = mapForTest [
                            (primeR 2 7, [2,3,5,7])
                           ,(primeR 1 7, [2,3,5,7])
                           ,(primeR 2 8, [2,3,5,7])
                           ,(primeR 1 8, [2,3,5,7])
                           ,(primeR 3 9, [3,5,7])
                           ,(primeR 3 11, [3,5,7,11])
                           ,(primeR 3 12, [3,5,7,11])
                           ,(primeR 3 3, [3])
                           ,(primeR 5 3, [])
                           ,(primeR 5 3, [])
                           ,(primeR 150 180, [151,157,163,167,173,179])
                        ]
--


-- Problem 40
-- goldbach - Give the two primes a given number n
-- is the sum of
{-
goldbach 28
(5, 23)
-}
-- not actually proven to terminate because
-- goldbach conjecture is still a conjecture....
-- However, it's been experimentally tested
-- for very high numbers
goldbach :: Integer -> (Integer, Integer)
goldbach n
    | not . and $ [n > 2, even n] = error "Goldbach conjecture applicable for n > 2 and even n."
    | otherwise = head
                    [ (a,b) |
                        a <- takeWhile (<=n `div` 2) primes,
                        let b = n - a,
                        isPrime b]
--

qcGoldbach :: Integer -> Bool
qcGoldbach n = let (a,b) = goldbach n'
               in and [isPrime a, isPrime b, a + b == n']
                where
                    n' = makeEven $ 3 + n `mod` 10000000000
                    makeEven n
                            | even n = n
                            | otherwise = n + 1
--


-- Problem 41
-- goldbachList
goldbachList = goldbachList' 2
--

-- goldbachList'
goldbachList' min n m = mapM_ (putStrLn . showAsSum)
                        . filter ((> min) . fst)
                        . map goldbach
                        . filter even $ [n..m]
            where
                showAsSum (a,b) = show (a + b)
                                  ++ " = "
                                  ++ show a
                                  ++ " + "
                                  ++ show b
--

tryGoldbachList :: IO ()
tryGoldbachList = goldbachList 9 20

tryGoldbachList' :: IO ()
tryGoldbachList' = goldbachList' 50 3 2000
tryGoldbachList2' = goldbachList' 100 123456 1234567
tryGoldbachList3' = goldbachList' 250 123456 1234567


-- Looking how much smaller the first
-- summand in goldbachs conjecture is
gb = map (\n -> let (a,b) = goldbach n in (n, a, b))
        . filter (\n -> even n && n > 2)
-- turns [2,3,4,5,6] into [(4,2,2),(6,3,3)]
--

drawGb xs = drawNAB <<< gb xs
    where
        drawNAB (n, a, _) = 
                        let rat = fromIntegral a / fromIntegral n
                        in (rat, draw . round $ rat * 15)
                        -- in draw . round $ rat * 60
--
tryDrawGb = drawGb [2..50]
