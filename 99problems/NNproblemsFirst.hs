
module NNproblemsFirst (
                          encode
                          )
                      where

import Test.QuickCheck
import qualified Data.Set as Set
import qualified Data.List as List
import Debug.Trace

applyJoin :: (a -> b) -> (b -> b -> c) -> a -> a -> c
applyJoin f g a b = g (f a) (f b)

runQCtests :: IO ()
runQCtests = qcbools >> qcprops --, sequence qcprops]

qcbools = mapM_ quickCheck [testP4]
qcprops = mapM_ quickCheck [testP1]


main :: IO ()
main = do
        putStrLn ">> Running quickCheck tests..."
        runQCtests
        putStrLn ">> Finished quickCheck Tests."

-- Problem 1
myLast :: [a] -> a
myLast xs
    | null xs = error "Not with an empty list!"
    | null bs = b
    | otherwise = myLast bs
    where
        (b:bs) = xs

testP1 :: Eq a => [a] -> Property
testP1 ls = (not $ null ls) ==> (last ls == myLast ls)


-- Problem 2
myButLast :: [a] -> a
myButLast xs
    | null xs = error "No forelast element!"
    | null rest = error "No forelast element!"
    | null r_rest = a
    | otherwise = myButLast rest
    where
        (b:rest) = xs
        (a:_:r_rest) = xs

testP2 :: (Eq a) => [a] -> Property
testP2 ls = (length ls >= 2) ==> (ls !! ( (length ls) - 1) == myButLast ls)

-- Problem 3
elemAt :: [a] -> Int -> a
elemAt xs n
    | null xs = error "No element in empty list!"
    | (n == 1) = first
    | otherwise = elemAt rest (n-1)
    where
        (first:rest) = xs

testP3 :: (Eq a) => [a] -> Int -> Property
testP3 ls n = (not $ null ls) ==>
                ((ls !! idx) == elemAt ls (idx + 1))
            where
                idx = n `mod` (length ls)


-- Problem 4
numOfElems :: [a] -> Int
numOfElems xs = len' xs 0
    where len' ls accu
            | null ls = accu
            | otherwise = len' r (accu+1)
            where
                (_:r) = ls

testP4 :: [a] -> Bool
testP4 ls = length ls == numOfElems ls

-- Problem 5
rvs :: [a] -> [a]
rvs xs = rvs' xs []
    where
        rvs' xs accu
            | null xs = accu
            | otherwise = rvs' r (f:accu)
            where
                (f:r) = xs

-- alternatively
rvs' :: [a] -> [a]
rvs' = foldl (flip (:)) []

testP5 :: Eq a => [a] -> Bool
testP5 ls = reverse ls == rvs ls

testP5' :: Eq a => [a] -> Bool
testP5' ls = rvs' ls == rvs ls

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = isSame xs (rvs xs) True
    where
        isSame xs rs accu
            | (null xs) && (null rs) = accu
            | otherwise = isSame tlxs tlrs (x == r && accu)
            where
                (x:tlxs) = xs
                (r:tlrs) = rs
--
testP6 :: Eq a => [a] -> Bool
testP6 str = isPalindrome  $ str ++ (reverse str)

testP6_2 :: String -> Int -> Property
testP6_2 str n = isLongEnough ==> False == testNegativeIsPalindrome str n
                where
                    isLongEnough = length str > 2

testNegativeIsPalindrome :: String -> Int -> Bool
testNegativeIsPalindrome str n = isPalindrome (badStr ++ (reverse str))
                where
                    badStr = replaceNth (changableIdx) str
                    changableIdx = length str - 2

replaceNth :: Int -> String -> String
replaceNth n xs = let (ys, (z:zs)) = splitAt n xs in ys ++ ((succ z):zs)


-- Problem 7
-- [defining new data type because List
-- in haskell have a homogeneous type]
data NLs a = Raw a | Ls [NLs a] deriving (Show, Eq)

-- (kinda bad data structure.....)

nls :: [NLs Integer]
nls = [
        Ls [],
        Raw 1,
        Ls [Raw 2, Raw 3],
        Ls [Raw 4, Raw 5, Raw 6, Ls [Raw 7, Ls [Raw 8], Raw 9], Raw 10]
    ]

flatten :: NLs a -> [a]
flatten nls = case nls of (Raw x) -> [x]
                          (Ls xs) -> (myConcatMap flatten xs)

-- rewriting concatMap for practise
myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = foldl (\accu x -> accu ++ (f x)) []

--prop_transparency nls = flatten nls 

-- ghci> mapM_ ((mapM_ print) . (\x -> [show x,show $ flatten x])) nls


-- Problem 8
-- remove duplicates. preserves order.
compress :: Eq a => [a] -> [a]
compress ls = fst $ foldl (\(accu, cont) x -> if x `elem` cont then (accu,cont) else (accu ++ [x], x:cont)) ([],[]) ls

testP8 ls = compress ls == List.nub ls


-- Problem 9
-- pack together cosecutive apprearences of the same element into a sublist
pack :: Eq a => [a] -> [[a]]
pack ls
    | null ls = []
    | otherwise = snd $ foldl (\(currSublElem, accu) x -> packSingle currSublElem accu x) (head ls, [[]]) ls

packSingle :: Eq a => a -> [[a]] -> a -> (a, [[a]])
packSingle currSublElem accu x
                    | currSublElem == x = (x, addSameElem accu)
                    | otherwise = (x, accu ++ [[x]])
                where
                    addSameElem accu = init accu ++ [ x : last accu ]

testP9 :: (Ord a, Eq a) => [a] -> Bool
testP9 ls = applyJoin (length . pack) (<=) ls (List.sort ls)
-- weirdly enough. applyJoin cannot be used in infix notation here....

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = (map (\xs -> (length xs, head xs))) . pack
