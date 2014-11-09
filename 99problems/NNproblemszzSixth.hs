
module NNproblemszzSixth (
                            
                          )
                      where

import Test.QuickCheck
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Ord -- comparing
import Data.Ratio
import Debug.Trace
import Test.HUnit
import Control.Applicative
import Control.Monad
import System.Random
import qualified Data.Foldable as F

-- ==========================================================
rtests :: IO Counts
rtests = runTestTT $ TestList unitsProblemTests

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

dot,(.:) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
dot = (.) (.) (.)   -- http://www.haskell.org/haskellwiki/Pointfree#Dot
(.:) = dot
infixr 9 `dot`
infixr 9 .:


main :: IO ()
main = do
        -- putStrLn "Quickchecks" -- cant do qucikChecks because of how hard its to make an arbitrary Tree
        putStrLn ">> Running HUnit tests..."
        counts <- rtests
        putStrLn ">> Finished HUnit Tests."
        if failures counts /= 0
            then putStrLn ">> Tests failed."
            else putStrLn ">> All ok."
--
mapForTest :: (Show a, Eq a) => [(a,a)] -> Test
mapForTest = TestList . map (\(was,exp) -> was ~?= exp)

mapForTestWithTasecase :: (Show a, Eq a, Show b, Eq b) => [(b,a,a)] -> Test
mapForTestWithTasecase = TestList . map (\(b,was,exp) -> (b,was) ~?= (b,exp))

mapTestVal :: (Show a, Eq a, Show b, Eq b) => [(b,a,a)] -> Test
mapTestVal = TestList . map (\(val, was,exp) -> (val,was) ~=? (val, exp))
-- ==========================================================


unitsProblemTests :: [Test]
unitsProblemTests = [
                       "testInsertions" ~:  testInsertions
                     , "testCbal" ~:  testCbal
                     , "testCbalModel" ~:  testCbalModel
                     , "testSymmetric" ~:  testSymmetric
                    ]

-- Problem 54A
-- Binary Trees

-- Tree for the treee
data Tree a = Empty
            | Node {
                left  :: (Tree a),
                value ::  a ,
                right :: (Tree a)
              }
            deriving (Eq, Ord)
--
instance Functor Tree where
        fmap _ Empty = Empty
        fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)
;

instance F.Foldable Tree where
        foldl f acc Empty = acc
        foldl f acc (Node l val r) = (F.foldl f (f (F.foldl f acc l) val) r) 
--

-- Pretty printing binary trees:
-- http://stackoverflow.com/questions/19082560/haskell-pretty-print-binary-tree-not-displaying-properly

instance Show a => Show (Tree a) where
        show = unlines . layoutTree
            where
                layoutTree :: Show a => Tree a -> [String]
                layoutTree Empty = ["<>"]
                layoutTree (Node l a r) = indent (layoutTree r) ++ ["<" ++ show a ++ ">"] ++ indent (layoutTree l)
                indent = map ("    " ++)
--

toList :: Tree a -> [a]
toList = reverse . F.foldl (flip (:)) []

fromList :: [a] -> Tree a
fromList xs = fromList' Empty xs
        where
            fromList' tree [] = tree
            fromList' tree (x:xs) = fromList' (insert x tree) xs
--

-- insert at the left-most closest point.
-- always prodces cbla trees.
insert :: a -> Tree a -> Tree a
insert a Empty = Node Empty a Empty
insert a (Node Empty val r) = Node (singleton a) val r
insert a (Node l val Empty) = Node l val (singleton a)
insert a (Node l val r)
            | minDepth l <= minDepth r = Node (insert a l) val r
            | otherwise = Node l val (insert a r)
--
singleton :: a -> Tree a
singleton a = Node Empty a Empty

depthBy :: Integral n => (n -> n -> n) -> Tree a -> n
depthBy f Empty = 0
depthBy f (Node l _ r) = f (1+depthBy f l) (1+depthBy f r)

minDepth :: Integral n => Tree a -> n
minDepth = depthBy min

height :: Integral n => Tree a -> n
height = depthBy max

tr = fromList [1,5,1,5,7,2,1,33,2,31,22,1,32,1,3,2,24,35,63,64,6,5,4,6,46]

-- Problem 55
-- cbal. completely balanced trees with n nodes
cbal :: Integral a => a -> [Tree Int]
cbal 0 = [Empty]
cbal n
    | n < 0 = error "negative integral"
    | otherwise = List.nub $
            do 
                x <- cbal (n-1)
                x' <- filter balanced . insertions $ x
                return x'
;
insertions :: Tree Int -> [Tree Int]
insertions Empty = [sx]
insertions (Node l val r) = 
                        do 
                            inL <- insertions l
                            return $ Node inL val r
                        ++
                        do 
                            inR <- insertions r
                            return $ Node l val inR
;

-- a tree is balanced, if both #of nodes in both subtrees differ not more than one.
balance :: Integral b => Tree a -> b
balance Empty = 0
balance (Node l _ r) = numOfNodes r - numOfNodes l

numOfNodes :: Integral n => Tree a -> n
numOfNodes Empty = 0
numOfNodes (Node l _ r) = 1 + numOfNodes l + numOfNodes r

balanced :: Tree a -> Bool
balanced Empty = True
balanced tr@(Node l _ r) = balanced l && balanced r && 1 >= (abs $ balance tr)


-- TODO: better solution that uses the
-- fact, that a connected graph with #Edges = #Vertices - 1 is a tree

cbal' :: Integral a => a -> [Tree Int]
cbal' 0 = [Empty]
cbal' n
    | n < 0 = error "negative integral"
    | otherwise = undefined
;

-- model implementation from the solutions
cbalTree :: Int -> [Tree Int]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2
    in [Node left x right | i     <- [q .. q + r],
                                left  <- cbalTree i,
                                right <- cbalTree (n - i - 1)]

x = 0

testInsertions = mapForTest [
                            (insertions Empty, [sx]),
                            (insertions sx, [Node sx x Empty, Node Empty x sx]),
                            (insertions (threeNodes Empty Empty Empty Empty), bal4)
                        ]
;

qcCBal :: Int -> Property
qcCBal n = 0 <= n && n < 100 ==> Set.fromList (cbal n) == Set.fromList (cbalTree n)

sx = singleton x

testCbal = mapForTest [
                        (cbal 0, [Empty]),
                        (cbal 1, [sx]),
                        (cbal 2, [Node sx x Empty, Node Empty x sx]),
                        (cbal 3, [threeNodes Empty Empty Empty Empty]),
                        (cbal 4, bal4)
                      ]
;

testCbalModel = mapForTest [
                        (Set.fromList $ cbal 0, Set.fromList $ cbalTree 0),
                        (Set.fromList $ cbal 1, Set.fromList $ cbalTree 1),
                        (Set.fromList $ cbal 2, Set.fromList $ cbalTree 2),
                        (Set.fromList $ cbal 3, Set.fromList $ cbalTree 3),
                        (Set.fromList $ cbal 4, Set.fromList $ cbalTree 4),
                        (Set.fromList $ cbal 5, Set.fromList $ cbalTree 5),
                        (Set.fromList $ cbal 6, Set.fromList $ cbalTree 6)
                      ]
;
bal4 = [
        threeNodes sx Empty Empty Empty,
        threeNodes Empty sx Empty Empty,
        threeNodes Empty Empty sx Empty,
        threeNodes Empty Empty Empty sx
      ]
;
threeNodes :: Tree Int -> Tree Int -> Tree Int -> Tree Int -> Tree Int
threeNodes ll lr rl rr = Node (Node ll x lr) x (Node rl x rr)


{-
Problem 56 - symmetric
Check if two trees are symmetric. Thas means, that the left and
right subtree of the root node are mirrors of each other.
The predicate ignores the actual contents.
-}

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Node l _ r) = l `mirrors` r

mirrors Empty Empty = True
mirrors (Node fl _ fr) (Node sl _ sr) = fl `mirrors` sr && fr `mirrors` sl
mirrors _ _ = False

testSymmetric = mapForTest [
                        (symmetric Empty, True),
                        (symmetric $ Node Empty x sx, False),
                        (symmetric $ Node sx x sx, True),
                        (symmetric $ Node sx x (Node Empty x sx), False),
                        (symmetric $ Node (Node sx x Empty) x (Node Empty x sx), True),
                        (symmetric $ Node (Node sx x Empty) x (Node Empty x Empty), False),
                        (symmetric $ Node (Node sx x Empty) x (Node sx x Empty), False),
                        (symmetric $ Node (Node sx x sx) x (Node sx x sx), True),
                        (symmetric $ Node (Node (Node Empty x sx) x sx) x (Node sx x (Node sx x Empty)), True),
                        (symmetric $ Node (Node (Node Empty x sx) x sx) x (Node sx x (Node Empty x sx)), False)
                      ]
;
