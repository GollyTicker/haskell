
module NNproblemszzFifth (
                            huffman
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

-- ==========================================================
rtests :: IO Counts
rtests = runTestTT $ TestList unitsProblemTests

unitsProblemTests :: [Test]
unitsProblemTests = [
                       "testPredicates" ~:  testPredicates
                      ,"testGray" ~:  testGray
                      ,"testHuffman1" ~:  testHuffman1
                      ,"testHuffman2" ~:  testHuffman2
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

dot,(.:) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
dot = (.) (.) (.)   -- http://www.haskell.org/haskellwiki/Pointfree#Dot
(.:) = dot
infixr 9 `dot`
infixr 9 .:


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
mapForTest = TestList . map (\(was,exp) -> was ~?= exp)

mapForTestWithTasecase :: (Show a, Eq a, Show b, Eq b) => [(b,a,a)] -> Test
mapForTestWithTasecase = TestList . map (\(b,was,exp) -> (b,was) ~?= (b,exp))

mapTestVal :: (Show a, Eq a, Show b, Eq b) => [(b,a,a)] -> Test
mapTestVal = TestList . map (\(val, was,exp) -> (val,was) ~=? (val, exp))
-- ==========================================================

-- Problem 46

-- Define and, or, nand, nor, xor, impl, eq

_and, _or, nand, nor, xor, impl, eq :: Bool -> Bool -> Bool

nand True True = False
nand _ _ = True

_or True _ = True 
_or _ True = True 
_or _ _ = False

_and = not .: nand
nor = not .: _or
xor = not .: eq
impl = _or . not

eq True True = True
eq False False = True
eq _ _ = False

tf = [True, False]

permuteBools f = map (\(a,b) -> [a, b, f a b]) permuteOverTwo

permuteOverTwo :: [(,) Bool Bool]
permuteOverTwo = [(a,b) | a <- tf, b <- tf]
                -- make it unessesary harder for others:
                -- map (,) tf <*> tf
                -- (,) <$> tf <*> tf

testPredicates = mapForTest
                    [ ((str, t,myPred a b), (str, t,modelPred a b)) |
                        t@(a,b) <- permuteOverTwo,
                        (str, myPred, modelPred) <- preds
                    ]
--
-- a list of the written predicates and a ghc model implementation
preds = [ ("and",  _and, (&&) )
         ,("or",   _or,  (||) )
         ,("nand", nand, not .: (&&) )  {- holy shit. this compiled -}
         ,("nor",  nor,  not .: (||) )
         ,("xor",  xor,  not .: (==) )
         ,("impl", impl, (||) . not )
         ,("eq",   eq,   (==) )
        ]
--

-- print it as a table
-- table

table :: (Bool -> Bool -> Bool) -> IO ()
table f = permuteBools f >>> id

-- Problem 47
-- make infix operators of the predicates

{-
_and, _or, nand, nor, xor, impl, eq :: Bool -> Bool -> Bool -}
(&), (*|), (~&), (~|), (+|), (#=) :: Bool -> Bool -> Bool
a & b = a `_and` b
a *| b = a `_or` b
a ~& b = a `nand` b
a ~| b = a `nor` b
a +| b = a `xor` b
a #= b = a `eq` b

infix 3 &, `_and`
infix 2 *|, `_or`
infix 4 #=, `eq`


-- Problem 48
-- genralise the table for any number of boolean values.
tableN :: Integral a => a -> ([Bool] -> Bool) -> IO ()
tableN n f = permuteBoolsN n >>> (\ls -> ls ++ [f ls])

permuteBoolsN :: Integral a => a -> [[Bool]]
permuteBoolsN 1 = map return tf
permuteBoolsN n = [ x:xs | x <- tf, xs <- permuteBoolsN (n-1)]


-- Problem 49
-- Gray Codes
{- Examples:
gray 1 == ["0","1"].
gray 2 == ["00","01","11","10"].
gray 3 == ["000","001","011","010","110","111","101","100"].
-}

gray :: Integral a => a -> [String]
gray n
    | n < 0 = error "gray code invalid argument"
    | n == 0 = head grayCodes
    | otherwise = ['0':xs | xs <- prev] ++ ['1':xs | xs <- reverse prev]
            where
                prev = (grayCodes !!) . fromIntegral $ n-1
--

gray' :: Integral a => a -> [String]
gray' 0 = [""]
gray' n = ['0':xs | xs <- prev] ++ ['1':xs | xs <- reverse prev]
            where
                prev = gray' (n - 1)
--

grayCodes = [""]:map gray [1..]

-- both versions seems to be equivalent in their memory/time

testGray = mapForTestWithTasecase
                    [
                      (0, gray 0, [""])
                     ,(1, gray 1, ["0","1"])
                     ,(2, gray 2, ["00","01","11","10"])
                     ,(3, gray 3, ["000","001","011","010","110","111","101","100"])
                     ,(0, gray' 0, gray 0)
                     ,(1, gray' 1, gray 1)
                     ,(2, gray' 2, gray 2)
                     ,(3, gray' 3, gray 3)
                     ,(5, gray' 5, gray 5)
                    ]
--

-- Problem 50
-- Huffman Code

-- Tree for the treee
data Tree a = Empty
            | Node {left::(Tree a), value::a, right::(Tree a)}
            deriving (Eq)
--

depthBy :: Integral n => (n -> n -> n) -> Tree a -> n
depthBy f Empty = 0
depthBy f (Node l _ r) = f (1+depthBy f l) (1+depthBy f r)

instance Show a => Show (Tree a) where
        show Empty = "<>"
        show (Node Empty a Empty) = "<" ++ show a ++ ">"
        show node@(Node l a r) = let depth = '\n':replicate (3*(5 - maxDepth node)) ' ' in
                           depth ++ "        " ++ show l ++ ""
                        ++ depth ++ "Node <" ++ show a ++ ">"
                        ++ depth ++ "        " ++ show r ++ ""
--
fromList :: [a] -> Tree a
fromList xs = fromList' Empty xs
fromList' tree [] = tree
fromList' tree (x:xs) = fromList' (insert x tree) xs

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

minDepth :: Integral n => Tree a -> n
minDepth = depthBy min

maxDepth :: Integral n => Tree a -> n
maxDepth = depthBy max

-- https://de.wikipedia.org/wiki/Huffman-Kodierung

type HuffmanTree a = Tree (Maybe a, Rational)
type HuffmanCodeTree a = Tree (Maybe a, String)
type HuffmanCode a = (a, String)

huffman :: (Eq a, Ord a, Show a, Integral n) => [(a, n)] -> [HuffmanCode a]
huffman ls =  let -- freqs :: [(a, Rational)]
                  freqs = relativeFrequencies ls
                  -- singletonTrees :: [HuffmanTree a]
                  singletonTrees = (\(a,r) -> singleton (Just a,r)) << freqs
                  -- huffmanTree :: HuffmanTree a
                  huffmanTree = huffmanFold singletonTrees
                  -- treeWithCodes :: HuffmanCodeTree a
                  treeWithCodes = addGraycode huffmanTree
                  -- flattend  :: [(a, String)]
                  flattend = flattenSort treeWithCodes
              in flattend
--
relativeFrequencies :: (Integral n) => [(a, n)] -> [(a, Rational)]
relativeFrequencies ls = (\(a,freq) -> (a, fromIntegral freq / len)) << ls
                where
                    len :: Rational
                    len = fromIntegral . sum . map snd $ ls
--

-- by now, I should've made Tree into a Foldable/Traversable
flattenSort :: Ord a => HuffmanCodeTree a -> [HuffmanCode a]
flattenSort = List.sortBy (comparing fst) . flatten

flatten :: HuffmanCodeTree a -> [HuffmanCode a]
flatten Empty = []
flatten (Node l (mybA, code) r) = flatten l ++ current ++ flatten r
            where
                -- current :: [(a, String)]
                current = case mybA of  Nothing -> []
                                        Just a -> [(a, code)]
--


addGraycode :: HuffmanTree a -> HuffmanCodeTree a
addGraycode = go ""

go :: String -> HuffmanTree a -> HuffmanCodeTree a
go _ Empty = Empty   -- only on empty inputs
go code (Node l (mybA, rat) r) = Node l' newVal r'
                where
                    -- newVal :: (Maybe a, String)
                    newVal = (mybA, code)
                    l' = (go (code ++ "0") l)
                    r' = (go (code ++ "1") r)
--

huffmanFold :: Eq a => [HuffmanTree a] -> HuffmanTree a
huffmanFold [] = Empty
huffmanFold [x] = x
huffmanFold xs = huffmanFold . merge . splitLowestFreq $ xs

splitLowestFreq :: Eq a => [HuffmanTree a] -> ([HuffmanTree a], [HuffmanTree a])
splitLowestFreq xs = let picked = take 2 $ List.sortBy (comparing relFreq) xs
                         unpicked = xs List.\\ picked
                     in (picked, unpicked)
--
merge :: ([HuffmanTree a], [HuffmanTree a]) -> [HuffmanTree a]
merge ([a,b],c) = merged : c
        where merged = Node a (Nothing, relFreq a + relFreq b) b
merge _ = error "blubb"

relFreq :: HuffmanTree a -> Rational
relFreq (Node _ (_, r) _) = r
relFreq _ = error "Huffmantreeee!!\""

huffmanCase = [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
huffmanExp = [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
testHuffman1 = 
            huffmanExp
            ~=?
            huffman huffmanCase
testHuffman2 = [] ~=? huffman ([]::([] (Char, Int)))
--


