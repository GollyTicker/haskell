{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, FlexibleInstances #-}
import Search

import qualified Data.Set as S
import Data.List
import Data.Ord
import Data.Maybe
import Text.Printf
import Debug.Trace
import Test.QuickCheck
import System.Random

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Control.Monad.Identity
import Control.Applicative

{-
This module uses the Applicative Instance of (->) r
Dont freak out.

Here is what it does.

Instead of writing

(a', b', c', d') = (fa x, fb x, fc x, fd x)

to get the different new values for a to d and
where fa to fd are similar functions,
one can write

(a', b', c', d') = (,,,) <$> fa <*> fb <*> fc <*> fd $ x

The applicative part of the expression (everything except $ x)
means as much as:
"Apply the argument on each of the functions and collect the results
using the provided function at the left."

# See: http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors
-}

linebreak :: IO ()
linebreak = putStrLn "----------------"

main :: IO ()
main = do
    linebreak
    
    putStrLn "Initial world:"
    showTop initialWorld
    linebreak
    
    putStrLn "Are there errors in the initial world?"
    print $ validWorld initialWorld
    linebreak
    
    let xs = pick initialWorld
    printf "Showing first of %d possibilities to pick up\n" (length xs)
    let x = head xs
    showTop x
    linebreak
    
    let ys = put x
    printf "Showing first of %d possibilities to put down\n" (length ys)
    showTop (head ys)
    linebreak
    
    putStrLn "Running Tests"
    void runTests

showTop :: World -> IO ()
showTop = putStrLn . topview

type P = SPath
type Size = Int
type Pos = (Int,Int,Int)
type Legos = S.Set (Posed Lego)
type M = Identity
type Box = (Pos, Pos) -- a box anywhere
newtype Vector = Vector Pos -- a Vector/Box from the origin
-- a small typeclass to refactor a few functions with similar usages
class Vectorspace a where
    move        :: Vector -> a -> a
    moveUp      :: a -> a
    moveUp = move (Vector (0,0,1))
    moveDown    :: a -> a
    moveDown = move (Vector (0,0,-1))
    -- should satisfy 
    --     move (Vector (0,0,0)) = id
instance Vectorspace Pos where move (Vector (dx,dy,dz)) (x,y,z) = (x + dx, y + dy, z + dz)
instance Vectorspace Box where move v (p,p') = (move v p, move v p')
    
data World = 
    World {
         holding    :: Maybe Lego -- lego currently in hand
        ,worldSize  :: Size     -- the world is a cube with Size length,widht and height
        ,legos      :: S.Set (Posed Lego)
    } deriving (Ord, Eq)
instance Show World where show = topview

data Lego = 
    L6x2x2   | L2x6x2 
    | L4x2x2 | L2x4x2
    | L2x2x2
    deriving (Show, Eq, Ord, Read, Enum, Bounded)

instance Random Lego where
    randomR (a,b) g =
        let domain = [a .. b] :: [Lego]
            len = length domain
            (i,g') = next g
            i' = i `mod` len
        in  (domain !! i', g')
    random = randomR (minBound,maxBound)

instance Arbitrary Lego where
    arbitrary = choose (minBound, maxBound)

legoBox :: Lego -> Vector
legoBox L2x6x2 = minusOne $ Vector (2,6,2) -- converting to zero based
legoBox L6x2x2 = minusOne $ Vector (6,2,2)
legoBox L2x2x2 = minusOne $ Vector (2,2,2)
legoBox L4x2x2 = minusOne $ Vector (4,2,2)
legoBox L2x4x2 = minusOne $ Vector (2,4,2)

minusOne :: Vector -> Vector
minusOne (Vector (x,y,z)) = Vector (x-1,y-1,z-1)

data Posed a = PS { getLego :: a, getOrigin :: Pos, getEdge :: Pos} deriving Show
-- Pos is the left-bottom Coordinate of the Lego Block
-- a size attribute saving the worlds size could be used to rotate blocks
-- in the higher and middle part to the upper left corner for meaningful
-- interpretations for findMaximum/minimum and lookupGT/LT in the Set

getBox :: Posed Lego -> Box
getBox (PS _ a b) = (a,b)

count :: World -> Int
count (World ml _ set) = maybe 0 (const 1) ml + S.size set

instance Ord (Posed a) where compare = comparing getOrigin
-- no need to compare lego, because that would mean that the two Lego are overlapping

instance Eq (Posed a) where x == y = (EQ==)$ comparing getOrigin x y

legoProblem :: Problem P M World
legoProblem =
    mkProblem {
         starts      = [ initialWorld ]
        ,checkGoal   = Identity . allOnFloor
        ,showElem    = topview -- show
        ,eqElem      = (==)

        ,actions     = [
               mkAction "Pick" (Identity . pick)
              ,mkAction "Put"  (Identity . put)
         ]
        
        ,heuristic   = Nothing
        ,strategy    = Breadth
        ,ordering    = Just compare
    }

allOnFloor :: World -> Bool
allOnFloor = all onFloor . S.toList . legos

onFloor :: Posed Lego -> Bool
onFloor = uncurry3 (\x y z -> z == 0) . getOrigin

-- Ich wünschte man könnte pick und put als reverse Methoden
-- voneinander definieren...

pick :: World -> [World]
pick (World Nothing size s) = [ World (Just l) size s' | (l,s') <- pickable s ]
pick _ = []

put :: World -> [World]
put (World (Just l) size s) =
    [ w | s' <- putable size l s, let w = World Nothing size s', isValidWorld w]
put _ = []

-- for debug
validate :: World -> World
validate w | validWorld w == Nothing     = w
           | otherwise = error "invalidWorld!"

-- find all places one can put this lego at
putable :: Size -> Lego -> Legos -> [Legos]
putable size l s = [ S.insert l' s | l' <- newPositions ]
    where
        newPositions = filter eligible $ mkLego <$> range <*> range <*> range  
        range = [0..(size-1)]
        eligible = ((&&) .: (&&)) <$> collisionFree s <*> isLoose s <*> inWorld size
        mkLego :: Int -> Int -> Int -> Posed Lego
        mkLego = mkPositionedLego l



inWorld :: Size -> Posed Lego -> Bool
inWorld size = and6 <$> inrange . getX . fst . getBox
                    <*> inrange . getY . fst . getBox
                    <*> inrange . getZ . fst . getBox
                    <*> inrange . getX . snd . getBox
                    <*> inrange . getY . snd . getBox
                    <*> inrange . getZ . snd . getBox
    where
        and6 a b c d e f = a && b && c && d && e && f
        inrange = (`elem` range)
        range = [0..(size-1)]

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f = \(a,b,c) -> f a b c

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f = \a b c -> f(a,b,c)

-- applicative chaining of logical operations is quite useful!
isLoose :: Legos -> Posed Lego -> Bool
isLoose s = (||) <$> looseOnFloor <*> looseOnLego
    where
        looseOnLego = (&&) <$> not . onFloor <*> (xor <$> topFree s <*> botFree s)
        looseOnFloor = (&&) <$> onFloor <*> topFree s

xor a = (||) <$> ((a &&) . not) <*> (not a &&)

-- find all legos one can pick
pickable :: Legos -> [(Lego, Legos)]
pickable s = [ (lego,s') | p@(PS lego _ _) <- picks, let s' = S.delete p s ]
    where
        picks = S.toList . S.filter (isLoose s) $ s

-- is the box free in the set of legos?
isFree :: Legos -> Box -> Bool
isFree set b = S.null $ intersectsBox b set

-- which legos intersect with with this box?
intersectsBox :: Box -> Legos -> Legos
intersectsBox b = S.filter (intersects b . getBox)

-- are two boxes intersecting?
intersects :: Box -> Box -> Bool
intersects a b =
    let ( (ax,ay,az), (ax',ay',az') ) = a
        ( (bx,by,bz), (bx',by',bz') ) = b
    in  rint bx bx' ax ax'
        && rint by by' ay ay'
        && rint bz bz' az az'

-- Range INTersect
rint :: Int -> Int -> Int -> Int -> Bool
rint x x' y y' = x <= y' && y <= x'

-- what is the top plane of the box?
boxTop :: Box -> Box
boxTop ((x,y,_),(x',y',z')) = ((x,y,z'), (x',y',z'))

-- what is the bottom plane of the box?
boxBot :: Box -> Box
boxBot ((x,y,z),(x',y',_)) = ((x,y,z), (x',y',z))

isValidWorld :: World -> Bool
isValidWorld = not . isJust . validWorld

-- check all legos for intersects and ensure that no lego is flying
-- returns a Just of errors if invalid
validWorld :: World -> Maybe ( Legos, [(Posed Lego,Posed Lego)] )
validWorld w | S.null flys && null intersects = Nothing
             | otherwise = Just (flys, intersects)
    where
        intersects = intersections ls
        ls = legos w
        flys = flyings ls

-- which legos are flying?
-- those, who have nothing underneath or above them
flyings :: Legos -> Legos
flyings s = S.filter flying s
    where flying =
            (&&) <$> not . onFloor <*> ( (&&) <$> topFree s <*> botFree s )

topFree, botFree, boxFree :: Legos -> Posed Lego -> Bool
topFree set = isFree set . moveUp   . boxTop . getBox
botFree set = isFree set . moveDown . boxBot . getBox
boxFree set = isFree set . getBox -- is the box of this lego free in the given set?

collisionFree :: Legos -> Posed Lego -> Bool
collisionFree s l = S.null $ intersectsBox (getBox l) s

-- which legos are intersecting?
intersections :: Legos -> [(Posed Lego,Posed Lego)]
intersections = catMaybes . forPairs f . (S.toList)
    where
        f a b | intersects (getBox a) (getBox b) = Just (a,b)
              | otherwise = Nothing

forPairs :: Eq a => (a -> a -> c) -> [a] -> [c]
forPairs f as = [ f a a' | a <- as, a' <- as, a /= a' ]

initialWorld :: World
initialWorld = 
    let size = 10
    in  World Nothing size
        $ S.fromList
        $ zipWith4 mkPositionedLego
            [ L2x2x2 ,L2x2x2 ,L2x6x2 ,L6x2x2 ,L2x2x2 ,L2x4x2 ,L4x2x2]
            [ 0      ,2      ,0      ,1      ,3      ,6      ,2     ]
            [ 0      ,0      ,0      ,4      ,3      ,3      ,5     ]
            [ 0      ,0      ,2      ,4      ,2      ,0      ,6     ]

-- How the world looks like
-- Topview. The number says the height
{-
    0    x   9
  0 3311000000
    3311000000
    3300000000
    3300001100
  y 3555555100
    3577775100
    0077771100
    0000000000
    0000000000
  9 0000000000
-}

topview :: World -> String
topview (World h size s) = "Holding " ++ show h ++ "\n" ++ topview' size s

topview' :: Int -> Legos -> String
topview' size s = toString heights
    where
        range = [0..(size-1)]
        heights :: [[Int]]
        heights = [ [ heightAt x y | x <- range] | y <- range]
        heightAt :: Int -> Int -> Int
        heightAt x y = 
            let rod = ((x,y,0),(x,y,size))
                onRod = intersectsBox rod s
            in  if S.null onRod
                    then 0
                    else S.findMax . S.map (getZ . snd . getBox) $ onRod

toString :: Show a => [[a]] -> String
toString = intercalate "\n" . map (intercalate "". map show)

getX :: (a,b,c) -> a
getX (a,b,c) = a
getY :: (a,b,c) -> b
getY (a,b,c) = b
getZ :: (a,b,c) -> c
getZ (a,b,c) = c

mkPositionedLego :: Lego -> Int -> Int -> Int -> Posed Lego
mkPositionedLego l x y z = 
    let origin = (x,y,z)
    in  PS l origin (move (legoBox l) origin)

-- alternatively model the world as a tree of legos
-- where every lego points to its immediate lower ones.
-- this makes finding free legos much easier


simpleWorld = 
    let size = 6
    in  World (Just L2x2x2) size
        $ S.fromList
        $ zipWith4 mkPositionedLego
            [ L2x2x2, L2x2x2, L2x2x2 ]
            [ 0     , 0     , 0      ]
            [ 0     , 1     , 2      ]
            [ 0     , 2     , 4      ]

main2 = do
    showTop simpleWorld >> print simpleWorld
    linebreak >> linebreak
    let xs = put simpleWorld
    mapM_ (\x -> showTop x >> print x >> linebreak) xs


showSample :: IO ()
showSample = sample' (arbitrary :: Gen World) >>= mapM_ showTop

instance Arbitrary World where
    arbitrary = do
            size           <- choose (2,8)
            (World _ _ s) <- repeatAWhile 5 (World Nothing size S.empty)
            ml             <- arbitrary :: Gen (Maybe Lego)
            return (World ml size s)
        where
            repeatAWhile n (World _ size ls) = do
                    i <- arbitrary :: Gen Int
                    l      <- arbitrary :: Gen Lego
                    let w = (World (Just l) size ls)
                        xs = put w
                    let stop = minBound + (round $ (0.5 :: Double) * fromIntegral (maxBound - minBound :: Int) )
                    if n == 0 || i < stop || null xs then return w else elements xs >>= repeatAWhile (n-1)

_expand w = put w ++ pick w

-- the the number of legos doesn't change on expansions
prop_PutPickCount = \w -> and $ map ((==EQ) . comparing count w) (_expand w)

-- all expansions should be valid Worlds
prop_ValidGen = all isValidWorld . _expand

-- In a World with exactly one Lego, that Lego is always pickable.
prop_SinglePickable = \w -> ((1==) . S.size . legos) w ==> isLoose (legos w) (S.elemAt 0 $ legos w)

-- After Puting a Lego, at least one Lego is Loose.
prop_LooseAfterPut =
    \w -> let xs = put w
          in  all (\w' -> let s = legos w' in not . S.null . S.filter (isLoose s) $ s) xs

-- consecutive put/pick actions may reverse each other.
prop_PutPickMayReverse = \w ->
    let as = _expand w
        xs = concatMap _expand as
    in  not (null xs) ==> w `elem` xs

-- how does the number of loose legos change on put/pick?
-- prop_PutPickLoose = \w -> and $ map ((==EQ) . comparing (S.size . legos) w) (_expand w)

runTests = $quickCheckAll
rt = runTests


