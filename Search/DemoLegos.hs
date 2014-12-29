{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Search

import qualified Data.Set as S
import Data.List
import Data.Ord
import Data.Maybe
import Text.Printf


main = do
    putStrLn "Are there errors in the initial world?"
    print $ validWorld initialWorld
    putStrLn "Initial world:"
    putStrLn $ topview initialWorld

type P = SPath
type Size = Int
type Pos = (Int,Int,Int)
type Legos = S.Set (Posed Lego)

data World = 
    World {
         holding    :: Maybe Lego -- lego currently in hand
        ,worldSize  :: Size     -- the world is a cube with Size length,widht and height
        ,legos      :: S.Set (Posed Lego)
    } deriving (Ord, Eq, Show)

data Lego = 
    L6x2x2   | L2x6x2 
    | L4x2x2 | L2x4x2
    | L2x2x2
    deriving (Show, Eq, Ord, Read, Bounded)

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

getPBox :: Posed Lego -> PBox
getPBox (PS _ a b) = (a,b)

instance Ord (Posed a) where compare = comparing getOrigin
-- no need to compare lego, because that would mean that the two Lego are overlapping

instance Eq (Posed a) where x == y = (EQ==)$ comparing getOrigin x y

legoProblem :: Problem P World
legoProblem =
    mkProblem {
         starts      = [ initialWorld ]
        ,checkGoal   = allOnFloor
        ,showElem    = topview -- show
        ,eqElem      = (==)

        ,actions     = [
              mkAction "DoStuff" doStuff
         ]
        
        ,heuristic   = Nothing
        ,strategy    = Breadth
        ,ordering    = Just compare
    }

allOnFloor :: World -> Bool
allOnFloor = all onFloor . S.toList . legos

onFloor :: Posed Lego -> Bool
onFloor = uncurry3 (\x y z -> z == 0) . getOrigin

doStuff :: World -> [World]
doStuff (World Nothing s ls) = [ World (Just l) s ls' | (l,ls') <- pickable ls ]
doStuff (World (Just l) s ls) = [ World Nothing s ls' | ls' <- putable l ls ]

-- find all places one can put this lego at
putable :: Lego -> Legos -> [Legos]
putable l ls = [ S.insert l' ls | l' <- newPositions ]
    where
        surfaces     :: [Pos]
        newPositions :: [Posed Lego]
        surfaces = undefined
        newPositions =
            filter (isFree ls . getPBox)
            . map (uncurry3 (mkPositionedLego l))
            $ surfaces
--        surfaces = undefined
  --      validOrigins = map undefined -- check for collisions, surfaces

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f = \(a,b,c) -> f a b c

-- find all legos one can pick
pickable :: Legos -> [(Lego, Legos)]
pickable set = [ (lego,set') | p@(PS lego _ _) <- picks, let set' = S.delete p set ]
    where
        picks = S.toList $ S.filter nothingAbove set
        nothingAbove posl =
            isFree set
            $ moveUp
            $ boxTop
            $ getPBox posl


type PBox = (Pos, Pos) -- a box anywhere

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

instance Vectorspace Pos where
    move (Vector (dx,dy,dz)) (x,y,z) = (x + dx, y + dy, z + dz)

instance Vectorspace PBox where
    move v (p,p') = (move v p, move v p')

-- is the box free in the set of legos?
isFree :: Legos -> PBox -> Bool
isFree set b = S.null $ intersectsPBox b set

-- which legos intersect with with this box?
intersectsPBox :: PBox -> Legos -> Legos
intersectsPBox b = S.filter (intersects b . getPBox)

-- are two boxes intersecting?
intersects :: PBox -> PBox -> Bool
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
boxTop :: PBox -> PBox
boxTop ((x,y,_),(x',y',z')) = ((x,y,z'), (x',y',z'))

-- what is the bottom plane of the box?
boxBot :: PBox -> PBox
boxBot ((x,y,z),(x',y',_)) = ((x,y,z), (x',y',z))


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

flyings :: Legos -> Legos
flyings ls = S.filter flying ls
    where
        flying lego = let pb = getPBox lego
                          floor = boxBot pb
                          underneath = moveDown floor
                          legosUnderneath = intersectsPBox underneath ls
                      in  not (onFloor lego) && S.null legosUnderneath

-- which legos are intersecting?
intersections :: Legos -> [(Posed Lego,Posed Lego)]
intersections = catMaybes . forPairs f . (S.toList)
    where
        f a b | intersects (getPBox a) (getPBox b) = Just (a,b)
              | otherwise = Nothing

forPairs :: Eq a => (a -> a -> c) -> [a] -> [c]
forPairs f as = [ f a a' | a <- as, a' <- as, a /= a' ]

initialWorld :: World
initialWorld = 
    let size = 10
    in  World Nothing size
        $ S.fromList
        $ zipWith4 mkPositionedLego
            [ L2x2x2 ,L2x2x2 ,L2x6x2 ,L6x2x2 ,L2x4x2 ,L4x2x2]
            [ 0      ,2      ,0      ,1      ,6      ,2     ]
            [ 0      ,0      ,0      ,4      ,3      ,5     ]
            [ 0      ,0      ,2      ,4      ,0      ,6     ]

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
topview (World h s ls) = "Holding " ++ show h ++ "\n" ++ topview' s ls

topview' :: Int -> Legos -> String
topview' s ls = toString heights
    where
        range = [0..(s-1)]
        heights :: [[Int]]
        heights = [ [ heightAt x y | x <- range] | y <- range]
        heightAt :: Int -> Int -> Int
        heightAt x y = 
            let rod = ((x,y,0),(x,y,s))
                onRod = intersectsPBox rod ls
            in  if S.null onRod
                    then 0
                    else S.findMax . S.map (getZ . snd . getPBox) $ onRod

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
