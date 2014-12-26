
import Search

import qualified Data.Set as S
import Data.List
import Data.Ord

type P = SPath
type Size = Int
type Pos = (Int,Int,Int)
type Legos = S.Set (Posed Lego)
data World = 
    World {
         holding    :: Maybe (Posed Lego) -- lego currently in hand
        ,worldSize  :: Size     -- the world is a cube with Size length,widht and height
        ,legos      :: S.Set (Posed Lego)
    } deriving (Ord, Eq, Show)

data Lego = L2x6x2 | L6x2x2 | L2x2x2 deriving (Show, Eq, Ord, Read, Bounded)

legoBox :: Lego -> Vector
legoBox L2x6x2 = minusOne $ Vector (2,6,2) -- converting to zero based
legoBox L6x2x2 = minusOne $ Vector (6,2,2)
legoBox L2x2x2 = minusOne $ Vector (2,2,2)

minusOne :: Vector -> Vector
minusOne (Vector (x,y,z)) = Vector (x-1,y-1,z-1)

data Posed a = PS { __size :: Size, getLego :: Lego, getOrigin :: Pos, getEdge :: Pos} deriving Show
-- Pos is the left-bottom Coordinate of the Lego Block
-- Size is the size of the world.  it could be used to rotate blocks
-- in the higher and middle part to the upper left corner for comparision

getPBox :: Posed Lego -> PBox
getPBox (PS _ _ a b) = (a,b)

instance Ord (Posed a) where compare = comparing getOrigin
-- no need to compare lego, because that would mean that the two Lego are overlapping

instance Eq (Posed a) where x == y = (EQ==)$ comparing getOrigin x y

legoProblem :: Problem P World
legoProblem =
    mkProblem {
         starts      = [ beginning ]
        ,checkGoal   = allOnFloor
        ,showElem    = show
        ,eqElem      = (==)

        ,actions     = [
              mkAction "DoStuff" doStuff
         ]
        
        ,heuristic   = Nothing
        ,strategy    = Breadth
        ,ordering    = Just compare
    }

allOnFloor :: World -> Bool
allOnFloor = all (\(x,y,z) -> z == 0) . map getOrigin . S.toList . legos

doStuff :: World -> [World]
doStuff (World Nothing s ls) = [ World (Just l) s ls' | (l,ls') <- pickable ls ]
    where
        pickable :: Legos -> [(Posed Lego, Legos)]
        pickable set = [ (p,set') | p <- picks, let set' = S.delete p set ]
            where
                picks = S.toList $ S.filter nothingAbove set
                nothingAbove posl = surfaceFree set $ boxTop $ getPBox posl

doStuff (World (Just l) s ls)  = undefined
    -- find all places one can put this lego at

type PBox = (Pos, Pos) -- a box anywhere

newtype Vector = Vector Pos -- a Vector/Box from the origin

surfaceFree :: Legos -> PBox -> Bool
surfaceFree set b = S.null $ intersectsL set b

intersectsL :: Legos -> PBox -> Legos
intersectsL set x = S.filter (intersects x . getPBox) set

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

boxTop :: PBox -> PBox
boxTop ((x,y,_),(x',y',z')) = ((x,y,z'), (x,y,z'))

--
isValidworld :: World -> IO ()
isValidworld = undefined
-- check all legos for intersects and check that no lego is flying

beginning :: World
beginning = 
    let size = 10
    in  World Nothing size
        $ S.fromList
        $ zipWith4 (mkPositionedLego size)
            [ L2x2x2 ,L2x2x2 ,L2x6x2 ,L6x2x2 ]
            [ 0      ,2      ,0      ,3      ]
            [ 0      ,0      ,0      ,3      ]
            [ 0      ,0      ,2      ,2      ]

mkPositionedLego :: Int -> Lego -> Int -> Int -> Int -> Posed Lego
mkPositionedLego size l x y z = 
    let origin = (x,y,z)
    in  PS size l origin (move (legoBox l) origin)

move :: Vector -> Pos -> Pos
move (Vector (dx,dy,dz)) (x,y,z) = (x + dx, y + dy, z + dz)


-- alternatively model the world as a tree of legos
-- where every lego points to its immediate lower ones.
-- this makes finding free legos much easier