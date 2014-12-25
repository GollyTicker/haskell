{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types (
        module Types
    )
    where

import qualified Data.Set as S
import Data.Monoid
import Data.Maybe

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .: g = \x y -> f $ g x y

data Strategy a =
    Depth
    | Breadth
    | A
--  | OptimistcClimbing
--  | Custom (StrategyF a)

type StrategyF a = [SPath a] -> [SPath a] -> [SPath a]
--                 NewPaths -> OldPaths -> AllPaths

-- Problem a is a description of a problem where the search space
-- consists of elemets of type a.
-- In example, the water bucket problem might use (Int, Int)
-- where the two numbers represent the liters in each bucket.
data Problem a =
    Problem {
         starts         :: [a]
        ,showElem       :: a -> String  -- keep (show) in problem description
        ,checkGoal      :: a -> Bool
        ,eqElem         :: a -> a -> Bool
        ,heuristic      :: Maybe (Heuristic a)
        ,actions        :: [Action a]
        ,strategy       :: Strategy a
        ,ordering       :: Maybe (a -> a -> Ordering) -- if given, allows usage of Sets to optimize cycle detection
        ,noCycleDetection :: Bool
            -- enable, if cycles are impossible in your domain.
            -- reduces time comsumption
        -- add custom strategies here?
    }

-- default Problem for easier use with record Syntax
mkProblem :: Problem a
mkProblem = Problem {
         starts = error "mkProblem{starts = ... Please specify ... }: "
        ,showElem = error "mkProblem{showElem = ... Please specify ... }: "
        ,checkGoal = error "mkProblem{checkGoal = ... Please specify ... }: "
        ,eqElem = error "mkProblem{eqElem = ... Please specify ... }: "
        ,heuristic = Nothing
        ,actions = error "mkProblem{actions = ... Please specify ... }: "
        ,strategy = Breadth
        ,noCycleDetection = False
        ,ordering = Nothing
    }

checkGoalNode :: Problem a -> Node a -> Bool
checkGoalNode pr = (pr `checkGoal`) . getElem

data Node a =
    Node {
         getElem        :: a
        ,getAction      :: AppliedAction a
        ,getHvalue      :: Maybe HValue   -- Heuristics Value
    }


mkStartPath :: Problem a -> a -> SPath a
mkStartPath pr x = case ordering pr of 
        Nothing -> SPath $ (,) [Node x Start Nothing] S.empty
        Just f  -> SPath $ (,) [Node x Start Nothing] (fromList f [x])

toNode :: AppliedAction a -> Node a
toNode aa@(AA x _ _ _) = Node x aa Nothing

mkAction :: String -> (a -> [a]) -> Action a
mkAction s f = Action s (\x -> zipWith (g x) (f x) [0..] )
    where g x y n = AA y s n x

data Action a = Action String (a -> [AppliedAction a])

data AppliedAction a =
    Start
    | AA a String Int a
   -- result, name, variationID, predecessor
   -- (the states it has been applied to)

applyOn :: a -> Action a -> [AppliedAction a]
applyOn a (Action _ f) = f a

getPath :: Solution a -> [a]
getPath = map getElem

-- a Path is a list of Nodes starting with the latest one.
type Path a = [Node a]

-- a SPath also has a set of the nodes. easier for equality checking
newtype SPath a = SPath ( Path a, S.Set (Order a) ) deriving Monoid

_getPath :: SPath a -> Path a
_getPath (SPath (x,_)) = x

getElems :: Path a -> [a]
getElems = map getElem

-- applies a reordering function over Paths to SPaths.
-- its like a functor from Paths to SPaths
reorderPaths :: Problem a -> ( [Path a] -> [Path a] ) -> ( [SPath a] -> [SPath a] )
reorderPaths pr f xs = let ys  = f $ map _getPath xs
                       in  case ordering pr of
                            Just ord -> 
                                    let ys' = map (fromList ord . getElems) ys
                                    in  zipWith (SPath .: (,)) ys ys'
                            Nothing -> zip

fromList :: (a -> a -> Ordering) -> [a] -> S.Set (Order a)
fromList f = S.fromList . map (`Order` f)

prepend :: Node a -> SPath a -> SPath a
prepend x (SPath (xs,xs')) = SPath $ (,) (x:xs) (S.insert x xs')

overPaths :: ([Path a] -> b) -> [SPath a] -> b
overPaths f = \xs -> f (map _getPath xs)

-- since a might not be instance of Ord, but we are given an implementation
-- we create a datastructure here solely for implementing the typeclass
-- to use in S.Set
data Order a = Order a (a -> a -> Ordering)

instance Ord (Order a) where
    compare (Order a f) (Order b _) = f a b

instance Eq (Order a) where (Order a f) == (Order b _) = f a b == EQ

type Heuristic a = a -> HValue

type HValue = Double -- currently, limiting heuristics to Double

type Solution a = Path a
