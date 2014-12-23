{-# LANGUAGE ConstraintKinds #-}

module Types (
        module Types
    )
    where


data Strategy a =
    Depth
    | Breadth
    | Informed IStrategy (Heuristic a)

data IStrategy = A 

-- Problem a is a description of a problem where the search space
-- consists of elemets of type a.
-- In example, the water bucket problem might use (Int, Int)
-- where the two numbers represent the liters in each bucket.
data Problem a =
    Problem {
         getStarts      :: [a]
        ,checkGoalNode  :: Node a -> Bool
        ,applyStateElem :: Node a -> [Node a] -> Bool
        ,expand         :: Node a -> [Node a]
        ,evalPath       :: Path a -> Path a
        ,actions        :: [Action a]
    }


type Elem a = (Eq a)


data Node a =
    Node {
         getElem        :: a
        ,getAction      :: (AppliedAction a)
        ,getHeuristic   :: (Maybe HValue)
    }

mkStartNode :: a -> Node a
mkStartNode x = Node x Start Nothing

data Heuristic a = H (Problem a -> a -> HValue)

type Action a = (String, a -> [AppliedAction a])

data AppliedAction a = AA a String [a] | Start
               -- result, name, args (the states it has been applied to)

-- a Path is a list of Nodes starting with the latest one.
type Path a = [Node a]

type HValue = Double -- currently, limiting heuristics to Double

type Solution a = Path a
