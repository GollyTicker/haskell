{-# LANGUAGE ConstraintKinds #-}

module Types (
        module Types
    )
    where

data Strategy a =
    Depth
    | Breadth
    | A
--  | OptimistcClimbing
--  | Custom (StrategyF a)

type StrategyF a = [Path a] -> [Path a] -> [Path a]
--                 NewPaths -> OldPaths -> AllPaths

-- Problem a is a description of a problem where the search space
-- consists of elemets of type a.
-- In example, the water bucket problem might use (Int, Int)
-- where the two numbers represent the liters in each bucket.
data Problem a =
    Problem {
         starts         :: [a]
        ,checkGoalNode  :: Node a -> Bool
        ,isStateElem    :: a -> [a] -> Bool
        ,expand         :: Node a -> [Node a] -- maybe not user-submitted lateron
        ,heuristic      :: Maybe (Heuristic a)
        ,actions        :: [Action a]
        ,strategy       :: Strategy a
        -- add custom strategies here?
    }

data EvalPath a =
    E (Path a -> Path a) -- should not reevaluate Justs

type Elem a = (Eq a)

data Node a =
    Node {
         getElem        :: a
        ,getAction      :: AppliedAction a
        ,getHvalue      :: Maybe HValue   -- Heuristics Value
    }

mkStartNode :: a -> Node a
mkStartNode x = Node x Start Nothing

type Action a = (String, a -> [AppliedAction a])

data AppliedAction a = AA a String [a] | Start
               -- result, name, args (the states it has been applied to)

-- a Path is a list of Nodes starting with the latest one.
type Path a = [Node a]

type Heuristic a = a -> HValue

type HValue = Double -- currently, limiting heuristics to Double

type Solution a = Path a
