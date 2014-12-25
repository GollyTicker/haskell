
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
        ,showElem       :: a -> String  -- keep (show) in problem description
        ,checkGoal      :: a -> Bool
        ,eqElem         :: a -> a -> Bool
        ,heuristic      :: Maybe (Heuristic a)
        ,actions        :: [Action a]
        ,strategy       :: Strategy a
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
    }

checkGoalNode :: Problem a -> Node a -> Bool
checkGoalNode pr = (pr `checkGoal`) . getElem

data Node a =
    Node {
         getElem        :: a
        ,getAction      :: AppliedAction a
        ,getHvalue      :: Maybe HValue   -- Heuristics Value
    }


mkStartNode :: a -> Node a
mkStartNode x = Node x Start Nothing

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

type Heuristic a = a -> HValue

type HValue = Double -- currently, limiting heuristics to Double

type Solution a = Path a
