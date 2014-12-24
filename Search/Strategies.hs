
module Strategies (
        module Strategies
    )
    where

import Types

import Data.List
import Data.Maybe
import Data.Ord

-- type StrategyF a = [Path a] -> [Path a] -> [Path a]
--                    NewPaths -> OldPaths -> AllPaths

insertNewPaths :: Elem a => Problem a -> StrategyF a
insertNewPaths pr = case (strategy pr, heuristic pr) of
    (Depth,_)       -> depth
    (Breadth,_)     -> breadth
    (A,Just h)      -> a h
    (A,_)           -> error "No heuristic given. Required by A Algorithm."


-- new paths are appended at the beginning
depth :: StrategyF a
depth = (++)

-- new paths are appended at the end
breadth :: StrategyF a
breadth = flip (++)


-- A Algorithm using a Heuristic
-- A: f(x) = g(x) + h(x)
a :: Heuristic a -> StrategyF a
a h nps ops =
        sortBy (comparing (fromJust . getHvalue . head))
        . map evalPath
        $ nps ++ ops
    where
        evalPath ( (Node x aa _) :ns) =
            let hvalue = h x + fromIntegral (length ns)
            in  (Node x aa (Just hvalue) ) :ns


