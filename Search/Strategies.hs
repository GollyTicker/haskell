{-# LANGUAGE ScopedTypeVariables #-}
module Strategies (
        module Strategies
    )
    where

import Types

import Data.List
import Data.Maybe
import Data.Ord
import Data.Monoid

-- type StrategyF a = [Path a] -> [Path a] -> [Path a]
--                    NewPaths -> OldPaths -> AllPaths

insertNewPaths :: Problem a -> StrategyF a
insertNewPaths pr = case (strategy pr, heuristic pr) of
    (Depth,_)       -> depth
    (Breadth,_)     -> breadth
    (A,Just h)      -> a pr h
    (A,_)           -> error "No heuristic given. Required by A Algorithm."


-- new paths are appended at the beginning
depth :: StrategyF a
depth = mappend

-- new paths are appended at the end
breadth :: StrategyF a
breadth = flip mappend 

-- A Algorithm using a Heuristic
-- A: f(x) = g(x) + h(x)
a :: forall a. Problem a -> Heuristic a -> StrategyF a
a pr h nps ops =
        reorderPaths pr f
        $ nps `mappend` ops
    where
        f :: [Path a] -> [Path a]
        f = (sortBy (comparing (fromJust . getHvalue . head)) . map evalPath)
        evalPath ( (Node x aa _) :ns) =
            let hvalue = h x + fromIntegral (length ns)
            in  (Node x aa (Just hvalue) ) :ns
                 -- TODO: alte Justs nicht erneut berechnen

