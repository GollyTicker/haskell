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

insertNewPaths :: PathT p a => Problem p m a -> StrategyF p a
insertNewPaths pr = case (strategy pr, heuristic pr) of
    (Depth,_)            -> depth
    (Breadth,_)         -> breadth
    (A,Just h)          -> a pr h
    (Optimistic,Just h) -> optimisticBestSearch pr h
    (_,Nothing)         ->
        error "No heuristic given. Required by the chosen algorithm."


-- new paths are appended at the beginning
depth :: PathT p a => StrategyF p a
depth = (++)

-- new paths are appended at the end
breadth :: PathT p a => StrategyF p a
breadth = flip (++)

-- A Algorithm using a Heuristic
-- A: f(x) = g(x) + h(x)
a :: forall p m a. PathT p a => Problem p m a -> Heuristic a -> StrategyF p a
a pr h nps ops =
    sortBy (comparing (fromJust . getHvalue . first))
    . map (evalPathWith (\x rest -> rest + h x))
    $ ops ++ nps

optimisticBestSearch :: PathT p a => Problem p m a -> Heuristic a -> StrategyF p a
optimisticBestSearch pr h nps _ops = 
            take 1
            . sortBy (comparing (fromJust . getHvalue . first))
            . map (evalPathWith (\x _ -> h x)) $ nps


