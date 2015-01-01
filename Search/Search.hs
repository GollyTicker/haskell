{-# LANGUAGE ScopedTypeVariables #-}
module Search (
        module Search -- TODO: export only user functions
       ,module Types
       ,module Utils
       ,module Strategies
    )
    where

-- TODO: Use Data.Sequence instead of Lists?
-- DONE: Profiling first!
-- DONE? Tests? xD
-- DONE: Search als Transformer um effekte in expand zu intigrieren
-- TODO: Transformer schöner modellieren und intigrieren?

-- TODO: mkAction mit sprechenden Strings statt Zahlen

-- Proper Project structure: https://en.wikibooks.org/wiki/Haskell/Packaging

{-

Profiling:

https://downloads.haskell.org/~ghc/5.04/docs/html/users_guide/profiling.html
$ sudo apt-get install ghc-prof

-p installs profiling versions.
http://lambdor.net/?p=258
$ cabal install -p <pgk>

-}

import Types
import Utils
import Strategies

import Control.Monad
import Data.Maybe
import Pipes
import qualified Pipes.Prelude as P

-- SearchT
-- p implements the search tree data structure (can be cahnged for efficiency etc...)
-- m is the inner monad which is used in node expansion and goal checking.
-- a is the type of the search space. eg. (Int,Int) in bucket example
-- r is the return type of the Producer on it's termination.

-- r might be used lateron for stats etc. like in Context.hs in /Constraints
{-
newtype SearchT p m a r = SearchT {
        runSearchT ::
            (PathT p a, Monad m) =>
            Problem p m a
            -> Producer Solution a m r
    }
-}
-- SearchT could be a monad. That could mean
-- doing a search in the expansion of an outer search. (which isnt far fetched)

-- retrieve a single solution
search :: (PathT p a, Monad m) => Problem p m a -> m (Maybe (Solution a))
search p = P.head $ searchAll p

-- retrieve all solutions
searchAll :: (PathT p a, Monad m) => Problem p m a -> Producer (Solution a) m ()
searchAll p = search' p startNodes
    where startNodes = [ mkStartPath p x | x <- starts p ]

search' :: (PathT p a, Monad m) => Problem p m a -> [p a] -> Producer (Solution a) m ()
search' pr ps' = do
    status <- lift $ stat pr ps'
    case status of
        Sackgasse          -> return ()
        Goal      p ps     -> yield (toSolution p) >> search' pr ps
        Continue  tip p ps -> do
                                children <- lift $ pr `expand` tip
                                let new = mkNewPaths pr children p
                                    all = insertNewPaths pr new ps
                                search' pr all

stat :: (PathT p a, Monad m) => Problem p m a -> [p a] -> m (Status p a)
stat pr ps' =
    let tip = first p
        (p:ps) = ps'
    in if null ps' then return Sackgasse
       else do
              isGoal <- pr `checkGoalNode` tip
              if isGoal
                then return $ Goal p ps
                else return $ Continue tip p ps

data Status p a =
    Sackgasse
    | Goal (p a) [p a]
    | Continue (Node a) (p a) [p a]

expand :: Monad m => Problem p m a -> Node a -> m [Node a]
expand pr x = liftM (map (toNode x) . concat) . mapM (applyOn x) . actions $ pr

mkNewPaths :: forall p m a. PathT p a => Problem p m a -> [Node a] -> p a -> [p a]
mkNewPaths pr ns p = [ n `prepend` p | n <- ns, validChild n ]
    where validChild :: Node a -> Bool
          validChild n = cd || not (contains pr p n)
          cd = noCycleDetection pr



