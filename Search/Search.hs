{-# LANGUAGE ScopedTypeVariables #-}
module Search (
        module Search -- TODO: export only user functions
       ,module Types
       ,module Utils
       ,module Strategies
    )
    where

-- TODO: Use Data.Sequence instead of Lists?
-- TODO: Profiling first!
-- TODO: Tests? xD
-- TODO: mkAction mit sprechenden Strings statt Zahlen
-- TODO: Search als Transformer um effekte in expand zu intigrieren

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

search :: (PathT p a, Monad m) => Problem p m a -> m [Solution a]
search p = search' p startNodes
    where startNodes = [ mkStartPath p x | x <- starts p ]

search' :: (PathT p a, Monad m) => Problem p m a -> [p a] -> m [Solution a]
search' pr ps' = do
    status <- stat pr ps'
    case status of
        Sackgasse          -> return []
        Goal      p ps     -> (toSolution p:) `liftM` search' pr ps
        Continue  tip p ps -> do
                                children <- pr `expand` tip
                                let new = mkNewPaths pr children p
                                    all = insertNewPaths pr new ps
                                search' pr all

stat :: (PathT p a, Monad m) => Problem p m a -> [p a] -> m (Status p a)
stat pr ps'@(p:ps) =
    let tip = first p
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



