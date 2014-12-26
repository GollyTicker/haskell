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

search :: PathT p a => Problem p a -> [Solution a]
search p = search' p startNodes
    where startNodes = [ mkStartPath p x | x <- starts p ]

search' :: PathT p a => Problem p a -> [p a] -> [Solution a]
search' pr ps' | null ps' = []
               | pr `checkGoalNode` tip = toSolution p : search' pr ps
               | otherwise = let children = pr `expand` tip
                                 new = mkNewPaths pr children p
                                 all = insertNewPaths pr new ps
                             in  search' pr all
    where
        (p:ps) = ps'
        tip = first p

expand :: Problem p a -> Node a -> [Node a]
expand pr x = map (toNode x) . concatMap (applyOn x) . actions $ pr

mkNewPaths :: forall p a. PathT p a => Problem p a -> [Node a] -> p a -> [p a]
mkNewPaths pr ns p = [ n `prepend` p | n <- ns, validChild n ]
    where validChild :: Node a -> Bool
          validChild n = cd || not (contains pr p n)
          cd = noCycleDetection pr



