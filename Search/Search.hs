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

{-

Profiling:

https://downloads.haskell.org/~ghc/5.04/docs/html/users_guide/profiling.html
$ sudo apt-get install ghc-prof

-}

import Types
import Utils
import Strategies

search :: Problem a -> [Solution a]
search p = search' p startNodes
    where startNodes = [ [mkStartNode x] | x <- starts p ]
          
expand :: Problem a -> Node a -> [Node a]
expand pr (Node x _ _) = map toNode . concatMap (applyOn x) . actions $ pr


search' :: Problem a -> [Path a] -> [Solution a]
search' pr ps' | null ps' = []
               | pr `checkGoalNode` tip = p : search' pr ps
               | otherwise = let children = pr `expand` tip
                                 new = mkNewPaths pr children p
                                 all = insertNewPaths pr new ps
                             in  search' pr all
    where
        (p:ps) = ps'
        tip = head p


mkNewPaths :: Problem a -> [Node a] -> Path a -> [Path a]
mkNewPaths pr ns p = [ n:p | n <- ns, (validChild . getElem) n ]
    where validChild n = cd || all (\n' -> not $ n `eq` n') ns'
          cd = noCycleDetection pr
          eq = eqElem pr
          ns' = map getElem p



