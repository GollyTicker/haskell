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
    where startNodes = [ mkStartPath p x | x <- starts p ]
          
expand :: Problem a -> Node a -> [Node a]
expand pr (Node x _ _) = map toNode . concatMap (applyOn x) . actions $ pr


search' :: Problem a -> [SPath a] -> [Solution a]
search' pr ps' | null ps' = []
               | pr `checkGoalNode` tip = _getPath p : search' pr ps
               | otherwise = let children = pr `expand` tip
                                 new = mkNewPaths pr children p
                                 all = insertNewPaths pr new ps
                             in  search' pr all
    where
        (p:ps) = ps'
        tip = head . _getPath $ p
        
{-

search' pr (p@(tip:_):ps)
    | pr `checkGoalNode` tip = p : search' pr ps

search' pr (p@(tip:_):ps) = 
    let children = pr `expand` tip
        new = mkNewPaths pr children p
        all = insertNewPaths pr new ps
    in  search' pr all

search' _  _              = []

-}

mkNewPaths :: Problem a -> [Node a] -> SPath a -> [SPath a]
mkNewPaths pr ns p = [ n `prepend` p | n <- ns, (validChild . getElem) n ]
    where validChild n = cd || all (\n' -> not $ n `eq` n') ns'
          cd = noCycleDetection pr
          eq = eqElem pr
          ns' = map getElem p



