{-# LANGUAGE ScopedTypeVariables #-}
module Search (
        module Search -- TODO: export only user functions
       ,module Types
       ,module Utils
       ,module Strategies
    )
    where

-- TODO: Use Data.Sequence instead of Lists?

import Types
import Utils
import Strategies

search :: Problem a -> [Solution a]
search p = search' p startNodes
    where startNodes = [ [mkStartNode x] | x <- starts p ]
          
expand :: Problem a -> Node a -> [Node a]
expand pr (Node x _ _) = map toNode . concatMap (applyOn x) . actions $ pr

search' :: Problem a -> [Path a] -> [Solution a]

search' pr (p@(tip:_):ps)
    | pr `checkGoalNode` tip = p : search' pr ps

search' pr (p@(tip:_):ps) = 
    let children = pr `expand` tip
        new = mkNewPaths pr children p
        all = insertNewPaths pr new ps
    in  search' pr all

search' _  _              = []

mkNewPaths :: Problem a -> [Node a] -> Path a -> [Path a]
mkNewPaths pr ns p = [ n:p | n <- ns, (validChild . getElem) n ]
    where validChild n = not $ isStateElem pr n ns'
          ns' = map getElem p



