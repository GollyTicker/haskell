
module Search (
         Strategy(..)
        ,IStrategy(..)
        ,Problem(..)
        ,Node
        ,Action(..)
        ,AppliedAction
        ,Path
        ,HValue
        
        ,mkStartNode
        ,search
    )
    where

import Types

search :: Elem a => Strategy a -> Problem a -> [Solution a]
search s p = search' s p startNodes
    where startNodes = [ [mkStartNode x] | x <- getStarts p ]


search' :: Elem a => Strategy a -> Problem a -> [Path a] -> [Solution a]

search' s pr (p@(tip:_):ps)
    | pr `checkGoalNode` tip = [p] -- TODO: all solutions here.

search' s pr (p@(tip:_):ps) = 
    let children = pr `expand` tip
        new = mkNewPaths children p
        all = insertNewPaths s new ps
    in  search' s pr all

search' _ _  _              = []


mkNewPaths :: Elem a => [Node a] -> Path a -> [Path a]
mkNewPaths ns p = [ n:p | n <- ns, validChild n ]
    where validChild = (`notElem` ns') . getElem
          ns' = map getElem p


insertNewPaths :: Strategy a -> [Path a] -> [Path a] -> [Path a]
insertNewPaths = undefined
