
module Search (
        module Search -- TODO: export only user functions
    )
    where

-- TODO: Use Data.Sequence instead of Lists?

import Types
import Strategies

search :: Elem a => Problem a -> [Solution a]
search p = search' p startNodes
    where startNodes = [ [mkStartNode x] | x <- starts p ]


search' :: Elem a => Problem a -> [Path a] -> [Solution a]

search' pr (p@(tip:_):ps)
    | pr `checkGoalNode` tip = [p] -- TODO: all solutions here.

search' pr (p@(tip:_):ps) = 
    let children = pr `expand` tip
        new = mkNewPaths pr children p
        all = insertNewPaths pr new ps
    in  search' pr all

search' _  _              = []


mkNewPaths :: Elem a => Problem a -> [Node a] -> Path a -> [Path a]
mkNewPaths pr ns p = [ n:p | n <- ns, (validChild . getElem) n ]
    where validChild n = not $ isStateElem pr n ns'
          ns' = map getElem p



