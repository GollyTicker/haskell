{-# LANGUAGE ScopedTypeVariables #-}

module Solving (
        solve, expandFirstNode
    )
    where

import Types
import ArcConsistency

type Solution a = [(NodeName, a)]

-- solve takes a net and returns a list of solutions
-- uses ac3 with Full Lookahead
solve :: (Eq a, Show a) => Net a -> [Solution a]
solve net_ = case inspect net of
                OK solution -> [solution]
                Empty       -> []
                Expandable  -> expandedSolutions
            where
                net = ac3 net_
                nets = map ac3 $ expandFirstNode net
                expandedSolutions = concatMap solve nets

data Inspection a =
    OK (Solution a)
    | Empty
    | Expandable

-- getSolution returns a solution to the net,
-- if the net already has single elements as its domain
inspect :: Net a -> Inspection a
inspect (Net ns _)
    | any (null . domain) ns                 = Empty
    | any (not . null . drop 1 . domain) ns  = Expandable
    | all (null . drop 1 . domain) ns        = OK $ map (\(Node s [x]) -> (s,x)) ns
    | otherwise = error "Net neither empty, expandable nor has a solution."

-- returns a list of nets where the first node with more than element in domain has been reduced to a singlenton element domain
-- returns empty if there aren't any expansions
expandFirstNode :: forall a. Net a -> [Net a]
expandFirstNode (Net ns_ cs) =
    case ns_ of 
        [] -> []    -- falls ich keine Nodes habe, dann gibts keine relevanten Netze
        ((Node s xs@(_:_:_)):ns) ->     -- falls die Domain des ersten Elements >= 2 ist:
            let newNodess :: [ [Node a] ]
                newNodess = (\x -> (Node s [x]):ns ) `map` xs   -- erzeugen der Liste an [Node] wobei jede NodeList ein anderes Element x ausgewählt hat
                nets = map (\nss -> Net nss cs) newNodess       -- aus jedem NodeList ´wird ein netz erzeugt.
            in  nets
        (n:ns) ->   -- falls der aktuelle Knoten nicht mehr als zwei Elemente hat, wird rekursiv ein nachfolgender Knoten reduziert.
            (\(Net ns' cs') -> Net (n:ns') cs') `map` expandFirstNode (Net ns cs)
--