{-# LANGUAGE ImpredicativeTypes, ConstraintKinds, ExistentialQuantification, DeriveDataTypeable  #-}

module Solving (
        solve, expandFirstNode, E(..)
    )
    where

import Types
import ArcConsistency

import qualified Data.Map as M
import Data.Typeable

type Solution = M.Map NodeName E

data E = forall a. Elem a => E a deriving Typeable

instance Show E where
    show (E a) = "E " ++ show a

-- solve takes a net and returns a list of solutions
-- uses ac3 with Full Lookahead
solve :: Net -> [Solution]
solve net_ = case inspect net of
                OK solution -> [solution]
                Empty       -> []
                Expandable  -> expandedSolutions
            where
                net = ac3 net_
                expansions = expandFirstNode net
                expandedSolutions = concatMap solve expansions

data Inspection =
    OK Solution
    | Empty
    | Expandable -- add [Net]

-- getSolution returns a solution to the net,
-- if the net already has single elements as its domain
inspect :: Net -> Inspection
inspect (Net ns _)
    | any (null . domainDummy) ns                 = Empty
    | any (not . null . drop 1 . domainDummy) ns  = Expandable
    | all (null . drop 1 . domainDummy) ns        = OK $ getSolution ns
    | otherwise = error "Net neither empty, expandable nor has a solution."

getSolution :: [Node] -> Solution
getSolution = foldr f M.empty
    where f :: Node -> Solution -> Solution
          f (Node s [x]) = M.insert s (E x)

-- returns a list of nets where the first node with more than element in domain has been reduced to a singlenton element domain
-- returns empty if there aren't any expansions
expandFirstNode :: Net -> [Net]
expandFirstNode (Net ns_ cs) =
    case ns_ of 
        [] -> []    -- falls ich keine Nodes habe, dann gibts keine relevanten Netze
        ((Node s xs@(_:_:_)):ns) ->     -- falls die Domain des ersten Elements >= 2 ist:
            let newNodess :: [ [Node] ]
                newNodess = map (\x -> (Node s [x]):ns ) xs   -- erzeugen der Liste an [Node] wobei jede NodeList ein anderes Element x ausgewählt hat
                nets = map (\nss -> Net nss cs) newNodess       -- aus jedem NodeList ´wird ein netz erzeugt.
            in  nets
        (n:ns) ->   -- falls der aktuelle Knoten nicht mehr als zwei Elemente hat, wird rekursiv ein nachfolgender Knoten reduziert.
            (\(Net ns' cs') -> Net (n:ns') cs') `map` expandFirstNode (Net ns cs)
--
