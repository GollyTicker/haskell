{-# LANGUAGE ImpredicativeTypes, FlexibleContexts, ScopedTypeVariables, ConstraintKinds, MonomorphismRestriction, ExistentialQuantification  #-}

module Solving (
        module Solving
    )
    where

import Types
import ArcConsistency

import qualified Data.Map as M
import Data.Typeable
import Control.Monad
import Control.Monad.Trans.RWS.Strict
import Control.Monad.Reader as R

defaultConfig :: Config
defaultConfig =
    Config {
         verbose = False
        ,countStats = True
        ,algorithm = ac3
    }

-- type m is explicitly given to be used in the inner scope.
solveGeneric :: forall m c r. Context m c r => m () -> Net -> c -> r
solveGeneric _ net cnf = runSolver cnf (solver net)
    where
        solver :: Net -> m [Solution]
        solver net = do
            net' <- ac net
            case inspect net' of
                OK solution -> info "Found solution:" >> info (show solution) >> return [solution]
                Empty       -> info "Backtracking..." >> return []
                Expandable expansions -> info "Expanding..." >> solveExpansions expansions
        solveExpansions :: [Net] -> m [Solution]
        solveExpansions = liftM concat . mapM solver

data Inspection =
    OK Solution
    | Empty
    | Expandable [Net]

-- getSolution returns a solution to the net,
-- if the net already has single elements as its domain
inspect :: Net -> Inspection
inspect net@(Net ns _)
    | any (null . domainDummy) ns                 = Empty
    | any (not . null . drop 1 . domainDummy) ns  = Expandable $ expandFirstNode net
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
