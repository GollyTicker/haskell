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
import Data.List
import Data.Ord
import Debug.Trace

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

expandFirstNode :: Net -> [Net]
expandFirstNode (Net ns cs) = expandFirstNode' (Net ns' cs)
    where ns' = sortBy (comparing (length . domainDummy)) ns

-- returns a list of nets where the first node with more than element in domain has been reduced to a singlenton element domain
-- returns empty if there aren't any expansions
expandFirstNode' :: Net -> [Net]
expandFirstNode' (Net ns_ cs) =
    let (firsts, ends) = span (null . drop 1 . domainDummy) ns_ -- firsts is a list of singleton-domain nodes
    in  case ends of
         (Node s xs@(_:_:_)):ns -> 
            let newNodess :: [ [Node] ]
                newNodess = map (\x -> (Node s [x]):ns ) xs   -- erzeugen der Liste an [Node] wobei jede NodeList ein anderes Element x ausgew�hlt hat
                nets = map (\nss -> Net (firsts ++ nss) cs) newNodess       -- aus jedem NodeList �wird ein netz erzeugt.
            in  nets
         ls -> []
