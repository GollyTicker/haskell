{-# LANGUAGE ExistentialQuantification, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, DeriveDataTypeable, ConstraintKinds #-}

module Types (
        module Types
    )
    where

import Data.Typeable
import Data.Maybe
import qualified Data.Map as M
import Control.Monad.Trans.RWS.Strict

type Elem a = (Show a, Typeable a)

-- Ein Knoten besteht aus seinem Namen sowie aus dessen Domainmenge

data Node = forall a. Elem a =>
            Node String (Domain a)

instance Show Node where
    show (Node s xs) = "Node " ++ show s ++ " " ++ show xs

type Domain a = [a]

type NodeName = String

var :: Elem a => String -> Domain a -> Node
var = Node

nodeName :: Node -> String
nodeName (Node s _) = s

-- create a list of Units of equal length as the domain. Used in Solving.hs
domainDummy :: Node -> [()]
domainDummy (Node _ xs) = map (const ()) xs

-- create a list of Units of equal length as the domain. Used in Solving.hs
showDomain :: Node -> String
showDomain (Node _ xs) = show xs

-- Net a ist ein Constraint Netz dessen Werte Elemente vom Typ a haben können.

data Net = Net [Node] [Constraint] deriving (Show)

-- Ein Constraint ist eine unäre/binäry Funktion die ein Node nimmt
-- und ein Node zurückliefert, bei dem fehlerhafte Elemente aus der Domain entfernt wurden.
-- Bei binären Constraints werden für Elemente der ersten Menge Partner in der Zweiten gesucht.
-- Es wird eine Modifikation des ersten Knoten zurückgeliefert sowie ein Bool das eine Änderung makiert.
-- Die Funktionen liefern beim Aufruf auf irrelevante Knoten den gleichen Knoten zurück.

data Constraint =
    forall a b. (Elem a, Elem b) =>
    Binary {
         cName :: String -- name des Constraints
        ,cNode1 :: NodeName -- name des zweiten Knotens
        ,cNode2 :: NodeName -- name des ersten Knotens
        ,apply :: (Node -> Node -> (Node, Bool)) -- Funktion
        ,originalFunction :: (a -> b -> Bool) -- Kopie der originalen Funktion
    }
    
instance Show Constraint where
    show c = "Constraint " ++ cName c

instance Eq Constraint where
    c == c' = cName c == cName c'
    
-- Configuration and return value

data Config =
    Config {
         verbose            :: Bool
        ,countStats         :: Bool
        ,algorithm          :: forall m c r. (Context m c r) => Net -> m Net
    }

-- every Context m is a place where we might use different methods to achive the same thing.
-- for example printing to console in IO vs. logging to Writer in RWS

-- a Context m c r uses the Context m to run and context r for the return value
-- and the type c for the running configuration and initial values
class Monad m => Context m c r | m -> c r where
    countAC :: m ()
    countInference :: m ()
    info :: String -> m ()
    ac :: Net -> m Net
    runSolver :: c -> m [Solution] -> r

type Solution = M.Map NodeName E

-- a pure solver can read from a Config, write into a String and
-- modify two counters and return a value a.
type Solver = RWS Config String (Int, Int)

data E = forall a. Elem a => E a deriving Typeable

instance Show E where
    show (E a) = "E " ++ show a







