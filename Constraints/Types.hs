{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable, ConstraintKinds #-}
module Types (
        Net(..),
        Constraint(..),
        Node(..),
        Domain, NodeName,
        Elem,
        
        var, nodeName,
        
        fCast
    )
    where

import Data.Typeable
import Data.Maybe

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

fCast :: (Elem a, Elem b) => a -> b
fCast = maybe (error "Typecast failed.") fromJust . cast

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
