
module Types (
        Net(..),
        Constraint(..),
        Node(..),
        Domain, NodeName,
        
        var, nodeName, domain
    )
    where


-- Ein Knoten besteht aus seinem Namen sowie aus dessen Domainmenge

data Node a = Node String (Domain a) deriving (Show, Eq)

type Domain a = [a]

type NodeName = String

var :: String -> Domain a -> Node a
var = Node

nodeName :: Node a -> String
nodeName (Node s _) = s

domain :: Node a -> Domain a
domain (Node _ xs) = xs


-- Net a ist ein Constraint Netz dessen Werte Elemente vom Typ a haben k�nnen.

data Net a = Net [Node a] [Constraint a] deriving Show


-- Ein Constraint ist eine un�re/bin�ry Funktion die ein Node nimmt
-- und ein Node zur�ckliefert, bei dem fehlerhafte Elemente aus der Domain entfernt wurden.
-- Bei bin�ren Constraints werden f�r Elemente der ersten Menge Partner in der Zweiten gesucht.
-- Es wird eine Modifikation des ersten Knoten zur�ckgeliefert sowie ein Bool das eine �nderung makiert.
-- Die Funktionen liefern beim Aufruf auf irrelevante Knoten den gleichen Knoten zur�ck.

data Constraint a =
    Binary {
         cName :: String -- name des Constraints
        ,cNode1 :: NodeName -- name des zweiten Knotens
        ,cNode2 :: NodeName -- name des ersten Knotens
        ,apply :: (Node a -> Node a -> (Node a, Bool)) -- Funktion
        ,originalFunction :: (a -> a -> Bool) -- Kopie der originalen Funktion
        }
    
instance Show (Constraint a) where
    show c = "Constraint " ++ cName c

instance Eq (Constraint a) where
    c == c' = cName c == cName c'