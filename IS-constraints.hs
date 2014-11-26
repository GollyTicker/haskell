

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad (replicateM)
import Data.List (find)
import Data.Maybe (fromJust)
import Debug.Trace

-- Ein Knoten besteht aus seinem Namen sowie aus dessen Domainmenge
type Node = (String, Domain)
type Domain = [Int] -- Die Domain ist stehts Int

data Net = Net [Node] [Constraint] deriving Show

-- Ein Constraint ist eine unäre/binäry Funktion die ein Node nimmt
-- und ein Node zurückliefert, bei dem fehlerhafte Elemente aus der Domain entfernt wurden.
-- Bei binären Constraints werden für Elemente der ersten Menge Partner in der Zweiten gesucht.
-- Es wird eine Modifikation des ersten Knoten zurückgeliefert sowie ein Bool das eine Änderung makiert.
-- Die Funktionen liefern beim Aufruf auf irrelevante Knoten den gleichen Knoten zurück.
data Constraint =
    Binary
        String -- name des Constraints
        String -- name des zweiten Knotens
        String -- name des ersten Knotens
        (Node -> Node -> (Node, Bool)) -- Funktion
        (Int -> Int -> Bool) -- Kopie der originalen Funktion
    
instance Show Constraint where
    show (Binary s _ _ _ _) = "Binary " ++ s

constraint :: (Int -> Int -> Bool) -> String-> String -> String -> Constraint
constraint f name s1 s2 = Binary name s1 s2 g f
    where
        g :: Node -> Node -> (Node, Bool) -- dies hier ist eine Implementation des REVISE Algorithmus
        g xNode@(x',xs) (y',ys)
                | x' == s1 && y' == s2 =
                    let xs' = [ x | x <- xs, any (\y -> f x y) ys]
                    in ( (x', xs'), xs' /= xs )
                | otherwise = (xNode, False)

-- arcconsistency1 :: Net -> Net
arcconsistency1 net@(Net ns cs) = result
    where
        result = ns' -- Net ns' cs
        queue :: [Constraint]
        queue = concatMap (\c -> [c, flop c]) cs
        ns' = evalStateT reviser (net, queue)

type S = (Net, [Constraint])

-- implementing AC1
reviser :: StateT S IO Net
reviser = do
    (net, cs) <- get
    lift $ putStr "Iteration: "
    lift $ print net
    changes <- replicateM (length cs) checkSingleConstraint
    (newnet, _) <- get
    if any (==True) changes
        then reviser
        else return newnet
    
checkSingleConstraint :: StateT S IO Bool
checkSingleConstraint = 
    do
        ((Net ns net_cs), cs) <- get
        case cs of
            [] -> return False
            (c:cs') ->
                let (Binary name s1 s2 f _) = c
                    n1 = fromJust $ find (\n -> fst n == s1) ns
                    n2 = fromJust $ find (\n -> fst n == s2) ns
                    (n1', changed) = f n1 n2
                    ns' = replaceNode n1 n1' ns
                    newnet = Net ns' net_cs
                in
                    when True
                        (lift (putStrLn $ s1 ++ " vs " ++ s2 ++ ": " ++ show n1 ++ " -> " ++ show n1'))
                    >> put (newnet, cs')
                    >> return changed

-- replaces first occurrence of x in the list by y
replaceNode :: Node -> Node -> [Node] -> [Node]
replaceNode x y [] = error "Not found in replace" 
replaceNode x y (b:bs)
    | fst b == fst x = y:bs
    | otherwise = b : replaceNode x y bs

equals :: String -> String -> Constraint
equals x y = constraint (==) (show x ++ " == " ++ show y) x y

mult2equals :: String -> String -> Constraint
mult2equals x y = constraint (\x y -> x*2 == y) (show x ++ " *2 == " ++ show y) x y

smallerThan :: String -> String -> Constraint
smallerThan x y = constraint (<) (show x ++ " < " ++ show y) x y

flop :: Constraint -> Constraint
flop (Binary name s1 s2 c f) = constraint (flip f) name s2 s1

net = Net
        [ ("V", [1,2,3,4]), ("X", [1,2,3,4]), ("Y", [1,2,3,4]), ("Z", [1,2,3,4]) ]
        [ "X" `equals` "V" , "X" `mult2equals` "Z", "X" `smallerThan` "Y", "Y" `equals` "Z" ]




