{-# LANGUAGE RankNTypes, ExistentialQuantification, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

import Control.Monad.State.Strict
import Control.Monad (replicateM)
import Data.List (find)
import Data.Maybe (fromJust)
import Debug.Trace
import Data.Typeable

-- Ein Knoten besteht aus seinem Namen sowie aus dessen Domain an besetzbaren Werten
data Node = forall a. Elem a =>
                Node NodeName (Domain a)

type Domain a = [a]
type NodeName = String

instance Show Node where
    show (Node s ls) = "Node " ++ s ++ " " ++ show ls

-- Ein Constraint Netz besteht aus den Knoten und den Constraints
data Net = Net [Node] [Constraint] deriving Show

{-
Ein Type synonym für Show und Typeable class.
f :: (Show a, Typeble a) => ...
kann nun ersetzt werden durch
f :: Elem a => ...
-}
type Elem a = (Show a, Typeable a)

data Constraint =
    forall a. Elem a =>
        Binary {
            name   :: String, -- Name of Constraint
            first  :: NodeName, -- Name of first Node
            second :: NodeName, -- Name of second Node
            over   :: Node -> Node -> (Node, Bool),
                -- funciton to run the constraint on two arbitrary nodes.
                -- if it returns false, then the constraint was violated.
            getConstraint :: (a -> a -> Bool)
                -- a copy of the original constrain on the original type
        }

instance Show Constraint where
    show c = "Binary " ++ name c

generalize ::
    (Typeable a, Typeable b, Typeable c) =>
    (a -> a -> Bool) -- original simple polymorphic function
    -> b -> c -> Bool -- becomes a generic function
generalize f b c = case (cast b, cast c) of
        (Just a, Just a') -> f a a'
        _ -> error "Should not have been called on differing types."

mkConstraint :: forall a. Elem a =>
    NodeName            -- first node
    -> (a -> a -> Bool) -- funciton
    -> NodeName         -- second node
    -> String           -- name of constriant
    -> Constraint       -- return
mkConstraint n1 f n2 s = Binary s n1 n2 g f
    where
        comp :: forall b c. (Typeable b, Typeable c) => b -> c -> Bool
        comp = generalize f
        g :: Node -> Node -> (Node, Bool)
        g xNode@(Node x' xs) (Node y' ys)
            | x' == n1 && y' == n2 =
                let xs' :: Domain a
                    xs' = fromJust . cast
                            $ [ x | x <- xs, any (\y -> x `comp` y) ys]
                in ( Node x' xs', (length xs') /= length (xs) )
            | otherwise = (xNode, False)
-- putting into type synonym for lateron
type Over a = Elem a =>
                NodeName -> (a -> a -> Bool) -> NodeName
                -> String -> Constraint


{-arcconsistency1 :: forall a. (Eq a, Show a) => Net a -> IO (Net a)
arcconsistency1 net@(Net ns cs) = result
    where
        result = ns'
        queue :: [Constraint a]
        queue = concatMap (\c -> [c, flop c]) cs
        ns' = evalStateT reviser (net, queue)

-- St a is the type used in the stateful computation.
type S a = (Net a, [Constraint a])

-- implementing AC1
reviser :: Show a => StateT (S a) IO (Net a)
reviser = do
    (net, cs) <- get
    lift $ putStr "Iteration: "
    lift $ print net
    changes <- replicateM (length cs) checkSingleConstraint
    (newnet, _) <- get
    if any (==True) changes
        then modify (\(n,_) -> (n,cs)) >> reviser
        else return newnet
    
checkSingleConstraint :: Show a => StateT (S a) IO Bool
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
                    do
                        when changed
                            (lift (putStrLn $ s1 ++ " vs " ++ s2 ++ ": " ++ show n1 ++ " -> " ++ show n1'))
                        put (newnet, cs')
                        return changed

-- replaces first occurrence of x in the list by y
replaceNode :: Node a -> Node a -> [Node a] -> [Node a]
replaceNode x y [] = error "Not found in replace" 
replaceNode x y (b:bs)
    | fst b == fst x = y:bs
    | otherwise = b : replaceNode x y bs

equals :: String -> String -> Constraint Int
equals x y = constraint (==) (show x ++ " == " ++ show y) x y

mult2equals :: String -> String -> Constraint Int
mult2equals x y = constraint (\x y -> x*2 == y) (show x ++ " *2 == " ++ show y) x y
-}

flop :: Constraint -> Constraint
flop (Binary name s1 s2 c f) = mkConstraint s1 (flip f) s2 name


