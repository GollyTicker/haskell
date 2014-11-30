{-# LANGUAGE RankNTypes, ExistentialQuantification, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module ConstraintSolving (
        Net(..), Constraint, Node,
        Domain, NodeName,
        var, mkConstraint, Elem, Over, On,
        ac1,
        Typeable
    )
    where

import Control.Monad.State.Strict
import Control.Monad (replicateM)
import Data.List (find)
import Data.Maybe (fromJust)
import Debug.Trace
import Data.Typeable

-- Ein Knoten besteht aus seinem Namen sowie aus dessen Domain an besetzbaren Werten
data Node = forall a. Elem a =>
                Node !NodeName (Domain a)

var :: Elem a => NodeName -> Domain a -> Node
var = Node

name :: Node -> NodeName
name (Node s _) = s

type Domain a = [a]
type NodeName = String

instance Show Node where
    show (Node s ls) = "Node " ++ s ++ " " ++ show ls

-- Ein Constraint Netz besteht aus den Knoten und den Constraints
data Net = Net [Node] ![Constraint]
instance Show Net where
    show (Net ns cs) = "Net \n  " ++ show ns ++ "\n  " ++ show cs

{-
Ein Type synonym für Show und Typeable class.
f :: (Show a, Typeble a) => ...
kann nun ersetzt werden durch
f :: Elem a => ...
-}
type Elem a = (Show a, Typeable a)

data Constraint =
    forall a b. (Elem a, Elem b) =>
        Binary
            String   -- Name of Constraint
            NodeName -- Name of first Node
            NodeName -- Name of second Node
            (Node -> Node -> (Node, Bool))
                -- function to run the constraint on two arbitrary nodes.
                -- if it returns false, then the constraint was violated.
            (a -> b -> Bool)
                -- a copy of the original constrain on the original type

instance Show Constraint where
    show (Binary c _ _ _ _) = "Binary " ++ c

generalize ::
    (Elem a, Elem b, Elem c, Elem d) =>
    (a -> b -> Bool) -- original simple polymorphic function
    -> c -> d -> Bool -- becomes a generic function
generalize f c d = case (cast c, cast d) of
        (Just a, Just b) -> f a b
        _ -> error "Should not have been called on differing types."

mkConstraint :: forall a b. (Elem a, Elem b) =>
    NodeName            -- first node
    -> (a -> b -> Bool) -- funciton
    -> NodeName         -- second node
    -> String           -- name of constriant
    -> Constraint       -- return
mkConstraint n1 f n2 s = Binary s n1 n2 g f
    where
        comp :: forall c d. (Elem c, Elem d) => c -> d -> Bool
        comp = generalize f
        g :: Node -> Node -> (Node, Bool)
        g xNode@(Node x' xs) (Node y' ys)
            | x' == n1 && y' == n2 =
                let xs' :: Domain a
                    xs' = fromJust $ cast [ x | x <- xs, any (\y -> x `comp` y) ys]
                in ( Node x' xs', (length xs') /= length (xs) )
            | otherwise = (xNode, False)


-- putting into type synonym for lateron
type Over a b = (Elem a, Elem b) =>
                NodeName -> (a -> b -> Bool) -> NodeName
                -> String -> Constraint

type On a = (Elem a) =>
                NodeName -> (a -> a -> Bool) -> NodeName
                -> String -> Constraint

-- arc consistency 1
ac1 :: Net -> IO Net
ac1 net@(Net ns cs) = result
    where
        result = evalStateT reviser (net, queue)
        queue = concatMap (\c -> [c, flop c]) cs
        reviser :: StateT S IO Net
        reviser = do
            (net, cs) <- get
            lift $ putStrLn "Next Iteration: "
            lift $ print net
            changes <- replicateM (length cs) checkSingleConstraint
            (newnet, _) <- get
            if any (==True) changes
                then modify (\(n,_) -> (n,cs)) >> reviser
                else lift (putStrLn "Finished with: ")
                     >> lift (print newnet)
                     >> return newnet


-- The State is the current net and a list of remaining
-- to be validated constraints
type S = (Net, [Constraint])

checkSingleConstraint :: StateT S IO Bool
checkSingleConstraint = 
    do
        ((Net ns net_cs), cs) <- get
        case cs of
            [] -> return False
            ((Binary cname s1 s2 f _):cs') ->
                let n1 = fromJust $ find (\n -> name n == s1) ns
                    n2 = fromJust $ find (\n -> name n == s2) ns
                    (n1', changed) = f n1 n2
                    ns' = replaceNode n1 n1' ns
                    newnet = Net ns' net_cs
                in
                    when changed
                        (lift (putStrLn $ cname ++  ": " ++ show n1 ++ " -> " ++ show n1'))
                    >> put (newnet, cs')
                    >> return changed


-- replaces first occurrence of x in the list by y
replaceNode :: Node -> Node -> [Node] -> [Node]
replaceNode x y [] = error "Not found in replace" 
replaceNode x y (b:bs)
    | name b == name x = y:bs
    | otherwise = b : replaceNode x y bs


flop :: Constraint -> Constraint
flop (Binary name s1 s2 c f) = mkConstraint s2 (flip f) s1 name


