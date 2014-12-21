{-# LANGUAGE ScopedTypeVariables #-}
module ArcConsistency (
        mkConstraint, ac3
    )
    where

import Types

import Control.Monad.State.Strict
import Control.Monad.Identity
import Data.List (find, nub)
import Data.Maybe (fromJust)
import Debug.Trace

type S a = (Net a, [Constraint a])

-- Implementation von ac3. Kantenkonsistenz / Arcconsistency
ac3 :: forall a. (Eq a, Show a) => Net a -> Net a
ac3 net@(Net _ cs) = resultNet
    where        
        queue :: [Constraint a]
        queue = concatMap (\c -> [c, flop c]) cs    -- create initial queue of constraints.
        
        resultNet = runIdentity $ evalStateT reviser (net, queue)       -- evalState führt die Stateful Computation aus und gibt den Rückgabewert zurück
        
        reviser :: Show a => StateT (S a) Identity (Net a)
        reviser = do
            (net, cs) <- get
            if null cs
                then return net
                else reduceSingleConstraint >> reviser
--

-- reduce Single Constraints wendet den allerersten Constraint
-- in der Liste an zu validierenden Constraints an und
-- fügt bei einer Domainreduktion die nachbarn hinzu.
reduceSingleConstraint :: (Eq a, Show a) => StateT (S a) Identity Bool
reduceSingleConstraint = 
    do
        (Net ns ncs, cs) <- get
        case cs of
            [] -> return False
            (c:cr) ->
                let (Binary name s1 s2 f _) = c
                    n1 = fromJust $ find (\n -> nodeName n == s1) ns
                    n2 = fromJust $ find (\n -> nodeName n == s2) ns
                    (n1', changed) = f n1 n2
                    ns' = replaceNode n1 n1' ns
                    newnet = Net ns' ncs
                    inc = incoming (nodeName n1) ncs 
                    relevant_neighbours = filter (\c -> cNode1 c /= nodeName n2) inc
                    cs'
                        | changed = nub $ relevant_neighbours ++ cr -- nub removes duplicates
                        | otherwise = cr
                in
                    do
                        put (newnet, cs')
                        return changed

mkConstraint :: forall a. (Eq a, Show a) => NodeName -> (a -> a -> Bool) -> NodeName -> String -> Constraint a
mkConstraint s1 f s2 name = Binary name s1 s2 g f
    where
        g :: Node a -> Node a -> (Node a, Bool)
        g xNode@(Node x' xs) (Node y' ys)
                | x' == s1 && y' == s2 =
                    let xs' = [ x | x <- xs, any (f x) ys] -- dies hier ist eine Implementation des REVISE Algorithmus
                    in ( Node x' xs', xs' /= xs )
                | otherwise = (xNode, False)

incoming :: NodeName -> [Constraint a] -> [Constraint a]
incoming n = filter (\c -> cNode2 c == n)

-- replaces first occurrence of x in the list by y
replaceNode :: Node a -> Node a -> [Node a] -> [Node a]
replaceNode x y [] = error "Not found in replace" 
replaceNode x y (b:bs)
    | nodeName b == nodeName x = y:bs
    | otherwise = b : replaceNode x y bs

-- flop takes a Constraint and builds its flipped Constraint.
flop :: (Show a, Eq a) => Constraint a -> Constraint a
flop (Binary name s1 s2 c f) = mkConstraint s2 (flip f) s1 name
