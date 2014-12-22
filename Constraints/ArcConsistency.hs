{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, RankNTypes #-}
module ArcConsistency (
        mkConstraint, applyUnaryConstraint, findNode, ac3, On, Over,
        Solver
    )
    where

import Types

import Data.List (find, nub)
import Data.Typeable
import Data.Maybe
import Control.Monad.State.Strict
import Text.Printf

type St = (Net, [Constraint])


-- Implementation von ac3. Kantenkonsistenz / Arcconsistency
ac3 :: forall m c r. Context m c r => Net -> m Net
ac3 net = countAC >> evalStateT reviser (net, queue)
    where
        queue = bidirectionalConstraints net
        reviser :: StateT St m Net
        reviser = do
            (net', cs') <- get
            if null cs'
                then return net'
                else reduceSingleConstraint >> reviser
--

-- reduce Single Constraints wendet den allerersten Constraint
-- in der Liste an zu validierenden Constraints an und
-- fügt bei einer Domainreduktion die nachbarn hinzu.
reduceSingleConstraint :: forall m c r. Context m c r => StateT St m ()
reduceSingleConstraint = 
    do
        (net@(Net ns ncs), cs) <- get
        case cs of
            [] -> return ()
            ((Binary _ s1 s2 f _):cr) ->
                let n1 = findNode s1 ns
                    n2 = findNode s2 ns
                    (n1', changed) = f n1 n2
                    ns' = replaceNode n1 n1' ns
                    newnet = Net ns' ncs
                    inc = incoming (nodeName n1) $ bidirectionalConstraints net
                    relevant_neighbours = filter (\c' -> cNode1 c' /= nodeName n2) inc
                    cs'
                        | changed = nub $ relevant_neighbours ++ cr
                        | otherwise = cr
                in do
                    when changed
                        (lift countInference)
                    when changed
                        (lift $ info $ printf "%s vs %s: +%d *%d | %s: %s -> %s"
                                        s1 s2 (length relevant_neighbours) (length cs')
                                        s1 (showDomain n1) (showDomain n1'))
                    put (newnet, cs')


generalize ::
    (Elem a, Elem b, Elem c, Elem d) =>
    (a -> b -> Bool) -- original simple polymorphic function
    -> (c -> d -> Bool) -- becomes a generic function
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
                    xs' = (fromJust . cast) [ x | x <- xs, any (\y -> x `comp` y) ys]
                in ( Node x' xs', (length xs') /= length (xs) )
            | otherwise = (xNode, False)

-- putting into type synonym for lateron
type Over a b = (Elem a, Elem b) =>
                NodeName -> (a -> b -> Bool) -> NodeName
                -> String -> Constraint

type On a = (Elem a) =>
                NodeName -> (a -> a -> Bool) -> NodeName
                -> String -> Constraint

applyUnaryConstraint :: forall a. Elem a => Net -> NodeName -> (a -> Bool) -> Net
applyUnaryConstraint (Net ns cs) s f =
    case findNode s ns of
        n@(Node _ dom) ->
            let n' = Node s (filter f ( (fromJust . cast) dom :: Domain a) )
                ns' = replaceNode n n' ns
            in  Net ns' cs

bidirectionalConstraints :: Net -> [Constraint]
bidirectionalConstraints (Net _ cs) = concatMap (\c -> [c, flop c]) cs

findNode :: NodeName -> [Node] -> Node
findNode s ns = fromJust $ find (\n -> nodeName n == s) ns

incoming :: NodeName -> [Constraint] -> [Constraint]
incoming n = filter (\c -> cNode2 c == n)

-- replaces first occurrence of x in the list by y
replaceNode :: Node -> Node -> [Node] -> [Node]
replaceNode _ _ [] = error "Not found in replace" 
replaceNode x y (b:bs)
    | nodeName b == nodeName x = y:bs
    | otherwise = b : replaceNode x y bs

-- flop takes a Constraint and builds its flipped Constraint.
flop :: Constraint -> Constraint
flop (Binary name s1 s2 _ f) = mkConstraint s2 (flip f) s1 name

