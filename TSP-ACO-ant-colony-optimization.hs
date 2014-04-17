

-- http://en.wikipedia.org/wiki/Ant_colony_optimization_algorithms#Overview

import Data.Map.Strict as M (fromList, union, Map, toList, empty, (!), lookup)
import Data.Ord (comparing)
import Data.List (minimumBy, permutations, intercalate)
import Data.Maybe (catMaybes)
import Data.Tuple (swap)

{- graph30.graph
#ungerichtet
A,B,5
A,C,10
A,D,15
A,E,20
B,C,35
B,D,40
B,E,45
C,D,25
C,E,30
D,E,50
-}

myGraph = Graph [0..4] grMap    -- graph30 from GKA -- 0 to 4 stands for A to E
grMap = fromList $ zip
                        (zip
                            [0,0,0,0,1,1,1,2,2,3]   -- erste spalte
                            [1,2,3,4,2,3,4,3,4,4]   -- zweite spalte
                        )
                        [5,10,15,20,35,40,45,25,30,50]  -- gewichtungsspalte
;

{-
Usage:
*Main> print myGraph
Graph
    0 -> 1 (5),
    0 -> 2 (10),
    0 -> 3 (15),
    0 -> 4 (20),
    1 -> 2 (35),
    1 -> 3 (40),
    1 -> 4 (45),
    2 -> 3 (25),
    2 -> 4 (30),
    3 -> 4 (50)
*Main> bruteForceTSP myGraph
([2,3,1,0,4],120)


-}



-- only need fully connected graphs
-- no need to take care of any graph where not every vertie is connected to every other vertice.

data Graph = Graph {
                    vertices :: [Vertex],
                    edges :: WEdges
                   }
type Vertex = Int
type Weight = Int
type WEdges = Map (Vertex,Vertex) Weight    -- weighted Edges

instance Show Graph where
    show gr = "Graph\n    " ++ showGraphEdges gr
; -- the show ignores the vertice list, because all vertices are connected anyways.
-- so no need to know, how many vertices the graph has.

-- prettier printing
showGraphEdges :: Graph -> String
showGraphEdges gr = intercalate ",\n    " $ map showWeightedEdge es
        where
            showWeightedEdge ((from,to), weight) = show from ++ " -> " ++ show to ++ " (" ++ show weight ++ ")"
            es :: [((Vertex, Vertex), Weight)]
            es = toList $ edges gr
;

-- ============================================================

bruteForceTSP :: Graph -> ([Vertex],Int)
bruteForceTSP gr =
    {- ([Vertex],Int) -}      minimumBy (comparing snd) -- finally, take the minimal cost
    {- [ ([Vertex], Int) ] -} . map (tripCost (edges gr)) -- assign each one a cost of taking that route
    {- [[Vertex]] -}          . permutations  -- make all permutations of them
    {- [Vertex] -}            . vertices $ gr -- get the vertices
-- these types on the left show the hidden(pipelined) type in function composition
-- you could also see them as the return type of that line

-- for given weights, get the weigth inbetween the given vertices.
-- because the order of the vertices isnt specified, both possibilities have to be tried out.
-- we assume, that inbetween any two vertices an edge exist.
-- the U in the getU standt for Undirected access
get :: WEdges -> (Vertex, Vertex) -> Weight
get w vs = head $ catMaybes [mweight, mweight'] -- take the first working one
        where
            mweight = M.lookup vs w
            mweight' = M.lookup (swap vs) w
;

-- given weights, it calculates the round trip cost for the given route.
-- it returns the route and its cost
tripCost :: WEdges -> [Vertex] -> ([Vertex], Int)
tripCost weights vs = (vs, cost)
            where
                cost = sum $ map (get weights) csec
                -- ConsSECutive vertices
                csec :: [(Vertex,Vertex)]
                csec = take (length vs) $ zip cvs (tail cvs)
                cvs = cycle vs
;





-- =====================================================================================

-- buildK n builds the K n graph. it is a graph with n edges. each connected to every other
-- warpup. build Kn graphs to see whether the primitive graph implementation works.
buildK :: Int -> Graph
buildK 1 = Graph [0] empty
buildK n = let g' = buildK n'
               n' = n-1
               vertices' = vertices g'
               newEdges = fromList [ ((n', v), 1) | v <- vertices'] -- connect the new vertice to every other
           in Graph (n':vertices') (union newEdges (edges g'))  -- ad the new edges and the new vertice to the graph
;
-- ghci> buildK 4
-- Graph [((1,0),1),((2,0),1),((2,1),1),((3,0),1),((3,1),1),((3,2),1)]

