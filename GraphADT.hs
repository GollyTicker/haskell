

-- http://en.wikipedia.org/wiki/Ant_colony_optimization_algorithms#Overview

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (empty, fromList, toList,
                                       union, lookup, keysSet,
                                       fromSet)
import Data.Tuple (swap) -- wtf, why is this not in Prelude?!
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S (findMax, insert, mapMonotonic,
                                null)

-- Use -Wall plase.

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

-- i is the labeling of edges.
-- a is the naming of vertices.
-- information on edges are called labels and on vertices names.

-- EXPORTED
data Graph i a = Graph {
                    vertices :: Vertices a,
                    edges :: Edges i
                   }
type Vertex = Int
type Vertices a = Map Vertex a
type Edge = (Vertex, Vertex)
type Edges i = Map Edge i    -- weighted Edges

-- EXPORTED
allV :: Graph i a -> Set Vertex
allV = M.keysSet . vertices

-- EXPORTED
allE :: Graph i a -> Set Edge
allE = M.keysSet . edges
;

instance (Show i, Show a) => Show (Graph i a) where
    show gr = "Graph" ++ showGraphVertices gr ++ showGraphEdges gr
;

nl, indent :: String
nl = "\n"
indent = "    "

showGraphVertices :: (Show i, Show a) => Graph i a -> String
showGraphVertices gr = showWith (Right ()) gr

-- prettier printing
showGraphEdges :: (Show i, Show a) => Graph i a -> String
showGraphEdges gr = showWith (Left ()) gr
;

showWith :: (Show i, Show a) => Either () () -> Graph i a -> String
showWith choice gr = concat
                     -- TODO: add alphabetical sorting here
                     . map ((nl ++ indent) ++)
                     . either caseEdges caseVertices
                     $ choice
        where
            caseEdges = const (map showEdge . M.toList . edges $ gr)
            caseVertices = const (map showVertice . M.toList . vertices $ gr)
;
showVertice :: Show a => (Vertex, a) -> String
showVertice (v, a) = show v ++ ": " ++ show a
showEdge :: Show i => (Edge, i) -> String
showEdge ((from, to), i) = show from ++ " -> " ++ show to ++ " | " ++ show i


-- for given weights, get the weigth inbetween the given vertices.
-- because the order of the vertices isnt specified, both possibilities have to be tried out.
-- we assume, that inbetween any two vertices an edge exist.
-- the U in the getU stands for Undirected access
-- EXPORTED
unsafeGetU :: Eq i => Edges i -> (Vertex, Vertex) -> i
unsafeGetU w vs = fromJust $ getU w vs
;

-- EXPORTED
getU :: Eq i => Edges i -> Edge -> Maybe i
getU w vs
        | a == Nothing = b
        | otherwise = a
    where a = getD w vs
          b = getD w (swap vs)
;

-- EXPORTED
getD :: Eq i => Edges i -> Edge -> Maybe i
getD w vs = M.lookup vs w
-- ============================================================

-- EXPORTED



mkNewVertice :: Set Vertex -> Vertex
mkNewVertice vs
        | S.null vs = 1
        | otherwise = {- restructure $ -}1 + S.findMax vs
-- TODO: graph might fail if an index already is at the Int maximum.
-- duplicated at the minimum Int will appear then!

-- =====================================================================================

-- buildK n builds the K n graph. it is a graph with n edges. each connected to every other
-- warmup. build Kn graphs to see whether the primitive graph implementation works.
-- EXPORTED
buildK :: Vertex -> Graph () ()
-- TODO: add empty graph here
buildK n | n < 0 = error $ "buildK: negative VertexNumber " ++ show n
buildK 0 = empty
buildK n = let g' = buildK (n - 1)
               oldVertices = allV g'
               setToMap = M.fromSet (const ())
               newEdges = setToMap . S.mapMonotonic f $ oldVertices
               f v = (n, v)
               newVertices = setToMap $ mkNewVertice oldVertices `S.insert` oldVertices
               verticesMap = (M.union newVertices (vertices g'))
               edgesMap = (M.union newEdges (edges g'))
           in Graph {
                    vertices = verticesMap,
                    edges = edgesMap
                    }
;
-- ghci> buildK 4
-- Graph [((1,0),1),((2,0),1),((2,1),1),((3,0),1),((3,1),1),((3,2),1)]

-- ================================================================
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

-- EXPORTED
empty :: Graph () ()
empty = Graph {
                vertices = M.empty,
                edges = M.empty
            }
;

-- EXPORTED
null :: Graph a i -> Bool
null g = M.null (vertices g) && M.null (edges g)

asLabledGraph :: Edges i -> Graph () i
asLabledGraph = (empty `withEdgeMap`)
-- make graph with multiples uses of `with`

withEdgeMap :: Graph a j -> Edges i -> Graph a i
withEdgeMap = undefined

myGraph :: Graph () Int
myGraph = empty `withEdgeMap` myWeightedMap -- graph30 from GKA -- 0 to 4 stands for A to E
myWeightedMap :: Edges Int
myWeightedMap = M.fromList $ zip
                        (zip
                            [0,0,0,0,1,1,1,2,2,3]   -- erste spalte
                            [1,2,3,4,2,3,4,3,4,4]   -- zweite spalte
                        )
                        [5,10,15,20,35,40,45,25,30,50]  -- gewichtungsspalte
;