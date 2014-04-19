module Graf
   (
    Graph,  -- Data Type (not the constructor)
    Vertex, Edge,   -- type synonyms for Indices    
    Vertices, Edges, -- type synonyms for label/names maps
    vertices, edges, allV, allE,    -- simple accessors
    allVlist, allElist,
    nameOf, unsafeNameOf, fname,
    labelU, labelD, unsafeLabelU,     --  label access
    buildK,
    mapE, mapV, mapBi,
    empty, null,
    sizeV, sizeE,
    adjacent, incident,
    outgoing, incoming,
    fromLabels, withEdgeMap,
    fromNames, withVerticeMap,
    fromEdgeList, addEdges,
    fromString
   )
    where
;

-- TODO: documentation of functions
-- testing of functions

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (empty, toList, union,
                                       lookup, keysSet, fromSet,
                                       null, keys, mapWithKey,
                                       fromList, insert)
import Data.Tuple (swap) -- wtf, why is this not in Prelude?!
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S (findMax, insert, mapMonotonic,
                                null, toList, fromList,
                                size, map, filter,
                                foldr, empty, insert)
;
import Prelude hiding (null)

-- Use -Wall plase.

-- util. See "(f .) . g" on stackexchange haskell
dot :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
dot = (.) . (.)
-- Example:
-- concatMap f ls = concat . map f $ ls
-- concatMap f = concat . map f
-- concatMap = concat `dot` map


-- i is the labeling of edges.
-- a is the naming of vertices.
-- information on edges are called labels and on vertices names.
-- no support of multigraphs

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
{-
Returns a Set/List of the Vertices of the graph.
-}
allV :: Graph i a -> Set Vertex
allV = M.keysSet . vertices
allVlist :: Graph i a -> [Vertex]
allVlist = M.keys . vertices

-- EXPORTED
{-
Returns a Set/List of the Edges of the graph.
-}
allE :: Graph i a -> Set Edge
allE = M.keysSet . edges
allElist :: Graph i a -> [Edge]
allElist = M.keys . edges


-- EXPORTED
{-
the empty Graph.
-}
empty :: Graph () ()
empty = Graph {
                vertices = M.empty,
                edges = M.empty
            }
;

{-
Returns the number of vertices/edges in the graph.
-}
sizeV :: Graph i a -> Int
sizeV = S.size . allV
sizeE :: Graph i a -> Int
sizeE = S.size . allE

-- EXPORTED
{-
Is the graph empty?
-}
null :: Graph a i -> Bool
null g = M.null (vertices g) && M.null (edges g)

instance (Show i, Show a) => Show (Graph i a) where
    show = showGraph
;


-- EXPORTED
{-
Functions to access names of vertices
-}
unsafeNameOf :: Vertices a -> Vertex -> a
unsafeNameOf = fromJust `dot` nameOf

nameOf :: Vertices a -> Vertex-> Maybe a
nameOf = flip M.lookup

-- EXPORTED
{-
Accesses an undirected Edge and returns its contents.
Returns Nothing if no edge was found.
-}
labelU :: Eq i => Edges i -> Edge -> Maybe i
labelU w e
        | a == Nothing = b
        | otherwise = a
    where a = labelD w e
          b = labelD w (swap e)
;

-- EXPORTED
{-
Returns the labeling of the given edge of a Map of Edges.
Returns Nothing if it fails.
-}
labelD :: Eq i => Edges i -> Edge -> Maybe i
labelD = flip M.lookup

-- EXPORTED
{-
Applies a function to a named verrtice.
-}
fname :: (a -> a) -> Vertex -> Graph i a -> Graph i a
fname f v gr = gr {vertices = vs'}
        where vs' = M.insert v (f x) $ vertices gr
              x = unsafeNameOf (vertices gr) v

-- for given weights, get the weigth inbetween the given vertices.
-- because the order of the vertices isnt specified, both possibilities have to be tried out.
-- we assume, that inbetween any two vertices an edge exist.
-- the U in the labelU stands for Undirected access
-- EXPORTED
{-
Gets the label of an Edge in the edges of an undirected Graph.
It should only be used, if one is sure, that the edge really exists.
-}
unsafeLabelU :: Eq i => Edges i -> Edge -> i
unsafeLabelU = fromJust `dot` labelU
;

-- ============================================================

-- used to make a new Vertice for a given Set of used Vertices.
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
{-
Returns K(n) for non-negative n.
-}
buildK :: Vertex -> Graph () ()
buildK n | n < 0 = error $ "buildK: negative Vertex# " ++ show n
buildK 0 = empty
buildK n = let g' = buildK (n - 1)
               oldVertices = allV g'
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
-- UTIL
{-
Adds non-information to a set to make a map.
-}
setToMap :: Set a -> Map a ()
setToMap = M.fromSet (const ())
{-
Same a setToMap for lists.
-}
listToMap :: Ord a => [a] -> Map a ()
listToMap = M.fromList . map (\x -> (x, ()))

-- ================================================================

-- incident returns Sets of incident vertices and Edges.
-- a vertice is only incident with ittselfs,
-- if theres a "Schlinge" on it.
-- EXPORTED
{-
For an Vertice and an Graph this function returns the
edges its connected with and the adjacent vertices.
-}
incident :: Vertex -> Graph i a -> (Set Vertex, Set Edge)
incident v gr = asTuple . S.map g . S.filter f . M.keysSet . edges $ gr
        where
            g edge@(from, to)
                | from == v = (to, edge)
                | to == v = (from, edge)
                | otherwise = error "incident. filter false."
            f (from, to) = from == v || to == v
;
asTuple :: Set (Vertex, Edge) -> (Set Vertex, Set Edge)
asTuple = S.foldr step (S.empty, S.empty)
        where
            step (v,e) (vs,es) = (S.insert v vs, S.insert e es)
;

-- TODO: get some useful interface functions from the usage
-- of this grafadt in f.e. djisktra

-- EXPORTED
{-
Returns all vertices this vertice is connected with.
The graph is interpreted as an undirected graph.
-}
adjacent :: Vertex -> Graph i a -> Set Vertex
adjacent = fst `dot` incident

-- EXPORTED
{-
Returns all the outgoing vertices and edges for this vertex.
-}
outgoing :: Vertex -> Graph i a -> (Set Vertex, Set Edge)
outgoing v gr = let es = S.filter ((v==) . fst) . snd . incident v $ gr
                    vs = S.map snd es
                in (vs, es)
;

-- EXPORTED
{-
Returns all the outgoing vertices and edges for this vertex.
-}
incoming :: Vertex -> Graph i a -> (Set Vertex, Set Edge)
incoming v gr = let es = S.filter ((v==) . snd) . snd . incident v $ gr
                    vs = S.map fst es
                in (vs, es)
-- EXPORTED
{-
Build a new Graph from labeled Edge.
-}
fromLabels :: Edges i -> Graph i ()
fromLabels = (empty `withEdgeMap`)
-- make graph with multiples uses of `with`

-- EXPORTED
{-
Build a new Graph from labelless edges.
-}
fromEdgeList :: [Edge] -> Graph () ()
fromEdgeList es = fromLabels . listToMap $ es

-- EXPORTED
{-
Build a new Graph from named Vertices.
-}
fromNames :: Vertices a -> Graph () a
fromNames = (empty `withVerticeMap`)

-- EXPORTED
{-
Adds Edges to a labelless and nameless Graph.
-}
addEdges :: Graph () () -> [Edge] -> Graph () ()
addEdges gr es = uptEdges . listToMap $ es
        where
            uptEdges em = gr {edges = M.union em (edges gr)}

-- the with operations overwrite the old contents.

-- given a possibly inconsistent graph where
-- there might be vertices in the edgeMap which dont appear in the vertices
-- this function adds these vertices
withVoidVertices :: Graph a () -> Graph a ()
withVoidVertices gr = let newVertices = M.union (vertices gr) (getVertices $ edges gr)
                          getVertices = setToMap . S.fromList . (concatMap (\(a,b) -> [a,b])) . S.toList .  M.keysSet
                      in gr {vertices = newVertices}
;
-- EXPORTED
{-
Returns a new Graph with the same vertices but the new given edges.
-}
withEdgeMap :: Graph j () -> Edges i -> Graph i ()
withEdgeMap gr edgemap = withVoidVertices . uptEdges $ gr
        where
            uptEdges g = g {edges = edgemap}
;

-- EXPORTED
{-
Returns a new Graph where the new named vertices are used instead.
The edges are left untouched.
-}
withVerticeMap :: Graph i a -> Vertices b -> Graph i b
withVerticeMap gr vmap = gr {vertices = vmap}

-- EXPORTED
{-
Change the label of edges by their indices and old labels
-}
mapE :: (Edge -> i -> j) -> Graph i a -> Graph j a
mapE f x = x {edges = M.mapWithKey f (edges x)}

-- EXPORTED
{-
Change the name of vertices by their indices and old names.
-}
mapV :: (Vertex -> a -> b) -> Graph i a -> Graph i b
mapV f x = x {vertices = M.mapWithKey f (vertices x)}
-- EXPORTED
{-
Combines mapV and mapV into a single map.
-}
mapBi :: (Edge -> i -> j) -> (Vertex -> a -> b) -> Graph i a -> Graph j b
mapBi f g = mapV g . mapE f

-- ======================    SHOW     ==========================
nl, indent :: String
nl = "\n"
indent = "    "
showGraph :: (Show a, Show i) => Graph i a -> String
showGraph gr = "Graph "
                ++ "#V = " ++ show (sizeV gr) ++ ", "
                ++ "#E = " ++ show (sizeE gr)
                ++ showWith (Right ()) gr   -- show vertices
                ++ showWith (Left ()) gr    -- show edges
;

showWith :: (Show i, Show a) => Either () () -> Graph i a -> String
showWith choice gr = concat
                     . map ((nl ++ indent) ++)
                     -- TODO: add alphabetical sorting here
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

-- parses Graph files like from GKA
-- EXPORTED
{-
Parse a Graph from a InputFile String.
Returns a ParseError or a Parsed Graph.
-}
fromString :: String -> Either String (Graph String String) -- not yet types i and a.
fromString = undefined      -- should be using Parsec!


