
import Graf
import Data.Map.Strict as M (Map, null, fromList, filter, assocs)
import Data.Set as S (Set, foldr, filter)
import Control.Applicative ((<$>), (<*>))
import Data.List as L (minimumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

-- utility funciton to compose two functions applying two argument to the first one insteadt of only one.
-- See "(f .) . g" on stackexchange haskell       -- why is this not in standard haskell libraries? Prelude, Data.Function ... nowhere.
dot :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
dot = (.) . (.) -- boobs

-- see GRBuch GKA Dijkstra for more info.


dijkstraF :: DirInf Int a -> Vertex -> Graph Int a -> Graph Int (a, Maybe (Vertex, Int))
dijkstraF dirinfo source gr = shortestPathgraph
        where
            shortestPathgraph = finalize resGraph
            resGraph = while hasUninspected (step dirinfo) initGraph
            initGraph = mapVWithKey initialize gr
            initialize v a
                    | v == source = (Left 0, Just v, False, a)
                    | otherwise = (Right (), Nothing, False, a)
;

dijkstraU :: Vertex -> Graph Int a -> Graph Int (a, Maybe (Vertex, Int))
dijkstraU = dijkstraF (labelU, adjacent) -- adjacent are outgoing vertices for undirected graphs

dijkstraD :: Vertex -> Graph Int a -> Graph Int (a, Maybe (Vertex, Int))
dijkstraD = dijkstraF (labelD, fst `dot` outgoingD )


-- These two following Types are just for me to delegate the two functions differentiating
-- directed and undirected to the lower levels functions they are used by.
-- LabelF is the type of the functions to access the labels of a graph.

-- A graphs label can be interpreted as a directed or undirected edge.
type LabelF i a = Graph i (FoldElems a) -> Edge -> Maybe i
-- the DirInf stands for direction information.
type DirInf i a = (LabelF i a, -- label accessor
                    Vertex -> Graph i (FoldElems a) -> Set Vertex)  -- outgoing accessor
;
-- The graph needs to hold additional information for the steps.
type FoldGraph a = Graph Int (FoldElems a)
-- the simple names of the vertices are transfered into a Tuple4 containing (Entf, Vorg, OK, a)
type FoldElems a = (Either Int (), Maybe Int, Bool, a)
    -- The Either Int () represents a numeric field with infinity.
    -- Taking Double with its built in Infinity ist suited, because its not a polymorphic/generic number format.
    -- Should allow any type of numberic+ordable type as distances.
    -- Nothing cannot be used here for infinity, because:
    -- Just 5 < Nothing == False
    -- But Either works because it ifrst sorts by the constructor.
    -- Left x < Right y == True
    -- The Maybe Int represents a possible predeseccor vertice.
    -- The Bool is sued for Dijkstra to mark inspected vertices.
    -- And the a is the original name of the graphs vertice.

-- Turns a Graph used for the algorithm into a final graph.
finalize :: FoldGraph a -> Graph Int (a, Maybe (Vertex, Int))
finalize gr = let f (mdist, mpred, _, a) =
                            (a, (,) <$> (toMaybe mdist) <*> mpred) -- Either Int () -> Maybe Int -> Maybe (Int, Int)
                   in mapV f gr
;

toMaybe :: Either a b -> Maybe a
toMaybe (Left a) = Just a
toMaybe _ = Nothing

while :: (a -> Bool) -> (a -> a) -> a -> a
while g f x
            | g x = while g f (f x)
            | otherwise = x
;

hasUninspected :: FoldGraph a -> Bool
hasUninspected gr = not . M.null . M.filter (not . thr4) . vertices $ gr

step :: DirInf Int a -> FoldGraph a -> FoldGraph a
step dirinfo gr = relaxed
    where
        relaxed = relaxEdges dirinfo h inspectedH
        h = minDistUninspected gr
        inspectedH = fname (\(dist,pred,ok,a) -> (dist, pred, True, a)) h gr
;

getMinDist :: Map Vertex (FoldElems a) -> Vertex
getMinDist mp = fst . L.minimumBy (comparing (fst4 . snd)) . M.assocs $ mp

minDistUninspected :: FoldGraph a -> Vertex
minDistUninspected gr = getMinDist . M.filter (not . thr4) $ vertices gr

relaxEdges :: DirInf Int a -> Vertex -> FoldGraph a -> FoldGraph a
relaxEdges (labelf, out) h gr' = S.foldr (relax labelf h) gr' candidates
    where
        candidates :: Set Vertex
        candidates = S.filter (uninspected gr') . out h $ gr'
;

thr4 :: (a,b,c,d) -> c
thr4 (a,b,c,d) = c

fst4 :: (a,b,c,d) -> a
fst4 (a,b,c,d) = a

uninspected :: FoldGraph a -> Vertex -> Bool
uninspected = (not . thr4) `dot` unsafeNameOf
;

dist :: FoldGraph a -> Vertex -> Either Int ()
dist gr = (\(d, _,_,_) -> d) . unsafeNameOf gr

relax :: LabelF Int a -> Vertex -> Vertex -> FoldGraph a -> FoldGraph a
relax labelf h v gr = fname f v gr
    where
        f tpl@(d,pred, ok, a)
            | d > alternative = (alternative, Just h, ok, a)
            | otherwise = tpl
        alternative = dist gr h `eitherAdd` label
        ea `eitherAdd` eb = flipEither $ do
                            a <- flipEither ea
                            b <- flipEither eb
                            return (a+b)
        label = case labelf gr (h,v) of
                    Nothing -> Right ()
                    Just x -> Left x
;

flipEither :: Either a b -> Either b a
flipEither (Left a) = Right a
flipEither (Right b) = Left b

myGraph :: Graph Int ()         -- graph30 from GKA -- 0 to 4 stands for A to E
myGraph = fromLabels $ M.fromList $
                    zip
                        (zip
                            [0,0,0,0,1,1,1,2,2,3]   -- erste spalte
                            [1,2,3,4,2,3,4,3,4,4]   -- zweite spalte
                        )
                        [5,10,15,20,35,40,45,25,30,50]  -- gewichtungsspalte
;

-- TODO:
-- * generic label type: shortestPaths :: (Bounded i, Num i, Ord i) => Graph i a -> Vertex -> Graph (i, Maybe Vertex) a
-- * function to calculate the path out of the new graph.


-- maybe also a type decl for shortest path graphs

pathFromShortestPathsGraph :: Graph Int (a, Maybe (Vertex, Int)) -> Vertex -> Graph Int (a, Maybe ([Vertex],Int))
pathFromShortestPathsGraph gr src = mapVWithKey (fmap . predToPath gr src) $ gr

predToPath :: Graph Int (a, Maybe (Vertex, Int)) -> Vertex -> Vertex -> Maybe (Vertex, Int) -> Maybe ([Vertex], Int)
predToPath gr src dest Nothing = Nothing
predToPath gr src dest (Just (pred, dist)) | src == dest = Just ([], dist)
predToPath gr src dest (Just (pred, dist)) = Just (dest: (predToPath gr src pred (fst . fromJust $ unsafeNameOf gr pred)) , dist)

sp1 = dijkstraU 3 myGraph

sp2 = dijkstraD 0 simpleGraph

simpleGraph = fromLabels $ M.fromList $ zip
                        (zip
                            [0,1,2]
                            [1,2,0]
                        )
                        [2,1,4]
;


