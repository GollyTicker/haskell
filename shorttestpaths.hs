


import Graf
import Data.Map.Strict as M (Map, null, fromList, filter, assocs)
import Data.Set as S (Set, foldr, filter)
import Control.Applicative ((<$>), (<*>))
import Data.List as L (minimumBy)
import Data.Ord (comparing)


-- shortestPaths :: (Bounded i, Num i, Ord i) => Graph i a -> Vertex -> Graph (i, Maybe Vertex) a
shortestPaths :: Vertex -> Graph Int a -> Graph Int (a, Maybe (Vertex, Int))
shortestPaths source gr = shortestPathgraph
        where
            shortestPathgraph = fromFoldGraph resGraph
            resGraph = iterateWhile hasUninspected step initGraph
            initGraph = mapV initialize gr
            -- (Entf, Vorg, OK, a)
            -- Right in Entf represents an infinite distance
            initialize v a
                    | v == source = (Left 0, Just v, False, a)
                    | otherwise = (Right (), Nothing, False, a)
;

type FoldGraph a = Graph Int (Either Int (), Maybe Int, Bool, a)

fromFoldGraph :: FoldGraph a -> Graph Int (a, Maybe (Vertex, Int))
fromFoldGraph gr = let f _ (mdist, mpred, _, a) =
                            (a, (,) <$> (toMaybe mdist) <*> mpred)
                   in mapV f gr 
;

toMaybe :: Either a b -> Maybe a
toMaybe (Left a) = Just a
toMaybe _ = Nothing

iterateWhile :: (a -> Bool) -> (a -> a) -> a -> a
iterateWhile g f x
            | g x = iterateWhile g f (f x)
            | otherwise = x
;

hasUninspected :: FoldGraph a -> Bool
hasUninspected gr = not . M.null . M.filter (not . thr4) . vertices $ gr

step :: FoldGraph a -> FoldGraph a
step gr = relaxed
    where
        relaxed = relaxEdges h inspectedH
        h = minDistUninspected gr
        inspectedH = fname (\(dist,pred,ok,a) -> (dist, pred, True, a)) h gr
;

getMinDist :: Map Vertex (Either Int (), Maybe Int, Bool, a) -> Vertex
getMinDist mp = fst . L.minimumBy (comparing (fst4 . snd)) . M.assocs $ mp

minDistUninspected :: FoldGraph a -> Vertex
minDistUninspected gr = getMinDist . M.filter (not . thr4) $ vertices gr

relaxEdges :: Vertex -> FoldGraph a -> FoldGraph a
relaxEdges h gr' = S.foldr (relax h) gr' candidates
    where
        candidates :: Set Vertex
        candidates = S.filter (uninspected gr') . fst . outgoing h $ gr'
;

thr4 :: (a,b,c,d) -> c
thr4 (a,b,c,d) = c

fst4 :: (a,b,c,d) -> a
fst4 (a,b,c,d) = a

uninspected :: FoldGraph a -> Vertex -> Bool
uninspected gr v = case unsafeNameOf (vertices gr) v
                of (_,_,False,_) -> True
                   _ -> False 
;

dist :: FoldGraph a -> Vertex -> Either Int ()
dist gr = (\(d, _,_,_) -> d) . unsafeNameOf (vertices gr)

relax :: Vertex -> Vertex -> FoldGraph a -> FoldGraph a
relax h v gr = fname f v gr
    where
        f tpl@(d,pred, ok, a)
            | d > alternative = (alternative, Just h, ok, a)
            | otherwise = tpl
        alternative = dist gr h `eitherAdd` label
        ea `eitherAdd` eb = eswap $ do 
                            a <- eswap ea
                            b <- eswap eb
                            return (a+b)
        label = case labelD (edges gr) (h,v) of
                    Nothing -> Right ()
                    Just x -> Left x
;

eswap :: Either a b -> Either b a
eswap (Left a) = Right a
eswap (Right b) = Left b

myGraph :: Graph Int ()         -- graph30 from GKA -- 0 to 4 stands for A to E
myGraph = fromLabels $ M.fromList $
                    zip
                        (zip
                            [0,0,0,0,1,1,1,2,2,3]   -- erste spalte
                            [1,2,3,4,2,3,4,3,4,4]   -- zweite spalte
                        )
                        [5,10,15,20,35,40,45,25,30,50]  -- gewichtungsspalte
;

shortestPathGraph = shortestPaths 0 myGraph



