


import Graf
import Data.Map.Strict as M (fromList)
import Data.Set as S (Set, foldr, filter)
import Control.Applicative ((<$>), (<*>))


-- shortestPaths :: (Bounded i, Num i, Ord i) => Graph i a -> Vertex -> Graph (i, Maybe Vertex) a
-- shortestPaths :: Vertex -> Graph Int a -> Graph (Int, Maybe Vertex) a
shortestPaths source gr = resGraph
        where
            resGraph = step initGraph
            initGraph = mapV initialize gr
            -- (Entf, Vorg, OK, a)
            -- Right in Entf represents an infinite distance
            initialize v a
                    | v == source = (Left 0, v, False, a)
                    | otherwise = (Right (), v, False, a)
;

type FoldGraph a = Graph Int (Either Int (), Int, Bool, a)

step :: FoldGraph a -> FoldGraph a
step gr = res
    where
        res = relaxed   -- TODO: add loop end
        relaxed = relaxEdges h inspectedH
        h = minDistUninspected gr
        minDistUninspected = undefined -- M.filter (\v -> uninspected) $ vertices gr'
        inspectedH = mapV (\v x@(dist,pred,ok,a) -> if v==h then (dist, pred, True, a) else x) gr
;

relaxEdges :: Vertex -> FoldGraph a -> FoldGraph a
relaxEdges h gr' = S.foldr (relax h) gr' candidates
    where
        candidates :: Set Vertex
        candidates = S.filter (uninspected gr') . fst . outgoing h $ gr'
       
;
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
            | d > alternative = (alternative, h, ok, a)
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


