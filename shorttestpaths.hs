


import Graf
import Data.Map.Strict as M (fromList)
import Data.Set as S (Set, foldr, filter)


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
        relaxed = relaxEdges inspectedH
        relaxEdges gr' = S.foldr f gr' candidates
            where
                f :: Vertex -> FoldGraph a -> FoldGraph a
                f = undefined
                candidates :: Set Vertex
                candidates = S.filter markedFalse . fst . outgoing h $ gr'
                markedFalse :: Vertex -> Bool
                markedFalse v = case unsafeNameOf (vertices gr') v
                                of (_,_,False,_) -> True
                                   _ -> False
        h = minDistUninspected gr
        minDistUninspected = undefined
        inspectedH = mapV (\v x@(dist,pred,ok,a) -> if v==h then (dist, pred, True, a) else x) gr
        


myGraph :: Graph Int ()         -- graph30 from GKA -- 0 to 4 stands for A to E
myGraph = fromLabels $ M.fromList $
                    zip
                        (zip
                            [0,0,0,0,1,1,1,2,2,3]   -- erste spalte
                            [1,2,3,4,2,3,4,3,4,4]   -- zweite spalte
                        )
                        [5,10,15,20,35,40,45,25,30,50]  -- gewichtungsspalte
;


