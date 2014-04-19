
import Graf
import Data.Map.Strict as M (fromList)
import Data.List (minimumBy, permutations)
import Data.Ord (comparing)

-- http://en.wikipedia.org/wiki/Ant_colony_optimization_algorithms#Overview

{- 30-TSPsimple.graph
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

myGraph :: Graph Int ()         -- graph30 from GKA -- 0 to 4 stands for A to E
myGraph = fromLabels $ M.fromList $
                    zip
                        (zip
                            [0,0,0,0,1,1,1,2,2,3]   -- erste spalte
                            [1,2,3,4,2,3,4,3,4,4]   -- zweite spalte
                        )
                        [5,10,15,20,35,40,45,25,30,50]  -- gewichtungsspalte
;

namedGraph = myGraph `withVerticeMap` (M.fromList $ zip [0..4] ['A'..'E'])

demoMap = mapE (\(s,d) w -> (s+1)*(s+6)*(d+3) `div` (s+1)) namedGraph
{-
*Main> mapE (\(s,d) w -> (s+1)*(s+6)*(d+3) `div` (s+1)) namedGraph
Graph
    0: 'A'
    1: 'B'
    2: 'C'
    3: 'D'
    4: 'E'
    0 -> 1 | 24
    0 -> 2 | 30
    0 -> 3 | 36
    0 -> 4 | 42
    1 -> 2 | 35
    1 -> 3 | 42
    1 -> 4 | 49
    2 -> 3 | 48
    2 -> 4 | 56
    3 -> 4 | 63
*Main> bruteForceTSP demoMap
([1,2,3,0,4],210)
-}

-- ohne mittlere diagonale
hausDesNikolaus = fromEdgeList $ zip
                                    [0,0,0,1,2,3]
                                    [1,3,4,2,3,4]
;

-- mit mittlerer Diagonale
hausDesNikolaus' = hausDesNikolaus `addEdges` [(2,0), (3,1)]


-- its assumed that all vertices are indeed connected. this is not checked!
bruteForceTSP :: (Ord i, Num i) => Graph i a -> ([Vertex], i)
bruteForceTSP gr =  minimumBy (comparing snd)
                    . map (tourDistance gr)
                    . permutations
                    . allVlist
                    $ gr
;

tourDistance :: (Eq i, Num i) => Graph i a -> [Vertex] -> ([Vertex], i)
tourDistance gr vs = (vs, sum . mapConsecutives dist $ vs)
            where
                mapConsecutives f ls = zipWith f ls (tail $ cycle ls)
                dist from to = unsafeLabelU gr (from, to)
;