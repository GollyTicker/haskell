
import Graf
import Data.Map.Strict as M (fromList)

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

myGraph :: Graph Int ()
myGraph = empty `withEdgeMap` myWeightedMap -- graph30 from GKA -- 0 to 4 stands for A to E
myWeightedMap :: Edges Int
myWeightedMap = M.fromList $ zip
                        (zip
                            [0,0,0,0,1,1,1,2,2,3]   -- erste spalte
                            [1,2,3,4,2,3,4,3,4,4]   -- zweite spalte
                        )
                        [5,10,15,20,35,40,45,25,30,50]  -- gewichtungsspalte
;
