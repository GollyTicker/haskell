
import Search

import Control.Applicative
import Text.Printf

type Buckets = (Int, Int)

maxL = 400
maxR = 401

problem :: Problem SPath Buckets
problem = mkProblem {
        starts      = [ (0,0) ]
       ,checkGoal   = ( \x -> fst x == 2 || snd x == 2 )
       ,showElem    = show
       ,eqElem      = (==)

       ,actions     = [
             mkAction "Refill" $ \(l,r) -> [ (maxL,r), (l,maxR) ]
            ,mkAction "Empty"  $ \(l,r) -> [ (0,r)   , (l,0)    ]
            ,mkAction "Shift" $ \(l,r) -> 
                                let r2l = min (maxL - l) r
                                    l2r = min (maxR - r) l
                                in  [(l+r2l,r-r2l), (l-l2r,r+l2r)]
        ]
        
       ,heuristic   = Nothing
       ,strategy    = Breadth
       ,ordering    = Just compare
    }


main = let sols :: [Solution Buckets]
           sols = search problem
       in  printSolutions problem (take 3 sols)
           *> printf "%d solutions in total.\n" (length sols)

