
import Search

import Control.Applicative
import Text.Printf
import Control.Monad.Identity
import Pipes
import qualified Pipes.Prelude as P

type Buckets = (Int, Int)

maxL = 400
maxR = 401

problem :: Problem SPath IO Buckets
problem = mkProblem {
        starts      = [ (0,0) ]
       ,checkGoal   = return . ( \x -> fst x == 2 || snd x == 2 )
       ,showElem    = show
       ,eqElem      = (==)

       ,actions     = [
             mkAction "Refill" $ \(l,r) -> return [ (maxL,r), (l,maxR) ]
            ,mkAction "Empty"  $ \(l,r) -> return [ (0,r)   , (l,0)    ]
            ,mkAction "Shift" $ \(l,r) -> 
                                let r2l = min (maxL - l) r
                                    l2r = min (maxR - r) l
                                in  return [(l+r2l,r-r2l), (l-l2r,r+l2r)]
        ]
        
       ,heuristic   = Just $ const 0
       ,strategy    = A
       ,ordering    = Just compare
    }


main = do
        let p :: Producer (Solution Buckets) IO ()
            p = searchAll problem
            c :: Consumer (Solution Buckets) IO ()
            c = forever $ do x <- await
                             lift $ printSolution problem x
                             lift $ putStrLn "---------"
        runEffect $ p >-> P.take 3 >-> c
        len <- P.length p
        printf "%d solutions in total.\n" len

