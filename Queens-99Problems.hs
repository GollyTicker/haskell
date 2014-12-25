
-- load with
-- $ ghci -iSearch
-- Main> :l Queens-99Problems.hs
-- ...

import Search

-- maybe better solved with Constraints?

import Control.Applicative

import Data.List

type Grid = [Int]

queensP :: Int -> Problem Grid
queensP n = mkProblem {
        starts      = [ [] ]
       ,checkGoal   = ( \ps -> length ps == n && consistent ps )
       ,showElem    = show
       ,eqElem      = (==)
       ,heuristic   = Nothing
       ,actions     = [mkAction "Add" (act n)]
       ,strategy    = Depth
       ,noCycleDetection = False
    }

act :: Int -> Grid -> [Grid]
act n ps = [ ps' | x <- [1..n], let ps' = x:ps, consistent ps']

consistent :: Grid -> Bool
consistent []     = True
consistent (n:ns) =
    and (zipWith (\d n' -> n /= n' && n + d /= n' && n - d /= n') [1..] ns)
    && consistent ns

queens :: Int -> (Problem Grid, [Solution Grid])
queens n = let p = queensP n
               xs = search p
           in  (p,xs)

main = let (p,sols) = queens 13
           xs = map ((:[]) . head) sols
       in do
        mapM_ (print . head . getPath) xs -- comment out in profiling mode.
        print (length xs)

