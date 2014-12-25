
-- load with
-- $ ghci -iSearch
-- Main> :l Queens-99Problems.hs
-- ...

import Search

-- maybe better solved with Constraints?

import Control.Applicative

import Data.List

type Grid = [Int]

type P = SPath

queensP :: Int -> Problem P Grid
queensP n = mkProblem {
        starts      = [ [] ]
       ,checkGoal   = ( \ps -> length ps == n && consistent True ps )
       ,showElem    = show
       ,eqElem      = (==)
       ,heuristic   = Nothing
       ,actions     = [mkAction "Add" (act n)]
       ,strategy    = Depth
       ,noCycleDetection = False
       ,ordering    = Just compare
    }

act :: Int -> Grid -> [Grid]
act n ps = [ ps' | x <- [1..n], let ps' = x:ps, consistent False ps']

-- the first argument specifies ,if all elemets should be checked.
-- if True, all will be checked, else only the first one will be inspected.
-- if the rest is known to be consistent, then only the first one needs to be checked
consistent :: Bool -> Grid -> Bool
consistent _     []     = True
consistent False (n:ns) = and (zipWith (\d n' -> n /= n' && n + d /= n' && n - d /= n') [1..] ns)
consistent True ns = consistent False ns && consistent True (tail ns)

queens :: Int -> (Problem P Grid, [Solution Grid])
queens n = let p = queensP n
               xs = search p
           in  (p,xs)

main = let (p,sols) = queens 12
           xs = map ((:[]) . head) sols
       in do
      --mapM_ (print . head . getPath) xs -- comment out in profiling mode.
        print (length xs)

