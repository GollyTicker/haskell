
-- load with
-- $ ghci -iSearch
-- Main> :l Queens-99Problems.hs
-- ...

import Search

-- maybe better solved with Constraints?
-- NO. Queens-Constraints.hs is slower.

import Control.Applicative
import qualified Data.Set as S
import Data.List
import Data.Ord
import Text.Printf

type Grid = [Int]

type P = SPath

-- A simple description of Queens
queensP :: Int -> Problem P Grid
queensP n = mkProblem {
        starts      = [ [] ]
       ,checkGoal   = ( \ps -> length ps == n && consistent True ps )
       ,showElem    = show
       ,eqElem      = (==)
       ,heuristic   = Nothing
       ,actions     = [mkAction "Add" (act n)]
       ,strategy    = Depth
       ,noCycleDetection = True
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
           in (p,xs)

-- Solution from HaskellWiki. Much slower than my version. Errors with out of memory for n > 9
queens' :: Int -> [[Int]]
queens' n = filter test (generate n)
    where generate 0      = [[]]
          generate k      = [q : qs | q <- [1..n], qs <- generate (k-1)]
          test []         = True
          test (q:qs)     = isSafe q qs && test qs
          isSafe   try qs = not (try `elem` qs || sameDiag try qs)
          sameDiag try qs = any (\(colDist,q) -> abs (try - q) == colDist) $ zip [1..] qs

data Grid' = Grid' {
         elems      :: [Int]
        ,currLength :: Int
        ,pitfalls   :: S.Set Donts
    }

newtype Donts = Donts (Int,Int) deriving (Ord,Eq)-- (PitfallY, PropagationDirection)

getDontsPos (Donts (x,_)) = x

-- A more complicated but hopefully faster description of Queens
queensP'' :: Int -> Problem P Grid'
queensP'' n = mkProblem {
        starts      = [ Grid' [] 0 (S.empty) ]
       ,checkGoal   = ( (n==) . currLength ) -- no need to check consistency, because only consistenc ones can be created
       ,showElem    = error "no show" -- show
       ,eqElem      = \x y -> (EQ==) $ (comparing elems) x y
       ,heuristic   = Nothing
       ,actions     = [mkAction "Add" (act'' n)]
       ,strategy    = Depth
       ,noCycleDetection = True
       ,ordering    = Just (comparing elems)
    }

advanceOne :: Donts -> Donts
advanceOne (Donts (y,x)) = Donts (y + x,x)

act'' :: Int -> Grid' -> [Grid']
act'' n (Grid' ls len set) = [ Grid' (i:ls) (len + 1) (set' i) | i <- [1..n], i `notElem` donts]
    where
        donts = map getDontsPos $ S.toList advanced
        advanced = S.map advanceOne set 
        set' i =
            let mkDonts f = Donts (i, f)
            in  advanced `S.union` S.fromList [ mkDonts 1, mkDonts 0, mkDonts (-1) ]

queens'' :: Int -> (Problem P Grid', [Solution Grid'])
queens'' n = let p'' = queensP'' n
                 xs'' = search p''
           in  (p'',xs'')

main = let n = 12
           
           (p,sols) = queens n
           xs = map (getElem . head) sols
           
           xs' = queens' n
           
           (p'',sols'') = queens'' n
           xs'' = map (elems . getElem . head) sols''
           
           
       in do
      --mapM_ (print . head . getPath) xs -- comment out in profiling mode.
        -- my version with simple search is the fastest, yay!
        putStrLn "Search variant (queens)"
        printf "  head:  %s\n" (show $ head xs)
        printf "  found: %d\n" (length xs)
        putStrLn "Search variant with precise node expansion (queens'')"
        printf "  head:  %s\n" (show $ head xs'')
        printf "  found: %d\n" (length xs'')
        putStrLn "Search variant with careful node expansions (queens'')"
        printf "  head:  %s\n" (show $ head xs'')
        printf "  found: %d\n" (length xs'')
        putStrLn "Solution from HaskellWiki (queens')"
        printf "  head:  %s\n" (show $ head xs')
        printf "  found: %d\n" (length xs')

