

-- http://community.haskell.org/~simonmar/par-tutorial.pdf

--  ghc -threaded -eventlog -rtsopts --make


import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe
import Control.Monad.Par



main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f
    
    let (xs,ys) = splitAt (length grids `div` 2) grids
    
    putStrLn "Starting."
    
    print $ length $ filter isJust $ runPar $ do
        i1 <- new
        i2 <- new
        fork $ put i1 (map solve xs)
        fork $ put i2 (map solve ys)
        xs' <- get i1
        ys' <- get i2
        return (xs' ++ ys')
    putStrLn "Fin."
;








