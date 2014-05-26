

-- http://community.haskell.org/~simonmar/par-tutorial.pdf

--  ghc -threaded -eventlog -rtsopts --make


import Sudoku
import Control.Exception
import System.Environment

main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f
    putStrLn "Starting."
    mapM_ (evaluate . solve) grids
    putStrLn "Fin."
;







