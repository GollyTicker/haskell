

-- comparision of different matrix multiplications


--for forcing evaluation
import Control.Exception.Base
import Control.DeepSeq

import Control.Parallel -- not yet used

import System.Environment (getArgs) --command line arguments

-- my Matrix
import Matriks

-- profiling: http://book.realworldhaskell.org/read/profiling-and-optimization.html


-- the calculation to be measured
workload d multId = (mults !! multId) (plusMat d) (plusMat d)

-- read matrix dimension from shell arguments and calculate workload
main = getArgs >>= \xs -> case xs of [n,multId] -> evaluate $ workload (read n) (read multId) `deepseq` (); _ -> putStrLn "Usage: main.exe <n> +RTS -p"
