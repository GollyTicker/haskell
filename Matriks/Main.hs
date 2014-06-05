

-- comparision of different matrix multiplications


--for forcing evaluation
import Control.Exception.Base
import Control.DeepSeq

import Control.Parallel -- not yet used

import System.Environment (getArgs) --command line arguments

-- my Matrix
import Matriks

-- import Acme.Omitted -- cannot use because no profiling libs installed :(

-- profiling: http://book.realworldhaskell.org/read/profiling-and-optimization.html



-- 150 is the matrix dimension +RTS -p produces a profiling output(.prof)

-- the calculation to be measured
workload d = plusMat d `smult` plusMat d

-- read matrix dimension from shell arguments and calculate workload
main = getArgs >>= \xs -> case xs of [n] -> evaluate $ workload (read n) `deepseq` (); _ -> putStrLn "Usage: main.exe <n> +RTS -p"
