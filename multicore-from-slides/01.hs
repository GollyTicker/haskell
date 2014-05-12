import GHC.Conc
import System.Info
import Text.Printf
import Data.Version

-- ghc -O2 --make -threaded 01.hs

main = do
    printf "Compiled with %s-%s on %s/%s\n"
        compilerName
        (showVersion compilerVersion)
        os arch
    printf "Running with %d OS threads" numCapabilities
;
{-
C:\Users\Swaneet\Desktop\ALLES\haskell\haskell\multicore-from-slides>01.exe +RTS -N2
Compiled with ghc-7.6 on mingw32/i386
Running with 2 OS threads
-}