
-- ghc -O2 --make Main.hs -prof -auto-all -caf-all -fforce-recomp

-- cmd> Main.exe $0 +RTS -p

However, if you dont have profiling versions of the haskell libraries, then -prof won't compile.

Find cabal/config file of you cabal installation:
Set profiling and Tests to True

And then any packages which don't have a profiling version instelled yet have to be reinstalled:
$ cabal install <package> --enable-tests --enable-library-profiling

You may also have to reinstall there and other names packages.

