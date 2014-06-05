
Run $ makeAndRun.bat <n>
See the results in Main.prof

However, if you dont have profiling versions of the haskell libraries, then -prof won't compile.

Find cabal/config file of you cabal installation:
Set profiling and Tests to True

And then any packages which don't have a profiling version instelled yet have to be reinstalled:
$ cabal install <package> --enable-tests --enable-library-profiling

You may also have to reinstall there and other names packages.


TODO: write the profiling actions not in batch...
* Call Main with all the different multiplications and save profiling files for each of them.
