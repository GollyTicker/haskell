-- This program, when run, prints itself on the terminal
-- This program is the fix point of `runhaskell`. This means that following equalities in bash hold.
-- $ cat somehs.hs
--  = $ cat somehs.hs | runhaskell
--  = $ cat somehs.hs | runhaskell | ... | runhaskell

main :: IO ()
main =
  putStrLn
    . (\s -> s ++ show s)
    $ "-- This program, when run, prints itself on the terminal\n-- This program is the fix point of `runhaskell`. This means that following equalities in bash hold.\n-- $ cat somehs.hs\n--  = $ cat somehs.hs | runhaskell\n--  = $ cat somehs.hs | runhaskell | ... | runhaskell\n\nmain :: IO ()\nmain =\n  putStrLn\n    . (\\s -> s ++ show s)\n    $ "