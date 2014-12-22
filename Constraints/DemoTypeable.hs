{-# LANGUAGE FlexibleContexts, ConstraintKinds, DeriveDataTypeable #-}

import Constraints
import qualified Data.Map as M
import Text.Printf

nums :: Domain Int
nums = [1,2,3,4]

data Animal = Tiger | Gans | Katze deriving (Show, Eq, Typeable)

net :: Net
net = Net
    [
        var "V" nums,
        var "X" nums,
        var "Y" nums,
        var "A" [Tiger,Gans,Katze],
        var "B" [Tiger,Katze],
        var "Z" (map show nums)
    ]
    [
       (mkConstraint :: On Animal) "A" (==) "B" "A == B",
       (mkConstraint :: On Int) "X" (<) "Y" "X < Y",
       (mkConstraint :: On Int) "Y" (\y v -> y +1 == v) "V" "Y +1 == V",
       (mkConstraint :: Over Int String) "X" (\x z -> show (x+1) == z) "Z" "str(X+1) = Z"
    ]

main = do
        let (sols, acs, infs, log) =
                solve net
                    $ defaultConfig { verbose = True }
        printf "%s" log
        putStrLn "Solutions:"
        mapM_ print sols
        printf "%d ACs, %d inferences.\n" acs infs


