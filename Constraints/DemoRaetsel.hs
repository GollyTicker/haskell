{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

import Constraints
import Text.Printf

nums :: [Int]
nums = [1..4]

net :: Net
net = Net
        [
            var "V" nums,
            var "X" nums,
            var "Y" nums,
            var "Z" nums
        ]
        [
            (mkConstraint :: On Int) "X" (==) "V"               "X = V",
            (mkConstraint :: On Int) "X" (\x z -> x*2 == z) "Z" "X *2 = Z",
            (mkConstraint :: On Int) "X" (<) "Y"                "X < Y",
            (mkConstraint :: On Int) "Y" (==) "Z"               "Y = Z"
        ]


main = do
        let (sols, acs, infs, log) =
                solve net
                    $ defaultConfig
        printf "%s" log
        putStrLn "Solutions:"
        mapM_ print sols
        printf "%d ACs, %d inferences.\n" acs infs

