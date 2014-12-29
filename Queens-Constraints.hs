{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

import Constraints

import Text.Printf

type Position = Int

mkDom :: Int -> [Int]
mkDom n = [1..n]

type Pos = (Int,Int)

queensNet :: Int -> Net
queensNet n = 
    let nodes f = map f [1..n]
    in  Net
            (nodes (\i -> var (show i) (mkDom n) ) )
            (
                allDiff (nodes show)
                ++ allDistinctPairs notDiag (nodes show)
            )


allDistinctPairs :: (Pos -> Pos -> Bool) -> [String] -> [Constraint]
allDistinctPairs f xs = 
    let pairs g = [g a b | a <- xs, b <- xs, a /= b]
    in  pairs
            (\x1 y1 ->
                mkConstraint' x1
                    (\x2 y2 ->
                        f (read x1 :: Int,x2) (read y1 :: Int,y2)
                    ) y1 (x1 ++ " notDiag " ++ y1)
            )

notDiag :: Pos -> Pos -> Bool
notDiag (x1,x2) (y1,y2) = abs (x1 - y1) /= abs (x2 - y2)

mkConstraint' :: On Int
mkConstraint' = mkConstraint

allDiff :: [String] -> [Constraint]
allDiff ls = [f a b | a <- ls, b <- ls, a /= b]
    where
        f :: String -> String -> Constraint
        f a b = mkConstraint' a (/=) b (a ++ " /= " ++ b)


main = do
        let net = queensNet 7
            (sols, acs, infs, log) =
                solve net $ defaultConfig
--      print net
        printf "%s" log
        putStrLn "Solutions:"
        mapM_ print sols
        printf "%d ACs, %d inferences.\n" acs infs

