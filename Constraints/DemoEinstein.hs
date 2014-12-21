{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

import Constraints
import Data.List hiding (all)
import Prelude  hiding (all)
import Data.Maybe
import Data.Ord
import Data.Function

-- main = ac3 `with` fullLookAhead net
main :: IO ()
main = 
    let sols = solve netEinstein
    in  mapM_ printSolution sols

netEinstein :: Net Int
netEinstein = applyUnaryConstraints $ Net nodes constraints

ps :: [Int]
ps = [0..4]

nat, col, drink, cigar, anim :: [String]
nat = ["brit", "swed", "dane", "german", "norwegian"]
col = ["red", "green", "white", "blue", "yellow"]
drink = ["tea", "coffee", "milk", "beer", "water"]
cigar = ["pallmall", "dunhill", "malboro", "rothmanns", "winfield"]
anim = ["dog", "cat", "bird", "fish", "horse"]

nodes :: [Node Int]
nodes = map (`var` ps) (nat ++ col ++ drink ++ cigar ++ anim)


constraints :: [Constraint Int]
constraints = [   
        mkConstraint "brit" (==) "red" "Der Brite lebt im roten Haus.",
        mkConstraint "swed" (==) "dog" "Der Schwede hält sich einen Hund.",
        mkConstraint "dane" (==) "tea" "Der Däne trinkt gern Tee.",
        mkConstraint "green" ((==) . succ) "white" "Das grüne Haus steht links neben dem weißen Haus.",
        mkConstraint "green" (==) "coffee" "Der Besitzer des grünen Hauses trinkt Kaffee.",
        mkConstraint "pallmall" (==) "bird" "Die Person, die Pall Mall raucht, hat einen Vogel.",
        mkConstraint "yellow" (==) "dunhill" "Der Bewohner des gelben Hauses raucht Dunhill.",
        mkConstraint "winfield" (==) "beer" "Der Winfield-Raucher trinkt gern Bier.",
        mkConstraint "german" (==) "rothmanns" "Der Deutsche raucht Rothmanns.",
        mkConstraint "malboro" neighbour "cat" "Der Malboro-Raucher wohnt neben der Person mit der Katze.",
        mkConstraint "malboro" neighbour "water" "Der Malboro-Raucher hat einen Nachbarn, der Wasser trinkt.",
        mkConstraint "horse" neighbour "dunhill" "Der Mann mit dem Pferd lebt neben der Person, die Dunhill raucht.",
        mkConstraint "norwegian" neighbour "blue" "Der Norweger wohnt neben dem blauen Haus."
    ] ++ diffConstraints

diffConstraints :: [Constraint Int]
diffConstraints = concatMap allDiff [nat, col, drink, cigar, anim]

applyUnaryConstraints :: Net Int -> Net Int
applyUnaryConstraints n0 =
    let n1 = applyUnaryConstraint n0 "milk" (==middle) -- Der Mann im mittleren Haus trinkt Milch.
        n2 = applyUnaryConstraint n1 "norwegian" (==left) -- Der Norweger lebt im ersten Haus.
    in  n2
    where
        middle = ps !! 2
        left = head ps

allDiff :: [String] -> [Constraint Int]     
allDiff ls = [f a b | a <- ls, b <- ls, a /= b]
    where
        f :: String -> String -> Constraint Int
        f a b = mkConstraint a (/=) b (a ++ " /= " ++ b)

neighbour :: Int -> Int -> Bool
neighbour a b = abs (a - b) == 1

printSolution :: forall a. (Ord a, Show a) => [(String, a)] -> IO ()
printSolution sols = mapM_ printPos grouped >> putStrLn ("The " ++ owner grouped ++ " owns the fish") >> putStrLn "-------------"
        where
            sorted = sortBy (comparing snd) sols
            grouped = groupBy ((==) `on` snd) sorted -- [[(String, Int)]]
            owner = show . fst . head . fromJust . find (\ls -> fst (last ls) == "fish")
            printPos ls = print $ show (snd (head ls)) ++ ": " ++ intercalate ", " (map fst ls)
