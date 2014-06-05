

import Data.List
import Control.Exception
import Control.DeepSeq
import System.Environment
import Data.DList hiding (map, foldr)
import qualified Data.DList as D (foldr)

import Control.Parallel
import Control.Parallel.Strategies

g0 :: [Int] -> String
g0 = intercalate " -> "  . map show . map (+2)

g1 :: [Int] -> String
g1 = intercalate " -> " . map (show . (+2))

gp1 :: [Int] -> String
gp1 ls = (intercalate " -> " $ map (show . (\a -> (2::Int)+a)) ls) `using` parListChunk 1000 rdeepseq

g2 :: [Int] -> String
g2 [] = ""
g2 (x:xs) = show (x+2) ++ " -> " ++ g2 xs

g3 :: [Int] -> String
g3 = foldr f ""
    where f x ls = show (x+2) ++ " -> " ++ ls
;

g5 :: [Int] -> String
g5 = toList . D.foldr f empty . fromList
    where f :: Int -> DList Char -> DList Char
          f x ls = fromList (show (x+2)) `append ` fromList " -> " `append` ls
;


gs = [g0, g1, g2, g3, g5, gp1]

work n = (gs !! n) [0..4000000]

main = getArgs >>= \[n] -> evaluate $ work (read n) `deepseq` ()


