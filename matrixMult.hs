

import qualified Data.Map.Strict as M (map)
import Data.Map.Strict hiding (map)
import Data.List
import Control.Applicative
import Control.Exception.Base
import Control.DeepSeq
import System.Time
import System.Environment
-- import Acme.Omitted -- cannot use because no profiling libs installed :(


-- ghc -O2 --make matrixMult.hs -prof -auto-all -caf-all -fforce-recomp
-- cmd> matrixMult.exe +RTS -p

type Idx = Int
type Elem = Int

data Matrix = Matrix {
                dim :: Int,
                matrix :: Map (Idx, Idx) Elem
              }
;

instance NFData Matrix where
    rnf (Matrix d mat) = d `seq` mat `deepseq` ()
;

instance Show Matrix where
    show (Matrix dim mat) =
                let elems = map (intercalate " " . map (fixsize 3 . show . snd)) . sliding dim . toAscList $ mat
                in unlines $ ("Matrix " ++ show dim):elems
;

-- read "matrix at (i,j)"
at :: Matrix -> Idx -> Idx -> Elem
at mat i j = (! (i,j)) . matrix $ mat   -- cannot write multiple args on right side of function name in def. therefore the lambda

fromGen :: (Idx -> Idx -> Int) -> Int -> Matrix
fromGen f dim = let ks = indices dim
                    mat = fromAscList $ map (\k -> (k, uncurry f k)) ks
                in Matrix dim mat
;

indices dim = (,) <$> [0..dim-1] <*> [0..dim-1]


-- (A*B) i j = sum (k, 1 to m, A i k * B k j)
smult :: Matrix -> Matrix -> Matrix
mat1 `smult` mat2
        | n /= dim mat2 = error "Unequal size"
        | otherwise = fromGen calcElem n
            where
                calcElem :: Idx -> Idx -> Int
                calcElem i j = sum $ map (\k -> (at mat1 i k) * (at mat2 k j)) [0..n-1]
                n :: Int
                n = dim mat1
;

calc d = plusMat d `smult` plusMat d

main = getArgs >>= \[n] -> evaluate $ calc (read n) `deepseq` ()

idMat = fromGen (\i j -> if i == j then 1 else 0)
zeroMat = fromGen (const . const $ 0)

plusMat = fromGen (+)


sliding n [] = []
sliding n ls = let (pre,post) = splitAt n ls in pre : sliding n post

fixsize n str = replicate (n-(length str)) ' ' ++ str






