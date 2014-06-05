module Matriks
    (
        Matrix(..),
        Idx, Elem,
        fromGen,
        idMat,zeroMat,plusMat,someMat,
        at,
        smult, cmult, stmult, pmult, qmult, dmult,
        mults
    )
    where

import Control.DeepSeq

import Data.Map.Strict hiding (map)
import Control.Applicative ((<$>), (<*>))
import Data.List
import Acme.Omitted

-- ============================== Matrix Data Definition And Instances =========================

-- use Map as data structure for the square matrices
data Matrix = Matrix {
                dim :: Int,
                matrix :: Map (Idx, Idx) Elem
              }
;

type Idx = Int
type Elem = Int

-- specify Matrix evaluation
instance NFData Matrix where
    rnf (Matrix d mat) = d `seq` mat `deepseq` ()
;

-- square printing
instance Show Matrix where
    show (Matrix dim mat) =
                let elems = map (intercalate " " . map (fixsize 3 . show . snd)) . sliding dim . toAscList $ mat
                    fixsize n str = replicate (n-(length str)) ' ' ++ str   -- fixsize "34" = " 34" and fixsize "452" = "452"
                in unlines $ ("Matrix " ++ show dim):elems
;
{- Example:
ghci> fromGen (+) 6
Matrix 6
  0   1   2   3   4   5
  1   2   3   4   5   6
  2   3   4   5   6   7
  3   4   5   6   7   8
  4   5   6   7   8   9
  5   6   7   8   9  10

-}


-- ================== Matrix creators ==================================

-- builds a Matrix from a Generator function
fromGen :: (Idx -> Idx -> Int) -> Int -> Matrix
fromGen f dim = let ks = (,) <$> [0..dim-1] <*> [0..dim-1]
                    mat = fromAscList $ map (\k -> (k, uncurry f k)) ks -- fromAscList is linear (+ subject to list fusion)
                in Matrix dim mat
;

-- identity matrix, zero matrix and two sample ones. makes haskell beautiful
idMat, zeroMat, plusMat, someMat :: Int -> Matrix

idMat = fromGen (\i j -> fromEnum (i==j))   --- (fromEnum .) . (==) in point-less

zeroMat = fromGen (const . const $ 0)

plusMat = fromGen (+)

someMat = fromGen (\i j -> j*4 - i)

-- read "matrix at (i,j)"
at :: Matrix -> Idx -> Idx -> Elem
at mat i j = (! (i,j)) . matrix $ mat

-- ======================= Matrix Multiplications ======================

-- (A*B) i j = sum (k, 1 to m, A i k * B k j)


-- [s]low matrix mult - naiive implementation
smult :: Matrix -> Matrix -> Matrix
mat1 `smult` mat2 | n /= dim mat2 = error "Unequal size"
        | otherwise = fromGen calcElem n
            where
                calcElem :: Idx -> Idx -> Int
                calcElem i j = sum $ map (\k -> (at mat1 i k) * (at mat2 k j)) [0..n-1]
                n = dim mat1
;

-- multiplying with a different strategy than simply using wikipedias definition
dmult :: Matrix -> Matrix -> Matrix
dmult = const

-- multiplying by strassens algorithm
stmult :: Matrix -> Matrix -> Matrix
stmult = const

-- multiplying by delegating to C
-- the matrix can(and needs to be) be sent to the C program in linear time, because Data.Map supporst lin. time list fusioned "toAscList"
cmult :: Matrix -> Matrix -> Matrix
cmult = const

-- parallel mult of wikipedias mult definition
pmult :: Matrix -> Matrix -> Matrix
pmult = const

-- mult using vectors? fast mutable arrays. but then we need a slightly different matrix impl.
qmult :: Matrix -> Matrix -> Matrix
qmult = const

mults = [smult, dmult, stmult, cmult, pmult, qmult]

myFunc = (...)

-- ==================== MISC ===========================

-- utility list function
sliding :: Int -> [a] -> [[a]]
sliding n [] = []
sliding n ls = let (pre,post) = splitAt n ls in pre : sliding n post



