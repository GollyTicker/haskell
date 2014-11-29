{-# LANGUAGE RankNTypes, ScopedTypeVariables, ExistentialQuantification #-}

-- http://stackoverflow.com/questions/3071136/what-does-the-forall-keyword-in-haskell-ghc-do


import Data.Typeable
import Text.Printf

f :: forall a. a -> a
f = id



-- ScopedTypeBariables
rfold :: forall a b. (a -> b -> b) -> b -> [a] -> b
rfold f z ls = go ls
    where
        go :: [a] -> b
        go [] = z
        go (x:xs) = f x (go xs)
{-
> rfold ((++). show) "" [1,2,3]
"123"
-}

-- RankNTypes
onBoth :: (forall a. a -> f a) -> (a, b) -> (f a, f b)
onBoth f (a,b) = (f a, f b)

{-
> onBoth (:[]) ("sdf", 4)
(["sdf"],[4])

onBoth (Just) ("sdf", 4)
(Just "sdf",Just 4)
-}


-- ExistentialQuantification

data Constraint = forall a. Eq a => Binary (a -> a -> Bool) a a

apply :: Constraint -> Bool
apply (Binary f a b) = f a b

cons :: Constraint
cons = Binary (==) 5 5

zwei :: Constraint
zwei = Binary (/=) 'c' 'd'

compare :: Constraint -> Constraint -> Bool
compare a b = apply a == apply b

mylist = [
        Binary (==) 5 5, Binary (==) 5 6,
        Binary (/=) 'c' 'd', Binary (/=) 'c' 'c'
     ]


-- ImpredicativeTypes
data Bi = forall a. Typeable a =>
            Bi
                String -- name der funktion
                -- constraintvergleichfunktion
                (forall b c. (Typeable b, Typeable c) => b -> c -> Bool)
                (a -> a -> Bool) -- originale Funktion

instance Show Bi where show (Bi s _ _) = s

list :: [Bi]
list = [
    mkConstraint "eq"  ( (==) :: Char   -> Char     -> Bool ),
    mkConstraint "neq" ( (/=) :: String -> String   -> Bool ),
    mkConstraint "lt"  (  (<) :: Int    -> Int      -> Bool )
    ]

mkConstraint :: Typeable a => String -> (a -> a -> Bool) -> Bi
mkConstraint s f = Bi s (generalize f) f

a :: Bi
a = Bi "" (generalize eqInt) eqInt
    where eqInt :: (Int -> Int -> Bool)
          eqInt = (==)

-- typeconvert from any type and test
generalize ::
    (Typeable a, Typeable b, Typeable c) =>
    (a -> a -> Bool)
    -> b -> c -> Bool
generalize f b c = case (cast b, cast c) of
        (Just a, Just a') -> f a a'
        _ -> True

-- typeconvert to int and test
ss :: Typeable b =>  (Int -> Int -> Bool) -> b -> b -> Bool
ss f tb tb' = case (cast tb, cast tb') of
            (Just a, Just a') -> f a a'
            _ -> False

data Elem = forall a. (Show a, Typeable a) => Elem a

instance Show Elem where show (Elem a) = show a

ls :: [Elem]
ls = [Elem (5::Int), Elem "sdf", Elem 'c', Elem "s", Elem (6::Int)]


pairs :: [Elem] -> [Bi] -> (Elem -> Elem -> Bi -> a) -> [a]
pairs ls cons f = [f x y c | x <- ls, y <- ls, c <- cons]

main :: IO ()
main = mapM_ print results
    where
        results = pairs ls list f
        f :: Elem -> Elem -> Bi -> String
        f (Elem a) (Elem b) (Bi s c g) = 
            show a ++ " " ++ s ++ " " ++ show b ++ " => " ++ show (c a b)


