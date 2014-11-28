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
                (forall b. Typeable b => a -> b -> Bool)
                (a -> a -> Bool) -- originale Funktion

instance Show Bi where show (Bi s _ _) = s

list :: [Bi]
list = [
    constraint "eq"  ( (==) :: Char   -> Char     -> Bool ),
    constraint "neq" ( (/=) :: String -> String   -> Bool ),
    constraint "lt"  (  (<) :: Int    -> Int      -> Bool )
    ]

constraint :: Typeable a => String -> (a -> a -> Bool) -> Bi
constraint s f = Bi s (make f) f

a :: Bi
a = Bi "" (make eqInt) eqInt
    where
        eqInt :: (Int -> Int -> Bool)
        eqInt = (==)

-- typeconvert from any type and test
make :: (Typeable a, Typeable b) =>  (a -> a -> Bool) -> a -> b -> Bool
make f a tb = maybe False (\a' -> f a a') (cast tb)


-- typeconvert to int and test
ss :: Typeable b =>  (Int -> Int -> Bool) -> b -> b -> Bool
ss f tb tb' = case (cast tb, cast tb') of
            (Just a, Just a') -> f a a'
            _ -> False

data Elem = forall a. (Show a, Typeable a) => Elem a
instance Show Elem where
    show (Elem a) = show a

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
            printf "%s %s %s => %s" (show a) s (show b) (c a b)

-- next step:
-- make this last call in main work.
-- currently only the type b in Bi(...) is universally quantified.
-- Now I have to make sure, the type a is also universally quantified
-- but still bound to the original function in Bi(...)
-- With this, I can simply extract the comparing funciton from Bi(...)

