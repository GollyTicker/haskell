{-# LANGUAGE ScopedTypeVariables,  FlexibleContexts, FlexibleInstances, ConstraintKinds, MultiParamTypeClasses, ExistentialQuantification, DeriveDataTypeable #-}

import Data.Typeable

-- data Net c = Net [Node c] deriving Show
data Net c where
    Nil :: Net c
    (+:) :: Net c -> Node c' -> (c,c')

type Elem a = (Show a, Eq a, Typeable a)

-- c stands for context
data Node c =
    forall a. (Has c a, Elem a) => 
    Node String a

instance Show (Node c) where show (Node s a) = "Node " ++ show s ++ show a

data Animal = A | B deriving (Show, Eq, Typeable)

data Void = Void !Void deriving (Show, Eq, Typeable)
{-
net :: forall c0 c1. Has c1 Animal => Net (c0,c1)
net = n1
    where
        n0 = Net []
        n1 = n0 +: (Node "eins" A)
-}
--(+:) :: Net c -> Node c' -> Net (c,c')
--(Net ns) +: n = Net (n:ns)

class Has c a where
    has :: c -> Maybe a

instance Has Int Int where
    has = Just

instance (Has c a) => Has [c] a where
    has = foldr (const . has) Nothing

instance (Has c a) => Has (c,b) a where
    has = has . fst

--instance (Has c a) => Has (b,c) a where
--    has = has . snd

hasAnimal :: Has ctx Animal => ctx -> Maybe Animal
hasAnimal = has

hasInt :: Has c Int => c -> Maybe Int
hasInt = has

test1 = hasInt (3 :: Int)
test2 = hasInt ([] :: [Int])
test3 = hasInt ([1,2,3] :: [Int])

main = mapM_ print [test1, test2, test3]



