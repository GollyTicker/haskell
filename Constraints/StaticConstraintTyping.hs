{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, FlexibleInstances, ConstraintKinds, MultiParamTypeClasses, ExistentialQuantification, DeriveDataTypeable #-}

import Data.Typeable

data Net = Net [Node] deriving Show

type Elem a = (Show a, Eq a, Typeable a)

data Node =
    forall a. (Elem a) => 
    Node String a

instance Show Node where show (Node s a) = "Node " ++ show s ++ show a

data Animal = A | B deriving (Show, Eq, Typeable)

net = Net
    [
        Node "eins" A,
        Node "zwei" (2::Int),
        Node "drei" (B,'B'),
        Node "eins" B,
        Node "zwei" (undefined::Int)
    ]

class Has c a where
    has :: c -> Maybe a

instance Has Int Int where
    has = Just

instance (Has c a) => Has [c] a where
    has = foldr (const . has) Nothing


hasInt :: Has c Int => c -> Maybe Int
hasInt = has
    
test1 = hasInt (3 :: Int)
test2 = hasInt ([] :: [Int])
test3 = hasInt ([1,2,3] :: [Int])

main = mapM_ print [test1, test2, test3]



