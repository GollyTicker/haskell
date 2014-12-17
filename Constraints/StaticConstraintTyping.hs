{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, ExistentialQuantification, DeriveDataTypeable #-}

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

class HasInt a where
    int :: a -> Maybe Int

instance HasInt Int where
    int = Just

instance (HasInt a) => HasInt [a] where
    int = foldr (const . int) Nothing


test1 = int (3 :: Int)
test2 = int ([] :: [Int])
test3 = int ([1,2,3] :: [Int])

main = mapM_ print [test1, test2, test3]



