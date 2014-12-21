{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module HList (
        Nil(..),
        hHead, hTail,
        (+:),
        hnull,
        Has, HList
    )
    where

import Data.Void
import Data.Maybe

data Cons h t = Cons { cHead :: h, cTail :: t } deriving (Show, Read)

data Nil = Nil deriving (Show, Read)

class HList l h t | l -> h t where
    inspect :: l -> Maybe (Cons h t)

instance HList Nil Void Void where
    inspect = const Nothing

instance HList (Cons a b) a b where
    inspect = Just

hHead :: HList l h t => l -> Maybe h
hHead = fmap cHead . inspect

hTail :: HList l h t => l -> Maybe t
hTail = fmap cTail . inspect

hHead2 :: HList l h t => l -> h
hHead2 = fromJust . hHead

hTail2 :: HList l h t => l -> t
hTail2 = fromJust . hTail

(+:) :: HList l h t => a -> l -> Cons a l
(+:) = Cons

hnull :: HList l h t => l -> Bool
hnull = isJust . inspect

class Has c a where
    has :: c -> a

instance Has a a where
    has = id

instance Has (Cons a b) a where
    has = hHead2

instance Has c a => Has (Cons b c) a where
    has = has . hTail2

