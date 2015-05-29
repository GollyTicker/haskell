{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, IncoherentInstances, FlexibleInstances #-}

module HList (
        Nil(..), Cons,
        hHead, hTail,
        hHead2, hTail2,
        (+:),
        hnull,
        HList,
        get, HList.collect
    )
    where

import Data.Void
import Data.Maybe

import Has as H

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

get :: (HList l h t, Has l a) => l -> a
get xs = case has xs of
            Nothing -> error "Not found."
            Just x -> x

collect :: (HList l h t, Collect l a) => l -> [a]
collect = H.collect

infixr 7 +:
(+:) :: HList l h t => a -> l -> Cons a l
(+:) = Cons

hnull :: HList l h t => l -> Bool
hnull = isJust . inspect

instance H.Has a a where
    has = Just

instance H.Has (Cons a b) a where
    has = hHead

instance H.Has c a => H.Has (Cons b c) a where
    has = \x -> hTail x >>= H.has

instance H.Collect a a where
    collect = \x -> [x]

instance H.Collect t a => H.Collect (Cons a t) a where
    collect c = hHead2 c : H.collect (hTail2 c)

instance H.Collect t a => H.Collect (Cons b t) a where
    collect c = H.collect (hTail2 c)

instance H.Collect Nil a where
    collect = const []

