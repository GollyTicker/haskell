{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}

module MyEuterpea (
         module Euterpea
        ,module Euterpea.Music.Note.Music
        ,module Data.Ratio
        
        , zipApply
    )
    where

import Euterpea
import Euterpea.Music.Note.Music
import Data.Ratio
import Data.Foldable
import Data.Monoid

zipApply :: [a] -> [a -> b] -> [b]
zipApply rt xs = zipWith ($) xs rt

-- gesucht:
-- eine abstraction die über die gleiche Datenstruktur
-- zwei verschiedene Monoiden mit ihärenter
-- Parallelität und Sequentialität enthält.
-- dadruch könnte ein folgender fold über Music
-- realisiert werden.
-- bifoldMap (:+:) (:=:) id    =    id
--                      -- id für Prim
instance Functor Primitive where
    fmap f (Note d a) = Note d (f a)

instance Functor Music where
    fmap :: forall a b. (a -> b) -> Music a -> Music b
    fmap f m =
        let go :: forall f. Functor f => f a -> f b -- general type needs to be explicit
            go = fmap f                             -- otherwise GHC infers go :: Music a -> Music b
        in  case m of                               -- which fails for the inner fmap in case Prim
             (Prim p) -> Prim (go p)
             (ma :+: mb) -> go ma :+: go mb
             (ma :=: mb) -> go ma :=: go mb
             (Modify ctrl ma) -> Modify ctrl (go ma)

instance Foldable Music where
    -- f :: Monoid m => a -> m
    -- m :: Music a
    -- res :: m
    foldMap f m =
        let go = foldMap f
        in  case m of
             (Prim (Rest dur)) -> mempty
             (Prim (Note dur a)) -> f a
             (ma :+: mb) -> go ma `mappend` go mb
             (ma :=: mb) -> go ma `mappend` go mb
             (Modify ctrl ma) -> go ma
;
-- http://hackage.haskell.org/package/base-4.7.0.2/docs/Data-Traversable.html
paired :: (a -> a -> b) -> [a] -> [b]
paired f xs = zipWith f xs (tail xs)

-- get the difference inbetween consecutive notes in the piece of music
melodicSteps :: Music Pitch -> [Int]
melodicSteps m = undefined -- how to get all absPitches and execute a paired subtract on them?

{-
Das hier spart man sich wegen Foldable! :D

getPitch :: Music a -> a
getPitch (Prim (Note _ pitch)) = pitch

getAbsPitch :: Music Pitch -> AbsPitch
getAbsPitch = absPitch . getPitch
-}


