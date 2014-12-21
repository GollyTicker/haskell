{-# LANGUAGE MultiParamTypeClasses #-}

module Has (
        Has, has, Collect, collect
    )
    where

class Has c a where
    has :: c -> Maybe a

class Collect c a where
    collect :: c -> [a]

