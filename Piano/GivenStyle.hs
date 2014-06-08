
-- using notes and sequences this file
-- tries to make a style. usually this is just application
-- of all the funciton
module GivenStyle
            (
            baseFive,
            lowerC, zeroBassNote, middleC, zeroViolinNote, higherC,
            major, minor
            )
        where

import Atomic
import Note
import Sequence
import Scale
import Prelude hiding (seq, sequence)

higherC = nat C % higherRepetition
zeroViolinNote = nat G % zeroRepetition
intermedE = nat E % zeroRepetition
middleC = nat C % zeroRepetition
intermedA = nat A % lowerRepetition
zeroBassNote = nat F % lowerRepetition
lowerC = nat C % lowerRepetition

-- dur besteht aus WWhWWWh
major = scaleC

-- moll besteht aus WhWWhWW
minor = scaleA

baseFive = [lowerC, zeroBassNote, middleC, zeroViolinNote, higherC]

-- we can insert some simple melodies here soon