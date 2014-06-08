
-- builds sequences above notes
module Sequence
            (
            Sequence,
            build, stepwise, -- aliases
            seq, attach, -- aliases
            w, h, z,
            Fixedstep, Fixedsteps,
            fixedStep, fixedSteps,
            fix,
            fixed2dynamic, dynamic2fixed
            )
        where

import Atomic
import Note
import Prelude hiding (seq) -- override seq.

w = step 2
h = step 1
z = step 0

-- as the name suggests, every Fixedstep
-- in Fixedsteps starts at the same position
-- this means the minor/major scale would then be:
-- *Piano> fix major
-- [0,2,4,5,7,9,11,12]
type RawFixedStep = RawStep
data Fixedstep = Fixedstep RawFixedStep deriving (Eq)
type Fixedsteps = [Fixedstep]

fixedStep :: RawFixedStep -> Fixedstep
fixedStep = Fixedstep

fixedSteps :: [RawFixedStep] -> Fixedsteps
fixedSteps = map (fixedStep)

instance Show Fixedstep where
    show fxdint = "<" ++ (show $ fromEnum fxdint) ++ ">"

instance Enum Fixedstep where
    succ = toEnum . succ . fromEnum
    pred = toEnum . pred . fromEnum
    toEnum = fixedStep
    fromEnum (Fixedstep n) = n

instance Num Fixedstep where
    a + b = toEnum (fromEnum a + fromEnum b)
    a * b = toEnum (fromEnum a * fromEnum b)
    negate = toEnum . negate . fromEnum
    abs = toEnum . abs . fromEnum
    signum = toEnum . signum . fromEnum
    fromInteger = toEnum . fromInteger

fixed2dynamic :: Fixedstep -> Step
fixed2dynamic = toEnum . fromEnum

dynamic2fixed :: Step -> Fixedstep
dynamic2fixed = toEnum . fromEnum

fix :: Steps -> Fixedsteps
fix ls = map (dynamic2fixed) $ scanSum ls

scanSum :: Steps -> Steps
scanSum = scanl (+) zeroStep

type Sequence = Notes
stepwise :: Note -> Steps -> Sequence
stepwise note = (map $ rotate note) . scanSum
build :: Note -> Steps -> Sequence
build = stepwise

seq :: Note -> Fixedsteps -> Sequence
seq note ls = (map (rotate note)) $ map (fixed2dynamic) ls
attach :: Note -> Fixedsteps -> Sequence
attach = seq