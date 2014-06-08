
-- this allows to shift inbetween different scales
-- without changing anything
module Scale
            (
            Scale,
            getSteps, getTone,
            constructScale, ladder,
            scaleA, scaleC, scales,
            Interval, interval, int
            )
        where

import Atomic
import Note
import Sequence
import Debug.Trace
import Prelude hiding (seq, sequence)

data Interval = Interval Int deriving (Eq)
interval :: Int -> Interval
interval = Interval
int :: Int -> Interval
int = interval

instance Show Interval where
    show n = "int " ++ show (fromEnum n) ++ ""

instance Enum Interval where
    fromEnum (Interval n) = n
    toEnum = int
    succ = toEnum . succ . fromEnum
    pred = toEnum . pred . fromEnum

instance Rotate Interval where
    rotate interval step = toEnum $ (fromEnum interval) + (fromEnum step)


-- data Scale = Scale Tone Steps deriving (Eq) --Enum?

data Scale = Scale {
                    baseTone :: Tone,
                    getSteps :: Steps
                    }
                    deriving (Eq)

instance Show Scale where
    show scale = "Scale " ++ bTone ++ " +=> " ++ steps
        where
            bTone = show $ baseTone scale
            steps = show $ getSteps scale

constructScale :: Tone -> Scale
constructScale tone = Scale tone $ makeScale start initStep fin initSteps
        where
            note = tone % zeroRepetition -- any Repetition could have been used here
            start = (succ note)
            initStep = h  -- step 1
            initSteps = []
            fin = (rotate note $ step numTones)

-- hidden Helper Function
makeScale :: Note -> Step -> Note -> Steps -> Steps
makeScale curr stepToCurr last accuSteps
        | isMid last    = error "< not sure how not-natural scales should be derived.... >"
            -- Scales with black keys as base?!?
        | curr == last  = accuSteps'
        | isNat curr    = makeScale next h last accuSteps'
        | otherwise     = makeScale next stepToNext last accuSteps
            where
                stepToNext = succ stepToCurr
                next = succ curr
                accuSteps' = accuSteps ++ [stepToCurr]
-- trace (show stepToCurr) $ 

ladder :: Note -> Scale -> Sequence
ladder note scale = stepwise note $ getSteps scale

--onScale :: Scale -> Note -> Interval -> Note

scaleC = constructScale $ nat C

scaleA = constructScale $ nat A

scales = [ constructScale x | x <- toneList, isNat $ x % zeroRepetition]

-- TIL: moll and dur are just different permutations of a general group
