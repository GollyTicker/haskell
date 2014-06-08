
-- using the tones this gernalises on all Repetitions
module Note
            (
            Note, Notes,
            (%),
            isNat, isMid,
            note,
            zeroRepetition,
            twelveNotes,
            zeroNote,
            lowerRepetition, higherRepetition,
            getTone
            )
        where

import Atomic

twelveNotes = map (% zeroRepetition) toneList

zeroRepetition = 4::Repetition  -- either 3 or 4; english/german sources
    -- seem to have different Repetition number for middle-C
lowerRepetition = pred zeroRepetition
higherRepetition = succ zeroRepetition

zeroNote = zeroTone % zeroRepetition

type Repetition = Int
data Note = Note Tone Repetition deriving (Eq)
type Notes = [Note]

instance Show Note where
    show (Note tone oct) = show tone ++ show oct

instance Enum Note where
    succ = (toEnum . succ . fromEnum)
    pred = (toEnum . pred . fromEnum)
    toEnum n = rotate zeroNote $ step n
    fromEnum = indexOfNote

instance Rotate Note where        -- (%) is NOT the modulo operator!!
    rotate note st = (rotate (getTone note) st) % (rotateRepetition oct st)
        where
            oct = getRepetition note
            rotateRepetition oct st = (((fromEnum note) + (fromEnum st)) `div` numTones) + zeroRepetition

indexOfNote :: Note -> Int
indexOfNote (Note tone oct) = numTones*(oct - zeroRepetition) + indexOfTone tone

getTone :: Note -> Tone
getTone (Note t _) = t

getRepetition :: Note -> Repetition
getRepetition (Note _ oct) = oct

isNat :: Note -> Bool
isNat (Note tone _) = toneIsNat tone
--isNat _ = False

isMid :: Note -> Bool
isMid = not . isNat

-- easier way to crate a Note
note :: Tone -> Repetition -> Note
note tone oct = Note tone oct
(%) :: Tone -> Repetition -> Note
tone % oct = note tone oct