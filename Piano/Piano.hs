

module Piano (
            Tone, Tones,
            Note, Notes,
            WKey(..),
            Step, Steps,
            step, steps,
            Rotate(..),
            Sequence,
            nat, sh, fl, (%), (.:.),
            isNat, isMid,
            natural, sharp, flat, note,   -- alises
            build, stepwise, -- both do the same
            seq, attach, -- both do the same
            numTones,
            twelveTones, twelveNotes,
            zeroTone, zeroStep, zeroRepetition, zeroNote,
            lowerRepetition, higherRepetition,
            baseFive,
            lowerC, zeroBassNote, middleC, zeroViolinNote, higherC,
            minor, major,
            w, h, z,
            Fixedstep, Fixedsteps,
            fixedStep, fixedSteps,
            fix,
            fixed2dynamic, dynamic2fixed,
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
import Scale
import GivenStyle
import Debug.Trace
import Prelude hiding (seq, sequence)
{-
DO-able:
< Progress Difficulty Despcription >
*----   *---- MyTemplateDescription
*----   ****- HUnit Tests!!!!!
*----   ****- HUnit Tests!!!!! rotate seems to be fixed now... i dont know. i got not tests.....
*----   ****- HUnit Tests!!!!!
*----   ****- HUnit Tests!!!!!
-----   *---- calculate Frequiencies for tones of a specific Repetition
*****   *---- build minor and major Steps
*----   ***-- build minor and major scales so that Steps can be played on a mask
-----   **--- build interals not only based on C (orgin of major) and A (for minor)
                but also any other combination
*****   **--- build a "Sequence" as a number of stepic movements
                f.e. the major would be WWhWWWh
-----   ----- maybe rename rotate operations into shift operations
-----   *---- maybe those shifts simply into "+" ? I mean conditions for Num typeclass are satisfied
*****   ****-- make typeclass for rotateable things (to sum up all rotateMeBy functions)
*****   **--- rotation for Steps?
-----   ***** make the tones WKeys and BKeys derived instead of hardcoded
*----   **--- make documentation of the public function
-----   ******** Make styles like
                 western :: Style
                 data Style = Style Piano Choice
                    A style consists of a logically derivable part (from the
                    axioms) and a choice part which is a projection of the derivation
-----   ****** data Piano = Piano Axioms Derivation
*****   ***-- forbid implicit conversion inbetween fixed and normal Steps
-----   *---- turn a sequence back into its Steps
-----   **--- turn Repetition into a "data" instead of "type". an Repetition shoudl take care of itself to
                calculate its successors and to rotate itselfs. this'll be expecially useful for later
                cases where an Repetition might not always be an Step of 8 Whtie Keys of 12 semi-tones.
-----   ***-- hide WKey to outside again. Make Note the most atomic piece towards outside. this also allows
                to use Tone::Enum inside this implementation without borthering the users with it.
                also remove demorotate then. this also allows for better syntax: "G % 5" instead of "nat G % 5"
-}


twelveTones = toneList

-- play :: Rythim x Sequence -> Play

