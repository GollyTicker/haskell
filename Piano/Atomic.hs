
-- basic buildung blocks. makes the twelve tones
module Atomic
                (
                Tone, Tones, zeroTone,
                numTones,
                WKey(..),
                Step, Steps, zeroStep,
                nat, sh, fl, (.:.),
                natural, sharp, flat,
                toneList,
                toneIsNat,
                indexOfTone,
                step, steps,
                Rotate(..),
                RawStep -- for defining FixedStep
                )
            where
import Data.Maybe(fromJust)
import Data.List(elemIndex)
toneList =
            [
                nat C, C .:. D,
                nat D, D .:. E,
                nat E,
                nat F, F .:. G,
                nat G, G .:. A,
                nat A, A .:. B,
                nat B
            ]
validBKey w1 w2 =
        case (w1, w2) of (A,B) -> True
                         (C,D) -> True
                         (D,E) -> True
                         (F,G) -> True
                         (G,A) -> True
                         (_) -> False

numTones = length toneList
zeroTone = head toneList
zeroStep = step 0

type RawStep = Int
data Step = Step RawStep deriving (Eq)
step :: RawStep -> Step
step = Step

instance Show Step where
    show int = "{" ++ (show $ fromEnum int) ++ "}"

instance Enum Step where
    succ = toEnum . succ . fromEnum
    pred = toEnum . pred . fromEnum
    toEnum = step
    fromEnum (Step n) = n

instance Num Step where
    a + b = toEnum (fromEnum a + fromEnum b)
    a * b = toEnum (fromEnum a * fromEnum b)
    negate = toEnum . negate . fromEnum
    abs = toEnum . abs . fromEnum
    signum = toEnum . signum . fromEnum
    fromInteger = toEnum . fromInteger

type Steps = [Step]
-- Steps are relative. f.e. see minor/major scale.
-- each Step in the Steps start where the previous
-- Step left off

-- utility funciton to map a complete list
steps :: [RawStep] -> Steps
steps = map (step)

-- Rotate is a class of types which can be rotated
-- by an Step
class Rotate a where
    rotate :: a -> Step -> a

instance Rotate Step where
    rotate = (+)

data Tone = WKey WKey | MakeBKey BKey
type Tones = [Tone]

instance Show Tone where
    show (WKey w) = show w
    show (MakeBKey (BKey (l,r))) = "#" ++ show l -- ++ "&" ++ "b" ++ show r

-- hidden helper function
-- given a function which maps the Ints as wanted, it does the same mapping
-- on the Tones but takes care of their cyclic nature.
instance Rotate Tone where
    rotate tone n = toneByIndex $ rotateIndex $ indexOfTone tone
        where
            rotateIndex m = ((fromEnum n) + m) `mod` numTones

instance Eq Tone where
    WKey w1 == WKey w2 = (w1 == w2)
    MakeBKey (BKey (l1, r1)) == MakeBKey (BKey (l2, r2)) = (l1 == l2) && (r1 == r2)
    _ == _ = False

instance Enum Tone where
    succ t = rotate t $ succ zeroStep
    pred t = rotate t $ pred zeroStep
    toEnum = (rotate zeroTone) . step
    fromEnum = indexOfTone

data WKey = A | B | C | D | E | F | G deriving (Show, Eq)

-- hidden helper function/data type
data BKey = BKey (WKey, WKey) deriving (Show, Eq)


-- sharp of a white key
sharp :: WKey -> Tone
sharp = succ . nat
sh :: WKey -> Tone
sh = sharp

-- flat of a white key
flat :: WKey -> Tone
flat = pred . nat
fl :: WKey -> Tone
fl = flat

-- natural of a white key
natural :: WKey -> Tone
natural a = WKey a
nat :: WKey -> Tone
nat = natural

toneIsNat :: Tone -> Bool
toneIsNat (WKey _) = True
toneIsNat _ = False


-- write a black key as the one between two white keys
(.:.) :: WKey -> WKey -> Tone
w1 .:. w2
    | validBKey w1 w2 = MakeBKey $ BKey (w1, w2)
    | otherwise = invalidBlackKeyError w1 w2

-- invalidBlackKeyError :: WKey -> WKey -> (abcdef) -- lol, ghc doesn't complain when I wrtie this!
invalidBlackKeyError :: WKey -> WKey -> Tone
invalidBlackKeyError w1 w2 = error ("Black key cannot be constructed between " ++ show w1 ++ " and " ++ show w2 ++ ".")

-- try this out with some (maybe negative) numbers
demoRotate :: Step -> Tones
demoRotate n = map (`rotate` n) toneList
-- the `...` allows to curryingly ommit
-- the first argument. usually ommiting
-- only works for the last argument

-- hidden helper function
toneByIndex :: Int -> Tone
toneByIndex = (!!) toneList

-- hidden helper function
-- given a tone it returns its position in the toneList
indexOfTone :: Tone -> Int
indexOfTone t = fromJust (elemIndex t toneList)
--length $ takeWhile (/=t) toneList