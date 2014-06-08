
Load to begin.
Prelude> :m + Piano

A White Key is A | B | C | D | E | F | G
A tone is a natural, sharpend or flattend White Key

Prelude Piano> nat A
A
Prelude Piano> nat B
B
Prelude Piano> nat G
G
Prelude Piano> sh A
#A
Prelude Piano> fl G
#F
Prelude Piano> sh E
F

Tones of black keys can also be notated with the (.:.) operator.
X .:. Y notates the black key between the white X and Y keys.

Prelude Piano> G .:. A
=> #G

Prelude Piano> D .:. E
=> #D

Prelude Piano> E .:. F
=> *** Exception: Black key cannot be constructed between E and F.

Tone is a member of Show, Eq ...

Prelude Piano> show (D .:. E)
"#D"
Prelude Piano> (sh D) == (D .:. E)
True

... and Enum.
Prelude Piano> succ $ nat D
#D

Prelude Piano> pred $ G .:. A
G

However because of the cyclic nature of tone
ranges should NOT be used.

Prelude Piano> [sh F .. sh F]
[#F]

Prelude Piano> [nat A .. nat B]
[A,#A,B]

Prelude Piano> [fl D .. nat E]
[#C,D,#D,E]

Prelude Piano> [nat D .. nat A]
[D,#D,E,F,#F,G,#G,A]

Prelude Piano> [nat A .. nat D]
[]

This is because the scale begins at nat C and ends at nat B.
Any negative range or any range crossing this border becomes null.

A Note is a tone of a specified octave.
Notes can be made with note or the (%) operator.

Prelude Piano> nat G % 5
G5

Prelude Piano> note (nat G) 5
G5

Prelude Piano> middleC
C4

Note is a member of Show, Eq...

Prelude Piano> middleC == nat C % 4
True
(to be accurate, I should have used "zeroOctave" instead of 4)

.. and Enum.
Prelude Piano> [nat A % 2..nat F % 3]
[A2,#A2,B2,C3,#C3,D3,#D3,E3,F3]

Prelude Piano> concatMap (show . (\note -> rotateNoteBy note 4)) [nat C % 4..nat G % 4]
"E4F4#F4G4#G4A4#A4B4"

Intervals are the digital representations of "intervallic reading".
f.e. the major is defined as:
major = [w,w,h,w,w,w,h]
where w is "second" and h is a "first"
Currently, intervals are just numbers.
F.e. a third is just 3. (yet...)
What does major look like?
Prelude Piano> major
[2,2,1,2,2,2,1]

The minor is also implemented

Each Sequence is made by playing Intervals from a starting Note.
seq is our function to make sequences.
seq :: Note -> Intervals -> Sequence

(seqeuences don't have any relationship to time.
I mean, they cannot be played just like that.)

Prelude Piano> seq middleC major
[C4,D4,E4,F4,G4,A4,B4,C5]

you'll get a  name abiguiity between the Prelude.seq and Piano.seq. You can either
specify Piano.seq or use (build) instead of (seq)

Prelude Piano> build (nat D % 3) major
[D3,E3,#F3,G3,A3,B3,#C3,D4]

Prelude Piano> seq middleC $ major ++ major
[C4,D4,E4,F4,G4,A4,B4,C5,D5,E5,F5,G5,A5,B5,C6]

Playing the major scale of the bass F, middle C and violin G
Prelude Piano> concatMap (`seq` major) [zeroBassNote, middleC, zeroViolinNote]
[F3,G3,A3,#A3,C3,D3,E3,F4,C4,D4,E4,F4,G4,A4,B4,C5,G4,A4,B4,C4,D4,E4,#F4,G5]

do what you want to. (WIP)
Prelude Piano> build middleC [w,h,-w,-h,-h,z,w+w+h]
[C4,D4,#D4,#C4,C4,B3,B3,E4]

Prelude Piano> build middleC [12,13,-22,-2,7]
[C4,C5,#C6,#D4,#C4,#G4]



Intervals:
*Piano.Atomic> step 5 + step 6
Interval 11
*Piano.Atomic> (step 5 - step 6)
Interval (-1)

*Piano> :t constructScale
constructScale :: Tone -> Scale

*Piano> let scaleB = constructScale  $ nat B

*Piano> scaleB
Scale &B +=> [{1},{2},{2},{1},{2},{2},{2}]

*Piano> stepwise middleC $ getSteps scaleB
[C4,#C4,#D4,F4,#F4,#G4,#A4,C5]

*Piano> stepwise middleC $ getSteps scaleB
[C4,#C4,#D4,F4,#F4,#G4,#A4,C5]

*Piano> stepwise (nat B % 4) $ getSteps scaleB
[B4,C5,D5,E5,F5,G5,A5,B5]

*Piano> stepwise (sh B % 4) $ getSteps scaleB
[C4,#C4,#D4,F4,#F4,#G4,#A4,C5]

*Piano> stepwise (fl B % 4) $ getSteps scaleB
[#A4,B4,#C5,#D5,E5,#F5,#G5,#A5]

*Piano> stepwise (nat A % 4) $ getSteps scaleB
[A4,#A4,C5,D5,#D5,F5,G5,A5]


*Piano> scales !! 0
Scale C +=> [{2},{2},{1},{2},{2},{2},{1}]
*Piano> scales !! 1
Scale D +=> [{2},{1},{2},{2},{2},{1},{2}]
*Piano> scales !! 2
Scale E +=> [{1},{2},{2},{2},{1},{2},{2}]
*Piano> scales !! 2
Scale E +=> [{1},{2},{2},{2},{1},{2},{2}]
*Piano> scales !! 3
Scale F +=> [{2},{2},{2},{1},{2},{2},{1}]
*Piano> scales !! 4
Scale G +=> [{2},{2},{1},{2},{2},{1},{2}]
*Piano> scales !! 5
Scale A +=> [{2},{1},{2},{2},{1},{2},{2}]
*Piano> scales !! 0
Scale C +=> [{2},{2},{1},{2},{2},{2},{1}]
*Piano> scales !! 1
Scale D +=> [{2},{1},{2},{2},{2},{1},{2}]
*Piano> scales !! 2
Scale E +=> [{1},{2},{2},{2},{1},{2},{2}]
*Piano> scales !! 3
Scale F +=> [{2},{2},{2},{1},{2},{2},{1}]
*Piano> scales !! 4
Scale G +=> [{2},{2},{1},{2},{2},{1},{2}]
*Piano> scales !! 5
Scale A +=> [{2},{1},{2},{2},{1},{2},{2}]
*Piano> scales !! 6
Scale B +=> [{1},{2},{2},{1},{2},{2},{2}]
