



import Piano
-- HUnit tests here

-- A Test for indexOfTone and toneByIndex. This test maps each tone into their
-- position and back. The tones shouldn't change
test1 = (map (toneByIndex . indexOfTone) toneList) == toneList
test2 = (map (succ . indexOfTone . toneByIndex . pred) [1..numTones]) == [1..numTones]
