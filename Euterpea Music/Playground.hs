
import MyEuterpea

t251' :: Music Pitch
t251' = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
            gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
            cMajor = c 4 wn :=: e 4 wn :=: g 4 wn
        in  dMinor :+: gMajor :+: cMajor
;

-- play :: Performable a => Music a -> IO ()

main = play t251'

-- http://www.cs.yale.edu/homes/hudak/Papers/HSoM.pdf

