
import MyEuterpea

main = sas
-- schiff am strand (selbst komponiert)

tn, twn, nn, dnn :: Dur
tn = 1 % 3
-- qn = 1 % 4
dnn = 1 % 6 -- qn - tn
-- en = 1 % 8
nn = 1 % 9 -- 1 % 6 = 1.5 * xn
twn = 1 % 12 -- footnote *1
-- sn = 1 % 16

-- rythm = [qn, sn, sn, sn, qn, den, sn, qn, den, sn, qn, en]

-- two beats long
rythmSas = [qn, twn, twn, twn, qn, dnn, twn, qn, dnn, twn, qn, qn]

duration = sum rythmSas -- 2 % 1

lead = line $ zipApply rythmSas [
        d 4,
        a 4,  a 4, a 4, a 4,
        a 4, bf 4, a 4,
        g 4, f  4, g 4,
        f 4
       ]
firstChore = line $ zipApply rythmSas [
        d 4,
        a 4,  a 4, a 4, a 4,
        a 4,  a 4, d 5,
        c 5, bf 4, c 5,
        bf 4
       ]
;
sasComposition = lead :+: firstChore

ins = Clarinet
sas = play
        $ tempo 1.15
        $ instrument ins
        $ sasComposition
;

-- footnote *1
-- to fill a qn with a triplet
-- we need xn with qn = 3 * xn
-- this yields xn = qn/3 = 1 % 12
-- xn is therefore called tn (twelveth note)