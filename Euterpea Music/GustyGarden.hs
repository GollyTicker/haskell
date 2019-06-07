
import MyEuterpea hiding (tn)
import Data.Ratio
import Data.Monoid
import Control.Applicative

main = gg

gg = play
        $ tempo 1.14
        $ instrument AcousticGrandPiano
        $ gustygarden
;

gustygarden :: Music Pitch
gustygarden = 
    let xs = zipApply ggR ggM 
    in  line xs
;

hen :: Dur -- Duration
hen = hn + en

leadR, zweiR :: [Dur] -- type Dur = Ratio from Data.Ratio
leadR = [hen, en, en, en]
zweiR = [dqn, hen]

takt4R = [dqn, dqn, qn]
takt6R = [dqn, qn, qn, sn, sn]
takt15 = [dqn, qn, qn, en]
takt16 = [dqn, hn, en]

takt1bis4R :: [Dur] -- rythmus der ersten vier Takte
takt1bis4R = leadR ++ leadR ++ zweiR ++ takt4R
takt5bis9R = leadR ++ takt6R ++ zweiR ++ [wn]
takt10bis13R = leadR ++ zweiR ++ leadR ++ zweiR
takt14bis17R = leadR ++ takt15 ++ takt16 ++ zweiR

-- ggR = gusty Garden Rythm
ggR :: [Dur]
ggR = concat [
        takt1bis4R,
        takt5bis9R,
        takt10bis13R,
        takt14bis17R
      ]
;
-- ggM = gusty garden melody
ggM :: [Dur -> Music Pitch]
ggM = concat [
        takt1bis4,
        takt5bis9,
        takt10bis13,
        takt14bis17
      ]
;
takt1bis4 :: [Dur -> Music Pitch]
takt1bis4 = [
              (\dr -> f 4 dr :=: af 4 dr :=: df 5 dr) -- Takt 1
            , c 5
            , ef 5
            , df 5
            
            , af 4               -- Takt 2
            , f 4
            , gf 4
            , af 4
            
            , af 4               -- Takt 3
            , gf 4
            
            , f 4                -- Takt 4
            , ef 4
            , df 4
           ]
--
takt5bis9 = [
              (\dr -> gf 4 dr :=: bff 4 dr :=: ef 5 dr) -- Takt 5
            
            , d 5               -- Takt 6
            , f 5
            , ef 5
            
            , df 5              -- Takt 7
            , c 5
            , bf 4
            , bf 4
            , c 5
            
            , bf 4              -- Takt 8
            , af 4
            
            , rest              -- Takt 9
            ]
;

two = (:=:)
three = \x y z -> x :=: y :=: z
four = \x y z w -> x :=: y :=: z :=: w

takt10bis13 = [
              three <$> af 4 <*> ef 5 <*> af 5   -- 10
            , two <$> ef 5 <*> g 5
            , two <$> ef 5 <*> bf 5
            , two <$> ef 5 <*> af 5
            
            , two <$> bf 4 <*> gf 5             -- 11
            , two <$> bf 4 <*> f 5
            
            , {- four <$> bf 4 <*> ef 5<*> f 5 <*> -} gf 5  -- 12
            , f 5
            , af 5
            , gf 5
            
            , f 5
            , {- three <$> g 4 <*> af 4 <*> -} ef 5 -- 13
            ]

takt14bis17 = [
              {- four <$> af 4 <*> df 5 <*> ef 5 <*> -} f 5   -- 14
            , ef 5
            , gf 5
            , f 5
            
            , ef 5                              -- 15
            , d 5
            , f 5
            , ef 5
            
            , df 5                              -- 16
            , c 5
            , a 4
            
            , two <$> a 4 <*> cs 5              -- 17
            , three <$> a 4 <*> cs 5 <*> ds 5
            ]
;

