
import Solving
import Constraints
import Control.Monad

ls = [0..2]

net = 
    Net
        [
            var "A" ls,
            var "B" [],
            var "C" []
        ]
        []
--

rfn = expandFirstNode net
rfn2 = concatMap expandFirstNode rfn

main = print net
        >> putStrLn "----------"
        >> zipWithM_ (\i n -> print i >> print n) [0..] rfn
        >> putStrLn "----------"
        >> zipWithM_ (\i n -> print i >> print n) [0..] rfn2

