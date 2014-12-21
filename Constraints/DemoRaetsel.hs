
import Constraints

net :: Net Int
net = Net
        [
            var "V" [1,2,3,4],
            var "X" [1,2,3,4],
            var "Y" [1,2,3,4],
            var "Z" [1,2,3,4]
        ]
        [
            mkConstraint "X" (==) "V"               "X = V",
            mkConstraint "X" (\x z -> x*2 == z) "Z" "X *2 = Z",
            mkConstraint "X" (<) "Y"                "X < Y",
            mkConstraint "Y" (==) "Z"               "Y = Z"
        ]


-- main = ac3 net
main = print $ solve net

