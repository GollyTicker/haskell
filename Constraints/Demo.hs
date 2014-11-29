
nums :: Domain Int
nums = [1,2,3,4]

net :: Net
net = Net
        [
            Node "V" nums,
            Node "X" nums,
            Node "Y" nums,
            Node "Z" nums
        ]
        [
           (mkConstraint :: Over Int) "X" (<) "Y" "X < Y"
        ]




