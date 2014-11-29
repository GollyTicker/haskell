{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

import ConstraintSolving

nums :: Domain Int
nums = [1,2,3,4]

net :: Net
net = Net
        [
            var "V" nums,
            var "X" nums,
            var "Y" nums,
            var "Z" nums
        ]
        [
           (mkConstraint :: Over Int) "X" (<) "Y" "X < Y"
        ]




