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
        var "Z" ["1","2","3","4"]
    ]
    [
       (mkConstraint :: On Int) "X" (<) "Y" "X < Y",
       (mkConstraint :: Over Int String) "X" ((==) . show) "Z" "str(X) = Z"
    ]




