
-- TODO: make it work over thhe Has type class

import HList

a = Nil
b = True +: a

c = 5 +: b

data A a = A a deriving Show

main = print a >> print b >> print c
