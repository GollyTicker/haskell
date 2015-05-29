
import HList

a = Nil

b = True +: a

c = False +: 5 +: b

d = 
    True
    +: (5, "hello", ())
    +: 'e'
    +: False
    +: True
    +: 'f'
    +: (3.14, True)
    +: Nil

main = do
    print a
    print b
    print c
    print (collect d :: [Bool])
    print (collect d :: [Char])
    print (get d :: Bool)
    print (get c :: Bool)
    print (get b :: Bool)
--  print (get a :: Bool) -- won't compile


