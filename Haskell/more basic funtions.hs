modulus :: Floating a => (a, a) -> a
modulus (x,y) = sqrt (x^2 + y^2)

complAdd (a,b) (c,d) = ((a+c),(b+d))
complSub (a,b) (c,d) = ((a-c),(b-d))
complMul (a,b) (c,d) = ((a*c)-(b*d), (a*d)+(c*b))

ackerman (m,n)
   | m==0  =  n+1
   | m>0 && n==0  =  ackerman(m-1,1)
   | m>0 && n>0  =  ackerman(m-1,ackerman(m,n-1))
-- ne vikai s poveche ot m>3 !

distance (a,b) (c,d) = sqrt ((c-a)^2 + (d-b)^2)

--myreplicate :: Integral a => a -> b -> [b]
myreplicate 0 _ = []
myreplicate a b = b : (myreplicate (a-1) b)

mytake _ [] = []
mytake 0 _ = []
mytake a (x:xs) = x : (mytake (a-1) xs) 

prime 1 = False
prime n = null [ d | d<-[2..(n-1)], mod n d == 0]

primes = [ x | x<-[2..], prime x]

descartes l1 l2 = [ (x,y) | x<-l1, y<-l2 ]

nthprime x = last (take (x+1) primes)

-- removeNth 1 _ = []
removeNth _ [] = []
removeNth x lst = (take (x-1) lst) ++ removeNth x (drop x lst)


