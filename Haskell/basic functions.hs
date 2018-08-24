fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

saySign :: (Num x, Ord x) => x -> String
saySign x
   | x<0  = "Negative"
   | x==0  = "Zero"	
   | otherwise  = "Positive"

countroots a b c
   | a==0 = "not quadratic"
   | d<0 = "zero roots"
   | d==0 = "one root"
   | otherwise = "two roots"
   where d = (b*b) - 4*a*c

cylinderVolume r h = (3.141592*r*r*h)

useless a b c d
   | a==0 || b==0 || c==0 || d==0 = 1
   | otherwise = a+b+c+d 
 
