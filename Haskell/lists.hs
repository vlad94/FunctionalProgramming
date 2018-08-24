myzipWith op lst1 [] = []
myzipWith op [] lst2 = []
myzipWith op lst1 lst2 = (op (head lst1) (head lst2)) : myzipWith op (tail lst1) (tail lst2)

myflip op a b = (op b a)

mytakewhile _ [] = []
mytakewhile p (x:xs)
   | p x = x : mytakewhile p xs
   | otherwise = []

testList = [1,1,2,3,3,3,4,2,2,2,1,1]

compress [] = []
compress lst = ((head lst), firsts) : compress rest
   where firsts = length (takeWhile (\x -> x == (head lst)) lst)
         rest = dropWhile (\x -> x == (head lst)) lst

maxRepeated lst = maximum (map snd (compress lst))

removeel x [] = []
removeel x lst
   | x == (head lst) = removeel x (tail lst)
   | otherwise = (head lst) : (removeel x (tail lst))

makeSet [] = []
makeSet lst = (head lst) : makeSet (removeel (head lst) lst)

histogram lst = [(el, count el lst) | el <- set ]
	where set = makeSet lst
              count el lst = length (filter (\x -> x == el) lst)

maxDistance points = maximum [ dist p1 p2 | p1<-points, p2<-points ]
    where dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)


 