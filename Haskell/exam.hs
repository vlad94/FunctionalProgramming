import Data.List
import Data.Ord
allIntervals1 a b = [ (x,y) | x<-[a..b], y<-[a..b], x<y]
allIntervals2 a b = [ (x,y) | x<-[a..b], y<-[a..b], x<=y]

largestInterval f g a b = [ (x,y) | (x,y)<-allIntervals1 a b, all (\x -> f x == g x) [x..y]]
countIntervals f g a b = length [ (x,y) | (x,y)<-allIntervals2 a b, all (\x -> f x /= g x) [x..y]]



sumofsquares = nub [ ((x^2)+(y^2)) | x <-[1..], y <-[1..x]] 
sumofcubes = nub [ a^3 + b^3 | a<-[1..], b<-[1..a] ]



type Video = (String,Int)
videos :: [Video]
videos = [("lolcat",15), ("dogewow", 35), ("omgseethis",28)]

averagelen = div (sum (map snd videos)) (length videos)
videoslen = maximum [ (fst x) | x<-videos, (snd x) < averagelen]

type Shoe = (String,Int)
shoes :: [Shoe]
shoes = [("boots", 38), ("sandals", 43), ("boots", 45), ("sandals", 43)]
getSize (_,a) = a
getName (a,_) = a

differentNames = (nub [ (getName x) | x<-shoes])
getSizes n = length $ nub $ filter ((==n) . fst) shoes
bestRange = maximumBy (comparing getSizes) differentNames






