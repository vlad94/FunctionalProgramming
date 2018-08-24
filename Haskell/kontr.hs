sumofsquares = [ ((a^2)+(b^2)) | a<-[1..50], b<-[1..50]]

type Battery = (Int,Double)

batteries :: [Battery]
batteries = [(10, 0.50), (25, 1.10), (30, 1.5), (32, 1.3), (40, 1.6)]
getWat(a,_) = a
getPrice(_,a) = a

bestbattery lst k = minimum [ getPrice wat | wat<-batteries, getWat wat >= k]

type Video = (String,Int)
videos :: [Video]
videos = [("lolcat",15), ("dogewow", 35), ("omgseethis",28)]

averageLength lst = div (sum (map snd videos)) (length videos)

averages lst = maximum [ snd videa | videa<-videos, snd videa < averageLength videos]
averageVideo lst = head [ fst ime | ime<-videos, snd ime == averages videos]

generateAllIntervals a b = [ (x,y) | x<-[a..b], y<-[a..b], x<=y]

check f g a b = [ (x,y) | (x,y)<-generateAllIntervals a b, all (\x -> f x /= g x) [x..y]]

type Plant = (String,Int,Int)
plants :: [Plant]
plants = [("peas",5,25),("beans",3,15),("cocoa",20,30)]

getName(a,_,_) = a
getMin(_,a,_) = a
getMax(_,_,a) = a

allIntervals lst = [ (x,y) | x <-map getMin plants, y<- map getMax plants]
--mostcommon lst = [ (x,y) | 















