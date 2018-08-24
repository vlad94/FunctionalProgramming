import Data.List
import Data.Ord

type Plant = (String, Int, Int)
getName(a,_,_) = a
getMinT(_,a,_) = a
getMaxT(_,_,a) = a

plants :: [Plant]
plants = [("peas",5,25),("beans",3,15),("cocoa",20,30)]

-- ���������� �� ������ �� �����������
allTemp l = [ (getMinT x, getMaxT y) | x <- l, y <- l, getMinT x < getMaxT y ]

-- ���������� �� ������ �� ����� �� ��������, ����� ����� �� ������ ��� ������ �����������
checkStuff temp l = helper [] l
      where helper res _l
              | null _l = res
              | (fst temp) >= (getMinT (head _l)) && (snd temp) <= (getMaxT (head _l)) = helper ((getName (head _l)) : res) (tail _l)
              | otherwise = helper res (tail _l)

-- ������ �� ������ checkStuff �� ������ ����������� � �� ����� ���� ��� ���-����� ��������

garden l = minimumBy (comparing $ length . snd) [ (temp, checkStuff temp l) | temp <- allTemp l ]
