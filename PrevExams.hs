module PrevExams where


--2016/Вариант А

--Зад.1
setUnion [] l2 = l2
setUnion l1 [] = l1
setUnion (x:xs) (y:ys)
 |x < y = x : setUnion xs (y:ys)
 |x == y = x : setUnion xs ys
 |otherwise = y : setUnion (x:xs) ys

setIntersect l1 l2 = [x | x <- l1, x `elem` l2]

setDiff l1 l2 = [x | x <- l1, (not (elem x l2))]

setSymDiff l1 l2 = setUnion (setDiff l1 l2) (setDiff l2 l1)

--Зад. 3
type Item = (String, Integer)

expiringItems::[Item] -> (String, Integer, String)
expiringItems items = (first, second, third)
  where first = fst (foldl (\x y -> if ((snd y) >= 0) && (snd x) < (snd y) then x else y) nv filteredElem)
        second = foldr (+) 0 (map (\x -> if (snd x) < 0 then 1 else 0) items)
        third = fst (foldl (\x y -> if (snd x) < (snd y) then x else y) ([], 0) items)
        filteredElem = filter (\x -> (snd x) > 0) items
        nv = if (null filteredElem) then ([], 0) else (head filteredElem)


--2016/Вариант Б
--зад.1

multisetUnion [] l2 = l2
multisetUnion l1 [] = l1
multisetUnion (x:xs) (y:ys)
 |x == y = x : multisetUnion xs ys
 |x < y = x : multisetUnion xs (y:ys)
 |otherwise = y : multisetUnion (x:xs) ys

multisetIntersect [] l2 = []
multisetIntersect l1 [] = []
multisetIntersect (x:xs) (y:ys)
 |x == y = x : multisetIntersect xs ys
 |x < y = multisetIntersect xs (y:ys)
 |otherwise = multisetIntersect (x:xs) ys

multisetSum [] l2 = l2
multisetSum l1 [] = l1
multisetSum (x:xs) (y:ys)
 |x == y = x : y : multisetSum xs ys
 |x < y = x : multisetSum xs (y:ys)
 |otherwise = y : multisetSum (x:xs) ys

--зад.3
type Quote = (String, Double)

removeEl _ [] = []
removeEl el (x:xs)
 |(fst el) == (fst x) = removeEl el xs
 |otherwise = [x] ++ (removeEl el xs)

{---
countEl x [] = 0
countEl x (y:xs)
 |(fst x) == (fst y) = (snd y) + countEl x xs
 |otherwise = countEl x xs
--}
count x [] = 0
count x (y:xs)
 |(fst x) == (fst y) = 1 + count x xs
 |otherwise = count x xs

histogram [] = []
histogram (x:xs) = [((fst x),y)] ++ histogram (removeEl x xs)
 where y = ((countEl x xs) + (snd x))/(count x xs + 1)

bestCompany :: [Quote] -> (String, Double, Double)
bestCompany info = (name, lowest, highest)
 where hist = histogram info
       maxCompany = maximum (map (\x -> (snd x)) hist)
       name = fst (head (filter (\x -> (snd x) == maxCompany) hist))
       lowest = minimum (map (\x -> (snd x)) (filter (\x -> (fst x) == name) info))
       highest = maximum (map (\x -> (snd x)) (filter (\x -> (fst x) == name) info))


--2017/Вариант А
--Зад.1

distance (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

sumSquareDist point restPoints = foldl (+) 0 (map (\x -> (distance x point)^2) restPoints)

{--
removeElem point [] = []
removeElem point (x:restPnts)
 |(fst point) == (fst x) && (snd point) == (snd x) = restPnts
 |otherwise = x : removeElem point restPnts
--}

findMedoid allPoints = point
  where point = fst (foldr (\x y -> if (snd x) < (snd y) then x else y) (head minEls) minEls)
        minEls = (map (\x -> (x, sumSquareDist x allPoints)) allPoints)


--Зад.2
sumHelper k n (x:xs) = element : sumHelper k n lastFive
  where element = foldr (+) 0 (x:xs)
        lastFive = if (length (x:xs) >= n) then (xs ++ [element]) else (x:xs) ++ [element]

sumLast k n = k : sumHelper k n [k]


--Вариант Б
--Зад.1

distance (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

sumDistances point restPoints = foldl (+) 0 (map (\x -> (distance point x)^2) restPoints)

findPoint allPoints = point
 where point = fst (foldr (\x y -> if (snd x) > (snd y) then x else y) (head maxEl) maxEl)
       maxEl = map (\x -> (x, sumDistances x allPoints)) allPoints

--Зад.2
multLastHelper k n (x:xs) = element : multLastHelper k n lastEl
  where element = foldr (*) 1 (x:xs)
        lastEl = if (length (x:xs)) < n then (x:xs) ++ [element] else xs ++ [element]

multLast k n = (k : (k * k) : (multLastHelper k n [k, (k*k)]))


--2018/Вариант А
--Зад.1
countEl el [] = 0
countEl el (x:xs)
 |el == x = 1 + (countEl el xs)
 |otherwise = (countEl el xs)

mostFrequentInOneList nums max
 |(null nums) = (fst max)
 |(countEl (head nums) nums) > (snd max) = mostFrequentInOneList (tail nums) ((head nums),(countEl (head nums) nums))
 |otherwise = mostFrequentInOneList (tail nums) max

mst nums = if (null nums) then error "Empty list" else mostFrequentInOneList nums ((head nums), 1)

mostFrequent numbers = if (length (filter (\x -> x == (head mapList)) mapList)) == (length numbers) then (head mapList) else 0
 where mapList = (map (\x -> mst x) numbers)

--Зад. 3


--Вариант Б
--Зад.1

makeCoupleMaxAndMin numbers = (x,y)
 where x = maximum numbers
       y = minimum numbers

isInAll num listOfPairs = result
 where result = foldr (\x y -> (num == (fst x) || num == (snd x)) && y ) True listOfPairs

extremum numbers = result
 where listWithExtremums = map (\x -> makeCoupleMaxAndMin x) numbers
       firstNum = fst (head listWithExtremums)
       secondNum = snd (head listWithExtremums)
       forFirst = isInAll firstNum listWithExtremums
       forSecond = isInAll secondNum listWithExtremums
       result
        |forFirst = firstNum
        |forSecond = secondNum
        |otherwise = 0




