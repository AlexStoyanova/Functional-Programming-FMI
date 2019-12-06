module SecondTasks where
import Prelude hiding (elem, reverse, map,
 filter, foldr, product, zip, last, init, (!!), take, drop, splitAt, chunk)

--Списъци в Haskell

--task1
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:xs) = if x == y then True else (elem x xs)

--task2
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

--task3
map :: (a -> b) -> [a] -> [b]
map _ [] = []
--map f (x:xs) = [f x] ++ map f xs
map f (x:xs) = f x : map f xs

--task4
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
 |(p x) = x : filter p xs
 |otherwise = filter p xs

--task5
foldr :: (t1 -> t1 -> t1) -> t1 -> [t1] -> t1
foldr _ nullValue [] = nullValue
foldr op nullValue (x:xs) = foldr op (op x nullValue) xs

--task6
sort [] = []
sort (x:xs) = sort leftElems ++ [x] ++ sort rightElems
 where leftElems = filter (<=x) xs
       rightElems = filter (>x) xs


--Матрици

--task7
nthEl _ [] = error "No elements in list"
nthEl n (x:xs)
 |n == 0 = x
 |n < 0 = error "No such index"
 |otherwise = nthEl (n - 1) xs

column [] i = []
column (x:xs) i = [nthEl i x] ++ (column xs i)
--column l n = map (nthEl n) l

--task8
transpose [] _ = []
transpose m i
 |i >= length m = []
 |otherwise = [column m i] ++ transpose m (i + 1)

transp m = transpose m 0

--task9
diagonal [] _ = []
diagonal (x:xs) i = [nthEl i x] ++ diagonal xs (i + 1)

diag1 m = diagonal m 0

--task10
diagonal2 [] _ = []
diagonal2 (x:xs) i = [nthEl i x] ++ diagonal2 xs (i - 1)

diag2 m = diagonal2 m ((length m) - 1)

--task11
foldm _ nullValue [] = nullValue
foldm op nullValue (x:xs) = op (foldr op nullValue x) (foldm op nullValue xs)

--Множества

--task12
intersection a b = [x | x <- a, x `elem` b]

--task13
difference a b = [x | x <- a, (not (x `elem` b))]

--task14
product a b = [(x, y)| x <- a, y <- b]

--task15
triplets n = [(x, y, z)| x <- [1..n], y <- [x..n], z <- [y..n],
 x^2 + y^2 == z^2, x + y + z <= n]

 --Асоциативни списъци

 --task16
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = [(x,y)] ++ (zip xs ys)

--task17
countEl x [] = 0
countEl x (y:xs)
 |x == y = 1 + countEl x xs
 |otherwise = countEl x xs

removeEl _ [] = []
removeEl el (x:xs)
 |el == x = removeEl el xs
 |otherwise = [x] ++ (removeEl el xs)

histogram [] = []
histogram (x:xs) = [(x, y)] ++ histogram (removeEl x xs)
 where y = (countEl x xs) + 1

--task18
countOrd x [] = 0
countOrd x (y:ys)
 |x == y = 1 + (countOrd x ys)
 |otherwise = 0

rm x [] = []
rm x (y:ys)
 |x == y = rm x ys
 |otherwise = y:ys

runLengthEncode [] = []
runLengthEncode (x:xs) = [(x, y)] ++ runLengthEncode (rm x xs)
 where y = (countOrd x xs) + 1

--Още задачи за списъци, функции от модула Prelude

--last
last [x] = x
last (_:xs) = last xs
last [] = error "empty list"

--init
init [] = error "empty list"
init [x] = []
init (x:xs) = [x] ++ init xs

--(!!)
(!!) [] _ = error "Empty list"
(!!) (x:_) 0 = x
(!!) (_:xs) n = (!!) xs (n - 1)

--take
take _ [] = []
take n (x:xs)
 |n <= 0 = []
 |n == 1 = [x]
 |otherwise = [x] ++ take (n - 1) xs

--drop
drop _ [] = []
drop n l
 | n <= 0 = l
drop n (_:xs) = drop (n-1) xs

--splitAt
splitAt _ [] = ([], [])
splitAt n l = (take n l, drop n l)

--chunk
chunk _ [] = []
chunk n l = let (first, rest) = splitAt n l
            in first : chunk n rest

--takeWhile
