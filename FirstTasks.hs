module FirstTasks where
import Prelude hiding (signum, sum)

-- task1
add = (+)

--task2
addN = add

--task3
signum x
 |x == 0 = 0
 |x > 0 = x
 |otherwise = -x

--task4
factorial n
 |n == 0 = 1
 |otherwise = n*factorial(n - 1)

--task5
fibonacci n
 |n == 0 = 1
 |n == 1 = 1
 |otherwise = fibonacci(n-1) + fibonacci(n-2)

--task6
less a = (a < )

--task7
sum start end
 |start > end = 0
 |otherwise = start + sum (start + 1) end

--task8

fastPow x n
 |n == 0 = 1
 |n == 1 = x
 |mod n 2 == 0 = (fastPow x (div n 2))^2
 |otherwise = x * fastPow x (n - 1)

--task9

countDigits n
 |(mod n 10) == n = 1
 |otherwise = (countDigits (div n 10)) + 1

--task10

twice f x = f (f x)

--task11

compose f g x = f (g x)

--task12

accumulate combiner nullValue term a next b
 |a > b = nullValue
 |otherwise = accumulate combiner (combiner a nullValue) term (next a) next b

--task13

reverseDigits n
 |(mod n 10) == n = n
 |otherwise = 10^(countDigits n - 1) * (mod n 10) + (reverseDigits (div n 10))

