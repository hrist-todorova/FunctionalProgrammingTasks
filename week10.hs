--https://github.com/fmi/fp2015/blob/master/week10/homework/README.md

--Exercise 1  Truncatable primes

isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = isPrime' 2 n
        where
            isPrime' current n
                | current == n = True
                | mod n current == 0 = False
                | otherwise = isPrime' (current + 1) n

truncatablePrime :: Int -> Bool
truncatablePrime x
    | (isPrime x) == True && (isPrime (div x 10)) == True = True
    | otherwise = False


-- Exercise 2  Contains Digits?

containsDigits :: Int -> Int -> Bool
containsDigits n s
    | s == 0 = True
    | checkFor n (mod s 10) == False = False
    | otherwise = containsDigits n (div s 10)


checkFor :: Int -> Int -> Bool
checkFor n number
    | (mod n 10) == number = True
    | n <= 0 = False
    | otherwise = checkFor (div n 10) number


-- Exercise 3  Product of Digits

productOfDigits :: Int -> Int
productOfDigits x
    | (div x 10) == 0 = x
    | otherwise = (mod x 10) * productOfDigits (div x 10)


-- Exercise 4  Interesting Number

interestingNumber :: Int -> Bool
interestingNumber n = if sumOfDivisors s == n then True else False
    where
        s = sumOfDivisors n

sumOfDivisors :: Int -> Int
sumOfDivisors x
    | isPrime x = 1
    | otherwise = helpingFunc x 1
        where
            helpingFunc x divisor
                | divisor >= x = 0
                | (mod x divisor) == 0 = divisor + helpingFunc x (succ divisor)
                | otherwise = helpingFunc x (succ divisor)


-- Exercise 5  Quadrant

quadrant :: Double -> Double -> Int
quadrant x y
    | x == 0 && y == 0 = 0
    | x >= 0 && y >= 0 = 1
    | x <= 0 && y >= 0 = 2
    | x <= 0 && y <= 0 = 3
    | x >= 0 && y <= 0 = 4
    
