--https://github.com/fmi/fp2015/blob/master/week11/homework/README.md

--Exercise 1

listToNumber :: [Int] -> Int
listToNumber [] = 0
listToNumber (x:xs) = x * 10 ^ (length (x:xs) - 1) + listToNumber xs


--Exercise 2

suffix :: (Eq a) => [a] -> [a] -> Bool
suffix array1 array2
    | null array1 = True
    | last array1 /= last array2 = False
    | otherwise = suffix (init array1) (init array2)


--Exercise 3
occurrences :: [Int] -> [Int] -> [Int]
occurrences array1 array2
    | null array1 = []
    | otherwise = (searchForElement (head array1) array2):occurrences (tail array1) array2
        where
            searchForElement t array
               | null array = 0
               | t == (head array) = 1 + searchForElement t (tail array)
               | otherwise = searchForElement t (tail array)


--Exercise 4

removeAt :: Int -> [a] -> [a]
removeAt index list
    | index < 0 || index >= length list = error "Index out of bounds"
    | otherwise = take index list ++ drop (index + 1) list

