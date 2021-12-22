
-- a
isElementOfList :: Int -> [Int] -> Bool
isElementOfList el [] = False
isElementOfList el (x:xs) | el == x = True
                        | otherwise = isElementOfList el xs

symmetricDifference :: [Int] -> [Int] -> [Int]
symmetricDifference [] [] = []
symmetricDifference xs [] = xs
symmetricDifference [] ys = ys
symmetricDifference x y = symmetricDifferenceX x y ++ symmetricDifferenceY x y

symmetricDifferenceX :: [Int] -> [Int] -> [Int]
symmetricDifferenceX [] [] = []
symmetricDifferenceX xs [] = xs
symmetricDifferenceX [] ys = []
symmetricDifferenceX (x:xs) ys = if isElementOfList x ys then symmetricDifferenceX xs ys
                                else x : symmetricDifferenceX xs ys

symmetricDifferenceY :: [Int] -> [Int] -> [Int]
symmetricDifferenceY [] [] = []
symmetricDifferenceY xs [] = []
symmetricDifferenceY [] ys = ys
symmetricDifferenceY xs (y:ys) = if isElementOfList y xs then symmetricDifferenceY xs ys
                                else y : symmetricDifferenceY xs ys

-- b

powerlist :: [Int] -> [[Int]]
powerlist [] = [[]];
powerlist xs =  [] : subsequences xs

subsequences :: [Int] -> [[Int]]
subsequences [] = []
subsequences (x:xs) = [x] : funktionaufListeAnwenden f (subsequences xs)
                            where f ys r = ys : (x : ys) : r

funktionaufListeAnwenden f [] = []
funktionaufListeAnwenden f (x:xs) = f x (funktionaufListeAnwenden f xs)


-- test

getElement :: Int -> [Int] -> Int
getElement _ [] = 0
getElement 0 (x:xs) = x 
getElement index (x:xs) = getElement (index-1) xs

lengthOfList :: [Int] -> Int
lengthOfList [] = 0
lengthOfList (x:xs) = 1 + lengthOfList xs

main = do
    -- print (symmetricDifference [1,2,3,4,5,8] [1,3,4,5,6,7,8,9])
    -- print (getElement 8 [1,2,4,5])
    -- print (lengthOfList [1,2,3])
    print (powerlist [1,2,3])