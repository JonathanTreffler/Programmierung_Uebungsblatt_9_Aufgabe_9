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

main = do
    print (symmetricDifference [1,2,3,4,5,8] [1,3,4,5,6,7,8,9])