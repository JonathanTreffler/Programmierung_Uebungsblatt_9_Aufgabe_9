
-- a)
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

-- b)

powerlist :: [Int] -> [[Int]]
powerlist [] = [[]];
powerlist xs =  [] : subsequences xs

subsequences :: [Int] -> [[Int]]
subsequences [] = []
subsequences (x:xs) = [x] : funktionaufListeAnwenden function (subsequences xs)
                            where function ys r = ys : (x : ys) : r

funktionaufListeAnwenden f [] = []
funktionaufListeAnwenden f (x:xs) = f x (funktionaufListeAnwenden f xs)


-- c)

-- Wie soll es möglich sein ohne den !! Operator, der in der Aufgabenstellung nicht whitelisted wurde Elemente zu vertauschen ?
-- Der untere ausgeklammerte Code kann nur einen Teil der Permutationen mittels Verscheibung bestimmen, aber nicht die die Elemente vertauschen

--permutations :: [Int] -> [[Int]]
--permutations [] = []
--permutations xs = permutate xs []

--permutate :: [Int] -> [Int] -> [[Int]]
--permutate [] _ = []
--permutate (x:xs) ys = [((x:xs) ++ ys)] ++ permutate xs (ys ++ [x])


-- d)

nodes :: [(Int,Int)] -> [Int]
nodes [] = []
nodes arr = nodesHelper arr []

nodesHelper :: [(Int,Int)] -> [Int] -> [Int]
nodesHelper [] _ = []
nodesHelper ((e1,e2):es) bisherige = nodesHelper2 e1 bisherige ++ nodesHelper2 e2 (e1 : bisherige) ++ nodesHelper es (e1 : e2 : bisherige)

nodesHelper2 :: Int -> [Int] -> [Int]
nodesHelper2 e arr = if (istInListe e arr) then [] else [e]

istInListe :: Int -> [Int] -> Bool
istInListe _ [] = False
istInListe el (x:xs) = (el == x) || istInListe el xs


-- e)


existsPath :: [(Int,Int)] -> Int -> Int -> Bool
existsPath [] _ _ = False
existsPath ((e1,e2):es) a b
  | a == b = True
  | a == e1 && existsPath es a e2 = existsPath es e2 b || existsPath es a b
  | otherwise = existsPath es a b


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
    -- print (powerlist [1,2,3])
    -- print(permutations [1,2,3])
    print(existsPath  [(1,2),(2,3),(3,1),(4,5),(3,4)] 1 5)