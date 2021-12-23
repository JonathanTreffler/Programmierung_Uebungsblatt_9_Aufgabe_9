
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
-- Ich halte das für einen Fehler in der Aufgabenstellung
-- Der untere ausgeklammerte Code kann nur einen Teil der Permutationen mittels Verscheibung bestimmen, aber nicht die die Elemente vertauschen

permutations :: [Int] -> [[Int]]
permutations [] = []
permutations xs = permutate xs []

permutate :: [Int] -> [Int] -> [[Int]]
permutate [] _ = []
permutate (x:xs) ys = [((x:xs) ++ ys)] ++ permutate xs (ys ++ [x])


-- d)


nodes :: [(Int,Int)] -> [Int]
nodes [] = []
nodes arr = nodesHelper arr []

nodesHelper :: [(Int,Int)] -> [Int] -> [Int]
nodesHelper [] _ = []
nodesHelper ((e1,e2):es) bisherige = nodesHelper2 e1 bisherige ++ nodesHelper2 e2 (e1 : bisherige) ++ nodesHelper es (e1 : e2 : bisherige)

nodesHelper2 :: Int -> [Int] -> [Int]
nodesHelper2 e arr = [e | not (elementInList e arr)]

-- Recursively check if Element el is in List (x:xs)
elementInList :: Int -> [Int] -> Bool
elementInList _ [] = False
elementInList el (x:xs) = (el == x) || elementInList el xs


-- e)


existsPath :: [(Int,Int)] -> Int -> Int -> Bool
existsPath [] _ _ = False
existsPath es el1 el2 = (el1 == el2) || elementInList el2 (nodeNeighbours es el1) || existsPathFromList (removeNodeFromGraph es el1) (nodeNeighbours es el1) el2

-- Recursively check if a path exists from a list of nodes (t:rest) to a node el
existsPathFromList :: [(Int,Int)] -> [Int] -> Int -> Bool
existsPathFromList _ [] _ = False
existsPathFromList [] _ _ = False
existsPathFromList graph (t:rest) el = existsPath graph t el || existsPathFromList graph rest el

-- Get all nodes than can be reached with one step from the Element el
nodeNeighbours :: [(Int, Int)] -> Int -> [Int]
nodeNeighbours [] _ = []
nodeNeighbours ((e1,e2):es) el = if e1 == el then e2 : nodeNeighbours es el else nodeNeighbours es el

-- Check if Element el is in Tupel (e1,e2) 
elementInTupel :: (Int,Int) -> Int -> Bool
elementInTupel (e1,e2) el = (el==e1) || (el==e2)

-- Recursively remove all references to Element rem e.g remove it from the Graph (el:rest)
removeNodeFromGraph :: [(Int,Int)] -> Int -> [(Int,Int)]
removeNodeFromGraph [] _ = []
removeNodeFromGraph (el:rest) rem = if elementInTupel el rem then removeNodeFromGraph rest rem else el : removeNodeFromGraph rest rem


-- f)


-- CHeck if graph es is connected
isConnected :: [(Int,Int)] -> Bool
isConnected [] = True
isConnected es = graphHasPaths es (nodes es) (nodes es)

graphHasPaths :: [(Int,Int)] -> [Int] -> [Int] -> Bool
graphHasPaths [] _ _ = False
graphHasPaths _ [] _ = True
graphHasPaths _ _ [] = True
graphHasPaths graph k (el:rest) = graphHasPathFromList graph k el && graphHasPaths graph k rest

graphHasPathFromList :: [(Int,Int)] -> [Int] -> Int -> Bool
graphHasPathFromList [] _ _ = False
graphHasPathFromList _ [] _ = True
graphHasPathFromList graph (el:rest) d = existsPath graph el d && graphHasPathFromList graph rest d


main = do
    print("-------- a) --------")
    print("symmetricDifference [1,2,3,4,5,8] [1,3,4,5,6,7,8,9] (Expected [2,6,7,9])")
    print (symmetricDifference [1,2,3,4,5,8] [1,3,4,5,6,7,8,9])

    print("-------- b) --------")
    print ("powerlist [1,2,3] (Expected [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]])")
    print (powerlist [1,2,3])

    print("-------- c) --------")
    print("permutations [1,2,3] (Expected [[1,2,3],[2,1,3],[3,2,1],[1,3,2],[3,1,2],[2,3,1]]")
    print(permutations [1,2,3])

    print("-------- d) --------")
    print ("nodes [(1,2),(2,3),(3,1),(4,5),(3,4) (Expected [1,2,3,4,5])")
    print(nodes [(1,2),(2,3),(3,1),(4,5),(3,4)])

    print("-------- e) --------")
    print("existsPath  [(1,2),(2,3),(3,1),(4,5),(3,4)] 1 3 (Expected True)")
    print(existsPath  [(1,2),(2,3),(3,1),(4,5),(3,4)] 1 3)
    print("existsPath  [(1,2),(2,3),(3,1),(4,5),(3,4)] 1 5 (Expected True)")
    print(existsPath  [(1,2),(2,3),(3,1),(4,5),(3,4)] 1 5)
    print("existsPath  [(1,2),(2,3),(3,1),(4,5),(3,4)] 5 5 (Expected True)")
    print(existsPath  [(1,2),(2,3),(3,1),(4,5),(3,4)] 5 5)
    print("existsPath  [(1,2),(2,3),(3,1),(4,5),(3,4)] 5 1 (Expected False)")
    print(existsPath  [(1,2),(2,3),(3,1),(4,5),(3,4)] 5 1)
    print("existsPath  [(1,2),(2,3),(3,1),(4,5),(3,4)] 5 4 (Expected False)")
    print(existsPath  [(1,2),(2,3),(3,1),(4,5),(3,4)] 5 4)
    print("existsPath  [(1,2),(2,3),(3,1),(4,5),(3,4)] 4 3 (Expected False)")
    print(existsPath  [(1,2),(2,3),(3,1),(4,5),(3,4)] 4 3)

    print("-------- f) --------")
    print("isConnected [(1,2),(2,3),(3,1),(4,5),(3,4)]  (Expected False)")
    print(isConnected [(1,2),(2,3),(3,1),(4,5),(3,4)])
    print("isConnected (5,1):[(1,2),(2,3),(3,1),(4,5),(3,4)]  (Expected True)")
    print(isConnected ((5,1):[(1,2),(2,3),(3,1),(4,5),(3,4)]))