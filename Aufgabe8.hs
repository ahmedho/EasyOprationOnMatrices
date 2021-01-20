-- |The 'isMatrix matrix' function returns whether matrix of type [[Int]] is actually the representation of a matrix
isMatrix :: [[Int]] -> Bool
isMatrix [] = False
isMatrix [_] = True
isMatrix (xs1:xs2:xs) = length xs1 == length xs2 && isMatrix(xs2:xs)

-- |The 'dimensions matrix' function returns the dimensions of matrix, where matrix is of type [[Int]]
-- When matrix is not a matrix, return (-1,-1)
dimensions :: [[Int]] -> (Int, Int)
dimensions (x:xs)
    | isMatrix(x:xs) = (length(x:xs), length x)
    | otherwise = (-1,-1)

-- | The 'isQuadratic matrix' function returns whether matrix of type [[Int]] is a square matrix
isQuadratic :: [[Int]] -> Bool
isQuadratic (x:xs) =
 if isMatrix (x:xs)
 then length(x:xs) == length x
 else False

-- |The 'getRow matrix n' function returns the n-th row from the matrix 
getRow :: [[Int]] -> Int -> [Int]
getRow (x:xs) b
          | isMatrix (x:xs) = (x:xs)!! b 
          |otherwise = []

-- |The 'getCol matrix n' function returns the n-th column from the matrix 
getCol :: [[Int]] -> Int -> [Int]
getCol [] _ = []
getCol (x:xs) c
             | isMatrix (x:xs) = x !! c : getCol xs c 
             | otherwise = []

-- | 'first' is a help function to give the first element
first :: (Int, Int) -> Int
first (x,_) = x

-- | 'second' gives the second element
second :: (Int, Int) -> Int
second (_,x) = x

-- | 'trav' function returns the transposed matrix of type [[Int]].
trav :: [[Int]] -> [[Int]]
trav matrix = if isMatrix matrix then help matrix (second(dimensions matrix) -1) else []
              where
help::[[Int]] -> Int -> [[Int]]
help matrix i = if i > 0 then (help matrix (i-1)) ++ [getCol matrix i ] else [getCol matrix 0]