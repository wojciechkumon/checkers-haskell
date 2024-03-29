module Utils where

addPair :: (Int,Int) -> (Int,Int) -> (Int,Int)
addPair (a,b) (c,d) = (a+c,b+d)

multPair :: Int -> (Int,Int) -> (Int,Int)
multPair n (a,b) = (n*a,n*b)

updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix matrix element (i, j) = updateList matrix i (\row -> updateList row j (const element))

updateList :: [a] -> Int -> (a -> a) -> [a]
updateList [] _ _ = []
updateList (x:xs) 0 fun = (fun x):xs
updateList (x:xs) n fun = x:updateList xs (n-1) fun

oneIfGreaterElseMinusOne :: Int -> Int -> Int
oneIfGreaterElseMinusOne first second = if (first > second) then 1 else -1
