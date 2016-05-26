module Utils where

addPair :: (Int,Int) -> (Int,Int) -> (Int,Int)
addPair (a,b) (c,d) = (a+c,b+d)

multPair :: Int -> (Int,Int) -> (Int,Int)
multPair n (a,b) = (n*a,n*b)

applyAll :: a -> [a -> a] -> a
applyAll a [] = a
applyAll a (f:xs) = applyAll (f a) xs

updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix matrix element (i, j) = updateList matrix i (\z -> updateList z j (const element))

updateList :: [a] -> Int -> (a -> a) -> [a]
updateList [] _ _ = []
updateList (x:xs) 0 f = (f x):xs
updateList (x:xs) n f = x:updateList xs (n-1) f