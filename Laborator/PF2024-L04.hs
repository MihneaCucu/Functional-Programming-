import Data.Char

{-
[x^2 |x <- [1..10], x `rem` 3 == 2] - [4, 25, 64]
[(x,y) | x <- [1..5], y <- [x..(x+2)]] - [(1,1), (1,2), (1,3), (2,2) ... (5,7)]
[(x,y) | x <- [1..3], let k = x^2, y <- [1..k]] - [(1,1), (2,1), (2,2), (2,3), (2,4) .. (3,9)]
[x | x <- "Facultatea de Matematica si Informatica", elem x ['A'..'Z']] - "FMI"
[[x..y] | x <- [1..5], y <- [1..5], x < y] - [[1,2], [1,2,3], [1,2,3,4], [1,2,3,4,5], [2,3] ... [4,5]]
-}

factori :: Int -> [Int]
factori n = [x | x <- [1..n], n `mod` x == 0]


prim :: Int -> Bool
prim n = length (factori n) == 2 
		



numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x]


myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : myzip3 xs ys zs
myzip3 _ _ _ = []


ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat (x:xs) = undefined


ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata = undefined



firstEl :: [(a,b)] -> [a]
firstEl = map fst


sumList :: [[Int]] -> [Int]
sumList = map sum

-- aici cu filter nu pastreaza ordinea
prel2f :: [Int] -> [Int]
prel2f x = map (`div` 2) (filter even x) ++ map (* 2) (filter odd x) 

prel2 :: [Int] -> [Int]
prel2 l = map (\x -> if even x then x `div` 2 else x * 2) l

contineChar :: Char -> [String] -> [String]
contineChar a = filter (elem a)

patrateImpare :: [Int] -> [Int]
patrateImpare = map (^2) . filter odd

patratePozImpare :: [Int] -> [Int]
patratePozImpare l = map (^2) [x | (i, x) <- zip [0..] l, odd i]


numaiVocale :: [String] -> [String]
numaiVocale l = map (filter vocala) l
	where
		vocala c = toLower c `elem` "aeiou"


mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []  
mymap f (x:xs) = f x : mymap f xs 


myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []  
myfilter p (x:xs)
  | p x = x : myfilter p xs  
  | otherwise = myfilter p xs  
