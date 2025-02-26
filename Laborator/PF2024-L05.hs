sumpimpare :: [Int] -> Int
sumpimpare l = foldr(+) 0 (map (^2) (filter odd l))

alltrue :: [Bool] -> Bool
alltrue l = foldr(&&) True l

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies f l = foldr(&&) True (map f l)

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies f l = foldr(||) True (map f l)

mapFoldr :: (a->b) -> [a] -> [b]
mapFoldr f = foldr (\x acc -> f x : acc) []

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p = foldr (\x acc -> if p x then x : acc else acc) []

listToInt :: [Integer]-> Integer
listToInt = foldl (\acc x -> acc *10 + x) 0

rmChar :: Char -> String -> String
rmChar c s = filter (/= c) s

rmCharsRec :: String -> String -> String
rmCharsRec [] s = s
rmCharsRec (c:cs) s = rmCharsRec cs (rmChar c s)

rmCharsFold :: String -> String -> String
rmCharsFold a b = foldr rmChar b a 

myReverse :: [Int] -> [Int]
myReverse = foldl (\acc x -> x : acc) []


myElem :: Int -> [Int] -> Bool
myElem x = foldr (\y acc -> (y==x) || acc) False
    
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip = foldr (\(x, y) (xs, ys) -> (x : xs, y : ys)) ([], [])

union :: [Int] -> [Int] -> [Int]
union x y = foldr (:) y x

intersect :: [Int] -> [Int] -> [Int]
intersect x y = filter (`elem` y) x

insertEverywhere :: a -> [a] -> [[a]]
insertEverywhere x [] = [[x]]
insertEverywhere x (y:ys) = (x:y:ys) : map (y:) (insertEverywhere x ys) 

permutations :: [Int] -> [[Int]]
permutations = foldr f [[]]
	where
		f :: a -> [[a]] -> [[a]]
		f x = concat.map(insertEverywhere x)











