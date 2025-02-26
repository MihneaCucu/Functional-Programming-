import Data.Char


verifL :: [Int] -> Bool
verifL l = if length l `mod` 2 == 0
			then True
			else False

takefinal :: [Int] -> Int -> [Int]
takefinal l n 
	|length l <= n = l
	|length l > n = takefinal (tail l) n

remove :: [Int] -> Int -> [Int]
remove l n
	|length l <= n = l
	|length l > n = take n l ++ drop (n+1) l

-- semiPareRec [0,2,1,7,8,56,17,18] == [0,1,4,28,9]
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t) 
	| even h    = h `div` 2 : t'
	| otherwise = t'
 	where t' = semiPareRec t


myreplicate :: Int -> Int -> [Int]
myreplicate n v
	| n == 0 = []
	| n > 0 = v : myreplicate (n - 1) v

sumImp :: [Int] -> Int
sumImp l
	|length l == 0 = 0
	|length l > 0 && head l `mod` 2 == 0 = sumImp (tail l)
	|length l > 0 && head l `mod` 2 /= 0 = sumImp (tail l) + head l

 
totalLen :: [String] -> Int
totalLen l
	|length l == 0 = 0
	|length l /= 0 && head(head l) /= 'A' = totalLen (tail l)
	|length l /= 0 && head(head l) == 'A' = length(head l) + totalLen (tail l)

nrVoc :: String -> Int
nrVoc l
	|length l == 0 = 0
	|length l /= 0 && elem (head l) "aeiouAEIOU" == False = nrVoc (tail l)
	|length l /= 0 && elem (head l) "aeiouAEIOU" == True = 1 + nrVoc (tail l) 

nrVocale :: [String] -> Int
nrVocale l 
	|length l == 0 = 0
	|length l /= 0 && head l /= reverse(head l) = nrVocale(tail l)
	|length l /= 0 && head l == reverse(head l) = nrVoc(head l) + nrVocale(tail l)
	

f :: Int -> [Int] -> [Int]
f x l
	|length l == 0 = []
	|length l /= 0 && head l `mod` 2 /= 0 = (head l) : f x (tail l)
	|length l /= 0 && head l `mod` 2 == 0 = (head l) : x : f x (tail l)
	


-- nrVocale ["sos", "civic", "palton", "desen", "aerisirea"] == 9

-- f 3 [1,2,3,4,5,6] = [1,2,3,3,4,3,5,6,3]

semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]

-- divizori 4 == [1,2,4]

divizori :: Int -> [Int]
divizori n = [x | x <- [1..n], n `mod` x == 0]

listadiv :: [Int] -> [[Int]]
listadiv l
	|length l == 0 = []
	|length l /= 0 = divizori (head l) : listadiv (tail l)

-- listadiv [1,4,6,8] == [[1],[1,2,4],[1,2,3,6],[1,2,4,8]]

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b l = [x | x <- l, x >= a && x <= b]

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec a b l
	|length l == 0 = []
	|length l > 0 && (head l >= a && head l <= b) = (head l) : inIntervalRec a b (tail l)
	|length l > 0 && (head l < a || head l > b) = inIntervalRec a b (tail l)

-- inInterval 5 10 [1..15] == [5,6,7,8,9,10]
-- inInterval 5 10 [1,3,5,2,8,-1] == [5,8]

pozitiveComp :: [Int] -> Int
pozitiveComp l = length [x | x <- l, x > 0]

pozitiveRec :: [Int] -> Int
pozitiveRec l
	|length l == 0 = 0
	|length l > 0 && (head l > 0) = 1 + pozitiveRec (tail l)
	|length l > 0 && (head l <= 0) = pozitiveRec (tail l)

-- pozitive [0,1,-3,-2,8,-1,6] == 3




pozitii :: Int -> [Int] -> [Int]
pozitii i l
	|length l == 0 = []
	|length l > 0 && (head l `mod` 2 == 0 ) = pozitii (i+1) (tail l)
	|length l > 0 && (head l `mod` 2 /= 0 ) = i : pozitii (i+1) (tail l)

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec l = pozitii 0 l

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [fst x | x <- zip[0..(length l - 1)] l, snd x `mod` 2 /= 0] 

-- pozitiiImpare [0,1,-3,-2,8,-1,6,1] == [1,2,5,7]

multDigitsRec :: String -> Int
multDigitsRec s
	|length s == 0 = 1
	|length s > 0 && isDigit(head s) == False = multDigitsRec (tail s)
	|length s > 0 && isDigit(head s) == True = digitToInt (head s) * multDigitsRec (tail s)

multDigitsComp :: String -> Int 
multDigitsComp s = product [digitToInt x| x <- s, isDigit x == True]

-- multDigits "The time is 4:25" == 40
-- multDigits "No digits here!" == 1



