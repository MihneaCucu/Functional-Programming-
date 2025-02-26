import Data.List

myInt = 31415926535897932384626433832795028841971693993751058209749445923

double :: Integer -> Integer
double x = x+x

maxim :: Integer -> Integer -> Integer
maxim x y = 
    if (x > y)
        then x
        else y

max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z =
    if (x < y)
        then if (y < z)
            then z
            else y 

    else if (x<z)
        then z 
        else x


maxim3 x y z = let
             u = maxim x y
             in maxim  u z


maxim4 :: Integer -> Integer -> Integer -> Integer -> Integer
maxim4 x y z t = let
                m1 = maxim x y 
                in 
                    let
                    m2 = maxim z t
                    in 
                        maxim m1 m2

test4 :: Integer -> Integer -> Integer -> Integer -> Bool
test4 x y z t = 
    if (maxim4 x y z t < x)
        then False
    else
        if (maxim4 x y z t < y)
            then False
        else
            if (maxim4 x y z t < z)
                then False
            else
                if (maxim4 x y z t < t)
                    then False
                else 
                    True


sumapatrate :: Integer -> Integer -> Integer
sumapatrate x y = x^2 + y^2


par :: Integer -> [Char]
par x = 
    if (x `mod` 2 == 0)
        then "par"
    else
        "impar"

fact :: Integer -> Integer
fact x = 
    if x == 0
        then 1
    else
        product[1..x]


dublu :: Integer -> Integer -> Bool
dublu x y =
    if x > y * 2
        then True 
    else
        False


maxlist :: [Integer] -> Integer
maxlist l = 
    if length l == 1
        then head l
    else 
        let
            l2 = tail l
        in 
            if (head l > maxlist l2) then head l else maxlist l2


poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = a * x * x + b * x + c


eeny :: Integer -> String
eeny x = 
    if even x == True
        then "eeny"
    else
        "meeny"


fizzbuzz :: Integer -> String
fizzbuzz x =
        if (x `mod` 3 == 0)
            then if (x `mod` 5 == 0)
                then "fizzbuzz"
                else "fizz"
            else if (x `mod` 5 == 0)
                then "buzz"
                else ""


fizzbuzzguards :: Integer -> String
fizzbuzzguards x 
    | x `mod` 3 == 0 && x `mod` 5 == 0 = "fizzbuzz"
    | x `mod` 3 == 0 && x `mod` 5 /= 0 = "fizz"
    | x `mod` 3 /= 0 && x `mod` 5 == 0 = "buzz"
    | x `mod` 3 /= 0 && x `mod` 5 /= 0 = ""



fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)
    
fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)
    
tribonacci :: Integer -> Integer
tribonacci x = 
            if x == 1
                then 1
            else if x == 2
                    then 1
                    else if x == 3
                        then 2
                        else tribonacci x - 1 + tribonacci x - 2 + tribonacci x - 3


tribonacciEc :: Integer -> Integer
tribonacciEc 1 = 1
tribonacciEc 2 = 1
tribonacciEc 3 = 2
tribonacciEc x = tribonacciEc x - 1 + tribonacciEc x - 2 + tribonacciEc x - 3


binomial :: Integer -> Integer -> Integer
binomial n k 
    | n == 0 && k == 0 = 1
    | n == 0 && k /= 0 = 0
    | n /= 0 && k == 0 = 1
    | n /= 0 && k /= 0 = (binomial (n-1) k + binomial (n-1) (k-1))
