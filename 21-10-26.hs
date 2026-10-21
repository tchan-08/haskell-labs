import Data.Char (toUpper)
import Data.List (sortBy)
import Data.Bits (Bits(xor))
import Prelude hiding ((.))
main :: IO()

greaterThan :: Int -> [Int] -> [Int]
greaterThan _ [] = []
greaterThan n xs = [x | x <- xs, x > n]

-- >>> greaterThan 3 [1,2,3,4,5]
-- [4,5]

filterOdd :: Integral a => [a] -> [a]
filterOdd [] = []
filterOdd xs = [x | x <- xs, x `mod` 2 /= 0]

-- >>> filterOdd [1,2,3,4,5]
-- [1,3,5]

filterSquare :: [Double] -> [Double]
filterSquare [] = []
filterSquare xs = [x | x <- xs, isSquare x]

isSquare :: Double -> Bool
isSquare a = root * root == a
    where
        root = fromIntegral (floor (sqrt a))

-- >>> filterSquare [1..10]
-- [1.0,4.0,9.0]

primeSieveUpTo :: Int -> [Int]
primeSieveUpTo n
    | n <= 1 = []
    | otherwise = removeMultiples [2..n]

removeMultiples :: [Int] -> [Int]
removeMultiples [] = []
removeMultiples (x:xs)
    | x > 1 = x : (removeMultiples $ filterIsMultiple x xs)
    | otherwise = removeMultiples $ xs

filterIsMultiple :: Int -> [Int] -> [Int]
filterIsMultiple _ [] = []
filterIsMultiple n xs = [x | x <- xs, x `mod` n /= 0]

-- >>> primeSieveUpTo 13
-- [2,3,5,7,11,13]

-- >>> removeMultiples [1,13,2,17,20]
-- [13,2,17]

-- >>> filterIsMultiple 2 [2,3,4,5,6,7,8]
-- [3,5,7]

doubleMap :: [Int] -> [Int]
doubleMap xs = map double xs

double :: Int -> Int
double x = x * 2

-- >>> doubleMap [1,2,3,4,5]
-- [2,4,6,8,10]

shout :: String -> String 
shout xs = map toUpper xs

-- >>> shout "hello"
-- "HELLO"

basicTitle :: String -> String
basicTitle xs = unwords (map capitaliseFirst listOfWords)
    where listOfWords = words xs

capitaliseFirst :: String -> String 
capitaliseFirst (x:xs) = toUpper x : xs 
-- >>> basicTitle "hello world"
-- "Hello World"

anyList :: (a -> Bool) -> [a] -> Bool
anyList _ [] = True
anyList f xs = checkTrue $ map f xs

checkTrue :: [Bool] -> Bool
checkTrue [] = False
checkTrue (x:xs) = if x then True else checkTrue xs

-- >>> anyList odd [1,5,3]
-- True

odds :: Int -> [Int]
odds n = [x | x <- [1..n], x `mod` 2 /= 0, x <= n]

-- >>> odds 5
-- [1,3,5]

orderedPairs :: [Int] -> [(Int,Int)]
orderedPairs xs = [(x,y) | x <- xs, y <- xs, x <= y, x /= y]

-- >>> orderedPairs [3,4,1]
-- [(3,4),(1,3),(1,4)]

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f xs = [f x | x <- xs]

-- >>> myMap double [2,4,6,8]
-- [4,8,12,16]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f xs = [x | x <- xs, f x]

-- >>> myFilter even [1..10]
-- [2,4,6,8,10]

myFunc :: [Int] -> [Int]
myFunc [] = []
myFunc xs = map (2*) (filter (>=2) xs)

-- >>> myFunc [1,2,3]
-- [4,6]

myZip :: [a] -> [b] -> [(a,b)]
myZip [] [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

-- >>> myZip [1,2,3] [4,5,6]
-- [(1,4),(2,5),(3,6)]

compareSnd :: (Ord a, Ord b) => [(a,b)] -> [(a,b)]
compareSnd [] = []
compareSnd [x] = [x]
compareSnd xs = sortBy sortPairs xs

sortPairs :: (Ord a, Ord b) => (a,b) -> (a,b) -> Ordering
sortPairs (a,b) (c,d)
    | b < d = LT
    | b == d = EQ
    | otherwise = GT

-- >>> compareSnd [(1, 9), (2, 8), (3, 7)]
-- [(3,7),(2,8),(1,9)]

applyN :: (Eq n, Num n) => (a -> a) -> a -> n -> a
applyN f x 0 = x 
applyN f x n = applyN f (f x) (n-1)

succ' :: Int -> Int 
succ' a = a + 1

-- >>> applyN succ 1 5
-- 6

applyUntilStable :: Eq a => (a -> a) -> a -> a
applyUntilStable f x
    | f x == x = x
    | f x /= x = applyUntilStable f (f x)

-- >>> applyUntilStable (\x -> if x <= 0 then 0 else x - 1) 7
-- 0

integrate :: (Float -> Float) -> Float -> Float
integrate _ 0 = 0
integrate f x = x * ((f 0 + f x ) / 2)

(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)

foo :: [Int] -> [Int]
foo = map (*10) . filter even

bar :: [Int] -> Int
bar = foldr (+) 0 . map (+1) . filter odd


main = do print "hi"