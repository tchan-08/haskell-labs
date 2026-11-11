import Data.Char(toUpper)
main :: IO()

-- 1.1a
data Maybe' a = Just' a | Nothing'

instance Functor Maybe' where
    fmap f (Just' a) = Just' (f a)
    fmap f Nothing' = Nothing'

-- 1.1b

data Either' a b = Left' a | Right' b

instance Functor (Either' a) where
    fmap f (Left' a) = Left' a
    fmap f (Right' b) = Right' (f b)

-- 1.1c

data Expr a = Var a | Lit a | Add (Expr a) (Expr a) | Mul (Expr a ) (Expr a) deriving Show

instance Functor Expr where
    fmap f (Lit n) = Lit (f n)
    fmap f (Var a) = Var (f a)
    fmap f (Add x y) = Add (fmap f x) (fmap f y)
    fmap f (Mul x y) = Mul (fmap f x) (fmap f y)

-- 1.2

maybeAddition :: Maybe Int -> Maybe Int
maybeAddition Nothing = Nothing
maybeAddition (Just x) = Just (x + 1)

maybeAddition' :: Maybe' Int -> Maybe' Int
maybeAddition' Nothing' = Nothing'
maybeAddition' (Just' x) = fmap (+1) (Just' x)

-- >>> maybeAddition' (Just 5)
-- Just 6

-- >>> maybeAddition' Nothing
-- Nothing

-- 1.3

listAddition :: [Int] -> [Int]
listAddition [ ] = [ ]
listAddition (x : xs) = (x + 1) : listAddition xs

listAddition' :: [Int] -> [Int]
listAddition' [] = []
listAddition' (x:xs) = fmap (+1) [x] ++ listAddition' xs

-- 1.4

functorAddition :: Functor f => f Int -> f Int
functorAddition xs = fmap (+1) xs

-- 1.5a

qux :: Int -> Int -> Maybe Int
qux _ 0 = Nothing
qux x y = fmap (*2) (Just (x `div` y))

-- >>> qux 10 5
-- Just 4

-- >>> qux 10 0
-- Nothing

-- 1.5b

quxList :: Int -> [Int] -> [Maybe Int]
quxList _ [] = []
quxList a (x:xs) = qux a x : quxList a xs

-- >>> quxList 10 [0, 1, 5, 0, 2]
-- [Nothing,Just 20,Just 4,Nothing,Just 10]

-- 1.5c

quxList' :: Int -> [Int] -> [Maybe Int]
quxList' _ [] = []
quxList' a (x:xs) = case x of
    0 -> Nothing : quxList' a xs
    n -> (fmap (*2) (Just (a `div` n))) : quxList' a xs

-- >>> quxList 10 [0, 1, 5, 0, 2]
-- [Nothing,Just 20,Just 4,Nothing,Just 10]

-- 1.6

renameExprs :: (String -> String) -> [Expr String] -> [Expr String]
renameExprs f [] = []
renameExprs f (x:xs) = case x of
    Var c -> fmap f (Var c) : renameExprs f xs
    Add x y -> Add (fmap f x) (fmap f y) : renameExprs f xs


    

-- >>> renameExprs (fmap toUpper) [Var "x", Add (Var "x") (Var "y")]
-- [Var "X",Add (Var "X") (Var "Y")]

-- 2.1

addThenDouble :: Functor f => f Int -> f Int
addThenDouble = fmap (*2) . fmap (+1)

addThenDouble' :: Functor f => f Int -> f Int
addThenDouble' a = fmap ((*2) . (+1)) a

-- >>> addThenDouble (Just 1)
-- Just 4

-- >>> addThenDouble' (Just 1)
-- Just 4

-- 2.2a

unzip :: Functor f => f (a,b) -> (f a, f b)
unzip xs = (fmap (\(a, _) -> a) xs, fmap (\(_, a) -> a) xs)

-- 2.2b

(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f


myFunc :: Functor h => (a -> b) -> (b -> c) -> h a -> h c
myFunc f g = fmap (f .> g)

-- 3.1

newtype Parser a = MkParser (String -> [(a, String)])
parse :: Parser a -> (String -> [(a, String)])
parse (MkParser p) = p
instance Functor Parser where
fmap' :: (a -> b) -> Parser a -> Parser b
fmap' f (MkParser p) = MkParser (\ts -> map (\(x, ts') -> (f x, ts')) (p ts))
infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
(MkParser px) <||> (MkParser py) = MkParser (\ts -> px ts ++ py ts)

main = do print "hi"
